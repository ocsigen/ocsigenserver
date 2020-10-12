(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2005
 * Vincent Balat, Denis Berthod, Nataliya Guts, Jérôme Vouillon
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*)

open Lwt.Infix

let () = Random.self_init ()

(* Without the following line, it stops with "Broken Pipe" without
   raising an exception ... *)
let _ = Sys.set_signal Sys.sigpipe Sys.Signal_ignore

(* Exit gracefully on SIGINT so that profiling will work *)
let _ = Sys.set_signal Sys.sigint (Sys.Signal_handle(fun _ -> exit 0))

let section = Lwt_log.Section.make "ocsigen:main"

(* Initialize exception handler for Lwt timeouts: *)
let _ =
  Lwt_timeout.set_exn_handler
    (fun e -> Lwt_log.ign_error ~section ~exn:e "Uncaught Exception after lwt \
                                                 timeout")

let _warn sockaddr s =
  Lwt_log.ign_warning_f ~section "While talking to %a:%s"
    (fun () sockaddr ->
       Unix.string_of_inet_addr
         (Ocsigen_lib.Ip_address.of_sockaddr sockaddr))
    sockaddr s

let _dbg sockaddr s =
  Lwt_log.ign_info_f ~section "While talking to %a:%s"
    (fun () sockaddr ->
       Unix.string_of_inet_addr
         (Ocsigen_lib.Ip_address.of_sockaddr sockaddr))
    sockaddr s

(* fatal errors messages *)
let errmsg = function
  | Dynlink_wrapper.Error e ->
    (("Fatal - Dynamic linking error: "^(Dynlink_wrapper.error_message e)),
     6)
  | (Unix.Unix_error _) as e ->
    (("Fatal - "^(Printexc.to_string e)),
     9)
  | Ssl.Private_key_error msg ->
    (("Fatal - bad password: " ^ msg),
     10)
  | Ocsigen_config.Config_file_error msg
  | Ocsigen_extensions.Error_in_config_file msg ->
    (("Fatal - Error in configuration file: "^msg),
     50)
  | Xml.Error (s, loc) ->
    let begin_char, end_char = Xml.range loc and line = Xml.line loc in
    Printf.sprintf
      "Fatal - Error in configuration file, line %d, characters %d-%d: %s"
      line begin_char end_char
      (Xml.error_msg s),
    51
  | Ocsigen_loader.Dynlink_error (s, (Dynlink.Error err)) ->
    (("Fatal - While loading "^s^": "^(Dynlink.error_message err)),
     52)
  | Ocsigen_loader.Dynlink_error (s, exn) ->
    (("Fatal - While loading "^s^": "^(Printexc.to_string exn)),
     52)
  | Ocsigen_loader.Findlib_error _ as e ->
    (("Fatal - " ^ Printexc.to_string e), 53)
  | exn ->
    try
      ((Ocsigen_extensions.get_init_exn_handler () exn),
       20)
    with
      exn ->
      (("Fatal - Uncaught exception: "^Printexc.to_string exn),
       100)

(* loading new configuration *)
let reload_conf s =
  try
    Ocsigen_extensions.start_initialisation ();

    Ocsigen_parseconfig.later_pass s;

    Ocsigen_extensions.end_initialisation ();
  with e ->
    Ocsigen_extensions.end_initialisation ();
    Lwt_log.ign_error ~section (fst (errmsg e))

(* reloading the config file *)
let reload ?file () =

  (* That function cannot be interrupted??? *)
  Lwt_log.ign_warning ~section "Reloading config file" ;
  (try
     match Ocsigen_parseconfig.parse_config ?file () with
     | [] -> ()
     | s::_ -> reload_conf s
   with e -> Ocsigen_messages.errlog (fst (errmsg e)));

  (try
     match Ocsigen_parseconfig.parse_config ?file () with
     | [] -> ()
     | s::_ -> reload_conf s
   with e -> Lwt_log.ign_error ~section (fst (errmsg e)));

  Lwt_log.ign_warning ~section "Config file reloaded"

let _ =
  let f _s = function
    | ["reopen_logs"] ->
      Ocsigen_messages.open_files () >>= fun () ->
      Lwt_log.ign_warning ~section "Log files reopened";
      Lwt.return ()
    | ["reload"] -> reload (); Lwt.return ()
    | ["reload"; file] -> reload ~file (); Lwt.return ()
    | ["shutdown"] ->
      Ocsigen_cohttp.shutdown None;
      Lwt.return ()
    | ["shutdown"; f] ->
      Ocsigen_cohttp.shutdown (Some (float_of_string f));
      Lwt.return ()
    | ["gc"] ->
      Gc.compact ();
      Lwt_log.ign_warning ~section "Heap compaction requested by user";
      Lwt.return ()
    | ["clearcache"] -> Ocsigen_cache.clear_all_caches ();
      Lwt.return ()
    | _ -> Lwt.fail Ocsigen_command.Unknown_command
  in
  Ocsigen_command.register_command_function f

type accessor =
  { accessor : 'a . (('a -> 'a) option * 'a Hmap.key) -> 'a option }

module type Hmap_wrapped = sig
  type t
  val get : t -> Hmap.t
  val do_ : t -> (Hmap.t -> Hmap.t) -> unit
end

module type Config_nested = sig

  type t

  type 'a key

  val key : ?preprocess:('a -> 'a) -> unit -> 'a key

  val find : t -> 'a key -> 'a option

  val set : t -> 'a key -> 'a -> unit

  val unset : t -> 'a key -> unit

  type accessor =
    { accessor : 'a . 'a key -> 'a option }

end

module Make_config_nested (W : Hmap_wrapped) = struct

  type 'a key = ('a -> 'a) option * 'a Hmap.key

  type nonrec accessor
    = accessor
    = { accessor : 'a . 'a key -> 'a option }

  let key ?preprocess () = preprocess, Hmap.Key.create ()

  let find w (_, k) = Hmap.find k (W.get w)

  let set w (f, k) v =
    let v =
      match f with
      | Some f ->
        f v
      | None ->
        v
    in
    W.do_ w (Hmap.add k v)

  let unset w (_, k) = W.do_ w (Hmap.rem k)

end

module Site = struct

  type extension_simple = accessor -> Ocsigen_extensions.extension

  type extension =
    [ `Simple of extension_simple
    | `Intrusive of
        Ocsigen_extensions.virtual_hosts ->
        Ocsigen_extensions.config_info ->
        Ocsigen_lib.Url.path ->
        extension_simple ]

  let registered_extensions = ref []

  let create_extension f =
    let v = `Simple f in
    registered_extensions := v :: !registered_extensions; v

  let create_extension_intrusive f =
    let v = `Intrusive f in
    registered_extensions := v :: !registered_extensions; v

  type t = {
    s_id :
      [ `Host of Ocsigen_extensions.virtual_hosts
      | `Attach of (t * Ocsigen_lib.Url.path) ];
    s_config_info : Ocsigen_extensions.config_info ;
    s_charset : Ocsigen_charset_mime.charset option ;
    mutable s_config_map : Hmap.t ;
    mutable s_children_l :
      [ `Extension of extension_simple | `Child of t ] list ;
  }

  let l = ref []

  let default_re_string = ".*"

  let default_re = Ocsigen_lib.Netstring_pcre.regexp default_re_string

  let rec path_and_hosts {s_id; _} =
    match s_id with
    | `Host hosts ->
      [], hosts
    | `Attach (s, path') ->
      let path, hosts = path_and_hosts s in
      path @ path', hosts

  let register ({ s_config_info ; s_children_l ; _ } as s) =
    function
    | `Simple f ->
      s.s_children_l <- `Extension f :: s_children_l
    | `Intrusive f ->
      let path, hosts = path_and_hosts s in
      s.s_children_l <-
        `Extension (f hosts s_config_info path) :: s_children_l

  let create
    ?(config_info = Ocsigen_extensions.default_config_info ())
    ?(id = `Host (default_re_string, None))
    ?charset
    ?(auto_load_extensions = false)
    () =
    let s_id =
      match id with
      | `Host (host_regexp, port) when host_regexp = default_re_string ->
        `Host
          [default_re_string, default_re, port]
      | `Host (host_regexp, port) ->
        `Host
          [host_regexp, Ocsigen_lib.Netstring_pcre.regexp host_regexp, port]
      | `Attach (parent, path) ->
        `Attach (parent, Ocsigen_extensions.preprocess_site_path path)
    in
    let s = {
      s_id ;
      s_charset = charset ;
      s_config_info = config_info ;
      s_config_map = Hmap.empty ;
      s_children_l = []
    } in
    (match s_id with
     | `Host _ ->
       l := s :: !l;
     | `Attach (parent, _) ->
       parent.s_children_l <- `Child s :: parent.s_children_l);
    if auto_load_extensions then
      List.iter (register s) (List.rev !registered_extensions);
    s

  let rec dump_host
      path
      { s_config_map ; s_children_l ; _} =
    let f = function
      | `Extension f ->
        f {accessor = fun (_, k) -> Hmap.find k s_config_map}
      | `Child ({s_charset ; s_id = `Attach (_, path');_} as s) ->
        let path = path @ path' in
        Ocsigen_extensions.site_ext (dump_host path s) s_charset path
      | `Child _ ->
        failwith "Ocsigen_server.dump_host"
    in
    Ocsigen_extensions.compose (List.map f s_children_l)

  let dump () =
    let f acc = function
      | { s_config_info ; s_id = `Host l ; s_children_l = _ :: _ ;_} as s ->
        (l, s_config_info, dump_host [] s) :: acc
      | _ ->
        acc
    in
    Ocsigen_extensions.set_hosts (List.fold_left f [] !l)

  module Config =
    Make_config_nested (struct
      type nonrec t = t
      let get {s_config_map;_} = s_config_map
      let do_ ({s_config_map;_} as vh) f =
        vh.s_config_map <- f s_config_map
    end)

end

let start ?config () =

  try

    (* initialization functions for modules (Ocsigen extensions or application
       code) loaded from now on will be executed directly. *)
    Ocsigen_loader.set_init_on_load true;

    (match config with
     | Some (_ :: _ :: _) ->
       Lwt_log.ign_warning ~section "Multiple servers not supported anymore"
     | _ ->
       ());

    let ask_for_passwd sslports _ =
      print_string "Please enter the password for the HTTPS server listening \
                    on port(s) ";
      print_string (String.concat ", " (List.map (fun (_,p) -> string_of_int p)
                                          sslports));
      print_string ": ";
      let old_term= Unix.tcgetattr Unix.stdin in
      let old_echo = old_term.Unix.c_echo in
      old_term.Unix.c_echo <- false;
      Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH old_term;
      try
        let r = read_line () in
        print_newline ();
        old_term.Unix.c_echo <- old_echo;
        Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH old_term;
        r
      with exn ->
        old_term.Unix.c_echo <- old_echo;
        Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH old_term;
        raise exn
    in

    let extensions_connector = Ocsigen_extensions.compute_result in

    let run s =

      let user = Ocsigen_config.get_user ()
      and group = Ocsigen_config.get_group () in

      Lwt_main.run (Ocsigen_messages.open_files ~user ~group ());

      let ports = Ocsigen_config.get_ports ()
      and ssl_ports = Ocsigen_config.get_ssl_ports () in

      let connection =
        match ports with
        | [] -> [`All, 80]
        | l -> l
      in

      let ssl_connection =
        let ssl =
          match Ocsigen_config.get_ssl_info () with
          | None
          | Some {
              Ocsigen_config.ssl_certificate = None ;
              Ocsigen_config.ssl_privatekey = None ;
              _
            } ->
            None
          | Some {
              Ocsigen_config.ssl_certificate = Some crt ;
              Ocsigen_config.ssl_privatekey = Some key ;
              _
          } ->
            Some (crt, key)
          | Some { Ocsigen_config.ssl_privatekey = None ;_ } ->
            raise (Ocsigen_config.Config_file_error "SSL key is missing")
          | Some { Ocsigen_config.ssl_certificate = None ; _} ->
            raise (Ocsigen_config.Config_file_error "SSL certificate is missing")
        in
        match ssl_ports, ssl with
        | [], Some (crt, key) -> [`All, 443, (crt, key)]
        | l, Some (crt, key) ->
          List.map (fun (a, p) -> (a, p, (crt, key))) l
        | _ -> []
      in

      let current_uid = Unix.getuid () in

      let gid = match group with
        | None -> Unix.getgid ()
        | Some group ->
          try (Unix.getgrnam group).Unix.gr_gid
          with Not_found as e ->
            Ocsigen_messages.errlog ("Error: Wrong group");
            raise e
      in

      let uid = match user with
        | None -> current_uid
        | Some user ->
          try (Unix.getpwnam user).Unix.pw_uid
          with Not_found as e ->
            Ocsigen_messages.errlog ("Error: Wrong user");
            raise e
      in

      (* A pipe to communicate with the server *)
      let commandpipe = Ocsigen_config.get_command_pipe () in
      begin
        try
          ignore (Unix.stat commandpipe);
        with Unix.Unix_error _ ->
          try
            let umask = Unix.umask 0 in
            Unix.mkfifo commandpipe 0o660;
            Unix.chown commandpipe uid gid;
            ignore (Unix.umask umask);
            Lwt_log.ign_warning ~section "Command pipe created";
          with e ->
            Lwt_log.ign_error ~section ~exn:e
              "Cannot create the command pipe";
      end ;
      (* I change the user for the process *)
      begin try
          if current_uid = 0 then begin
            match user with
            | None -> ()
            | Some user -> Unix.initgroups user gid
          end;
          Unix.setgid gid;
          Unix.setuid uid;
        with (Unix.Unix_error _ | Failure _) as e ->
          Lwt_log.ign_error ~section "Error: Wrong user or group";
          raise e
      end;

      let minthreads = Ocsigen_config.get_minthreads ()
      and maxthreads = Ocsigen_config.get_maxthreads () in
      if minthreads > maxthreads then
        raise
          (Ocsigen_config.Config_file_error
             "maxthreads should be greater than minthreads");

      ignore
        (Lwt_preemptive.init
           minthreads maxthreads
           (fun s -> Lwt_log.ign_error ~section s));

      Lwt.async_exception_hook := (fun e ->
        (* replace the default "exit 2" behaviour *)
        match e with
        | Unix.Unix_error (Unix.EPIPE, _, _) -> ()
        | _ -> Lwt_log.ign_error ~section ~exn:e "Uncaught Exception"
      );

      (match s with
       | Some s ->
         (* Now I can load the modules *)
         Dynlink_wrapper.allow_unsafe_modules true;
         Ocsigen_extensions.start_initialisation ();
         Ocsigen_parseconfig.later_pass s;
         (* As libraries are reloaded each time the config file is
            read, we do not allow to register extensions in
            libraries. Seems it does not work :-/ *)
         Dynlink_wrapper.prohibit ["Ocsigen_extensions.R"]
       | None ->
         (Ocsigen_extensions.start_initialisation ();
          Site.dump ()));

      if (Ocsigen_config.get_silent ())
      then begin
        (* Close stderr, stdout stdin if silent *)
        (* redirect stdout and stderr to /dev/null *)
        let devnull = Unix.openfile "/dev/null" [Unix.O_WRONLY] 0 in
        Unix.dup2 devnull Unix.stdout;
        Unix.dup2 devnull Unix.stderr;
        Unix.close devnull;
        Unix.close Unix.stdin;
      end;

      (* detach from the terminal *)
      if (Ocsigen_config.get_daemon ())
      then ignore (Unix.setsid ());

      Ocsigen_extensions.end_initialisation ();

      let pipe =
        Unix.(openfile commandpipe [O_RDWR; O_NONBLOCK; O_APPEND]) 0o660
        |> Lwt_unix.of_unix_file_descr
        |> Lwt_io.(of_fd ~mode:input)
      in

      let rec f () =
        Lwt_io.read_line pipe >>= fun s ->
        Ocsigen_messages.warning ("Command received: "^s);
        (Lwt.catch
           (fun () ->
              let prefix, c =
                match Ocsigen_lib.String.split ~multisep:true ' ' s with
                | [] -> raise Ocsigen_command.Unknown_command
                | a::l ->
                  try
                    let aa, ab = Ocsigen_lib.String.sep ':' a in
                    (Some aa, (ab::l))
                  with Not_found -> None, (a::l)
              in
              Ocsigen_command.get_command_function () ?prefix s c)
           (function
             | Ocsigen_command.Unknown_command ->
               Lwt_log.ign_warning ~section "Unknown command";
               Lwt.return ()
             | e ->
               Lwt_log.ign_error ~section ~exn:e "Uncaught Exception after \
                                                  command";
               Lwt.fail e))
        >>= f
      in ignore (f ());

      Lwt_main.run @@ Lwt.join
        ((List.map (fun (address, port) -> Ocsigen_cohttp.service
                       ~address
                       ~port
                       ~connector:extensions_connector ()) connection)
         @
         (List.map (fun (address, port, (crt, key)) -> Ocsigen_cohttp.service
                       ~ssl:(crt, key, Some (ask_for_passwd [(address, port)]))
                       ~address
                       ~port
                       ~connector:extensions_connector ())) ssl_connection)
       (*
         Ocsigen_messages.warning "Ocsigen has been launched (initialisations ok)";

       fst (Lwt.wait ())
       *)

    in

  (*
  let set_passwd_if_needed (ssl, ports, sslports) =
    if sslports <> []
    then
      match ssl with
      | None
      | Some (None, None) -> ()
      | Some (None, _) -> raise (Ocsigen_config.Config_file_error
                                   "SSL certificate is missing")
      | Some (_, None) -> raise (Ocsigen_config.Config_file_error
                                   "SSL key is missing")
      | Some ((Some c), (Some k)) ->
        Ssl.set_password_callback !Server.ssl_context (ask_for_passwd sslports);
        Ssl.use_certificate !Server.ssl_context c k
  in
  *)

    let write_pid pid =
      match Ocsigen_config.get_pidfile () with
        None -> ()
      | Some p ->
        let spid = (string_of_int pid)^"\n" in
        let len = String.length spid in
        let f =
          Unix.openfile
            p
            [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o640 in
        ignore (Unix.write_substring f spid 0 len);
        Unix.close f
    in

    let launch h =
      Ocsigen_lib.Option.iter Ocsigen_parseconfig.first_pass h;
      (* set_passwd_if_needed sslinfo; *)
      if Ocsigen_config.get_daemon () then
        let pid = Unix.fork () in
        if pid = 0 then
          run h
        else begin
          Ocsigen_messages.console
            (fun () -> "Process " ^ string_of_int pid ^ " detached");
          write_pid pid
        end
      else begin
        write_pid (Unix.getpid ());
        run h
      end
    in

    let launch = function
      | Some [] -> ()
      | Some [h] ->
        launch (Some h)
      | None ->
        launch None
      | Some (_ :: _ :: _) ->
        () (* Multiple servers not supported any more *)

    in
    launch config

  with e ->
    let msg, errno = errmsg e in
    Ocsigen_messages.errlog msg; exit errno
