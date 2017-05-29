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

let section = Lwt_log.Section.make "ocsigen:main"

(* Initialize exception handler for Lwt timeouts: *)
let _ =
  Lwt_timeout.set_exn_handler
    (fun e -> Lwt_log.ign_error ~section ~exn:e "Uncaught Exception after lwt \
                                                 timeout")

let warn sockaddr s =
  Lwt_log.ign_warning_f ~section "While talking to %a:%s"
    (fun () sockaddr ->
       Unix.string_of_inet_addr
         (Ocsigen_lib.Ip_address.of_sockaddr sockaddr))
    sockaddr s

let dbg sockaddr s =
  Lwt_log.ign_info_f ~section "While talking to %a:%s"
    (fun () sockaddr ->
       Unix.string_of_inet_addr
         (Ocsigen_lib.Ip_address.of_sockaddr sockaddr))
    sockaddr s

let try_bind' f g h = Lwt.try_bind f h g

(* fatal errors messages *)
let errmsg = function
  | Dynlink_wrapper.Error e ->
    (("Fatal - Dynamic linking error: "^(Dynlink_wrapper.error_message e)),
     6)
  | (Unix.Unix_error _) as e ->
    (("Fatal - "^(Printexc.to_string e)),
     9)
  | Ssl.Private_key_error ->
    (("Fatal - bad password"),
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
  let f s = function
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
  { accessor : 'a . 'a Hmap.key -> 'a option }

let compose_with_config m l =
  Ocsigen_extensions.compose
    (List.map (fun f -> f {accessor = fun k -> Hmap.find k m}) l)

module type Hmap_wrapped = sig
  type t
  val get : t -> Hmap.t
  val do_ : t -> (Hmap.t -> Hmap.t) -> unit
end

module type Config_nested = sig

  type parent_t

  type 'a key

  val key : unit -> 'a key

  val find : parent_t -> 'a key -> 'a option

  val set : parent_t -> 'a key -> 'a -> unit

  val unset : parent_t -> 'a key -> unit

  type accessor = { accessor : 'a . 'a key -> 'a option }

end

module Make_config_nested (W : Hmap_wrapped) = struct

  type nonrec accessor
    = accessor
    = { accessor : 'a . 'a Hmap.key -> 'a option }

  type 'a key = 'a Hmap.key

  let key () = Hmap.Key.create ()

  let find w k = Hmap.find k (W.get w)

  let set w k v = W.do_ w (Hmap.add k v)

  let unset w k = W.do_ w (Hmap.rem k)

end

module Vhost = struct

  type 'a config_key = 'a Hmap.key

  type t = {
    vh_list : Ocsigen_extensions.virtual_hosts ;
    vh_config_info : Ocsigen_extensions.config_info ;
    mutable vh_config_map : Hmap.t ;
    mutable vh_fun_l : (accessor -> Ocsigen_extensions.extension) list ;
  }

  let l = ref []

  let default_re_string = ".*"

  let default_re = Ocsigen_lib.Netstring_pcre.regexp default_re_string

  let create
    ?(config_info = Ocsigen_extensions.default_config_info ())
    ?host_regexp
    ?port () =
    let vh_list =
      match host_regexp with
      | Some host_regexp when host_regexp = default_re_string ->
        [default_re_string, default_re, port]
      | None ->
        [default_re_string, default_re, port]
      | Some host_regexp ->
        [host_regexp, Ocsigen_lib.Netstring_pcre.regexp host_regexp, port]
    in
    let vh = {
      vh_list ;
      vh_config_info = config_info ;
      vh_config_map = Hmap.empty ;
      vh_fun_l = []
    } in
    l := vh :: !l;
    vh

  let dump () =
    let f { vh_list ; vh_config_info ; vh_config_map ; vh_fun_l } =
      vh_list,
      vh_config_info,
      compose_with_config vh_config_map vh_fun_l
    and l =
      List.filter
        (function {vh_fun_l = _ :: _} -> true | _ -> false)
        (List.rev !l)
    in
    Ocsigen_extensions.set_hosts (List.map f l)

  module Config =
    Make_config_nested (struct
      type nonrec t = t
      let get {vh_config_map} = vh_config_map
      let do_ ({vh_config_map} as vh) f =
        vh.vh_config_map <- f vh_config_map
    end)

  let register ({vh_fun_l} as vh) f = vh.vh_fun_l <- f :: vh_fun_l

end

module Site = struct

  type t = {
    s_dir : string list ;
    s_charset : Ocsigen_charset_mime.charset option ;
    mutable s_config_map : Hmap.t ;
    mutable s_fun_l : (accessor -> Ocsigen_extensions.extension) list ;
  }

  let create s_dir s_charset = {
    s_dir = Ocsigen_extensions.preprocess_site_path s_dir ;
    s_charset ;
    s_config_map = Hmap.empty ;
    s_fun_l = []
  }

  module Config =
    Make_config_nested (struct
      type nonrec t = t
      let get {s_config_map} = s_config_map
      let do_ ({s_config_map} as s) f =
        s.s_config_map <- f s_config_map
    end)

  let register ({s_fun_l} as s) f = s.s_fun_l <- f :: s_fun_l

  let to_extension
      ~parent_path
      {s_dir ; s_charset ; s_fun_l ; s_config_map} =
    let ext_of_children = compose_with_config s_config_map s_fun_l in
    Ocsigen_extensions.site_ext
      ext_of_children s_charset
      (parent_path @ s_dir)

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

      Ocsigen_messages.open_files ~user ~group () >>= fun () ->

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
              Ocsigen_config.ssl_privatekey = None
            } ->
            None
          | Some {
              Ocsigen_config.ssl_certificate = Some crt ;
              Ocsigen_config.ssl_privatekey = Some key
          } ->
            Some (crt, key)
          | Some { Ocsigen_config.ssl_privatekey = None } ->
            raise (Ocsigen_config.Config_file_error "SSL key is missing")
          | Some { Ocsigen_config.ssl_certificate = None } ->
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
        (Ocsigen_config.init_preempt
           minthreads maxthreads
           (fun s -> Lwt_log.ign_error ~section s));

      (* Now I can load the modules *)
      Dynlink_wrapper.init ();
      Dynlink_wrapper.allow_unsafe_modules true;

      Ocsigen_extensions.start_initialisation ();

      (match s with
       | Some s ->
         Ocsigen_parseconfig.later_pass s
       | None ->
         Vhost.dump ());

      Dynlink_wrapper.prohibit ["Ocsigen_extensions.R"];
      (* As libraries are reloaded each time the config file is read,
         we do not allow to register extensions in libraries *)
      (* seems it does not work :-/ *)
      (* Closing stderr, stdout stdin if silent *)

      if (Ocsigen_config.get_silent ())
      then begin
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

      Lwt.async_exception_hook := (fun e ->
        (* replace the default "exit 2" behaviour *)
        Lwt_log.ign_error ~section ~exn:e "Uncaught Exception"
      );

      Lwt.join
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
        ignore (Unix.write f spid 0 len);
        Unix.close f
    in

    let launch h =
      Ocsigen_lib.Option.iter Ocsigen_parseconfig.first_pass h;
      (* set_passwd_if_needed sslinfo; *)
      if Ocsigen_config.get_daemon () then
        let pid = Unix.fork () in
        if pid = 0 then
          Lwt_main.run (run h)
        else begin
          Ocsigen_messages.console
            (fun () -> "Process " ^ string_of_int pid ^ " detached");
          write_pid pid
        end
      else begin
        write_pid (Unix.getpid ());
        Lwt_main.run (run h)
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
