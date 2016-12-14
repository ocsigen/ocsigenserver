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

open Lwt
open Ocsigen_messages
open Ocsigen_socket
open Ocsigen_lib
open Ocsigen_extensions
open Ocsigen_http_frame
open Ocsigen_headers
open Ocsigen_config
open Ocsigen_parseconfig
open Ocsigen_cookies
open Lazy

let () = Random.self_init ()

let () = Ocsigen_commandline.cmdline
(* This is only to have the module Ocsigen_commandline linked
   when we do not use -linkall *)

(* Without the following line, it stops with "Broken Pipe" without raising
   an exception ... *)
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
       Unix.string_of_inet_addr (ip_of_sockaddr sockaddr)) sockaddr s
let dbg sockaddr s =
  Lwt_log.ign_info_f ~section "While talking to %a:%s"
    (fun () sockaddr ->
       Unix.string_of_inet_addr (ip_of_sockaddr sockaddr)) sockaddr s


let http_url_syntax = Hashtbl.find Neturl.common_url_syntax "http"

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
  | Simplexmlparser.Xml_parser_error s ->
    (("Fatal - Error in configuration file: "^s),
     51)
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

    parse_server true s;

    Ocsigen_extensions.end_initialisation ();
  with e ->
    Ocsigen_extensions.end_initialisation ();
    Lwt_log.ign_error ~section (fst (errmsg e))

(* reloading the config file *)
let reload ?file () =

  (* That function cannot be interrupted??? *)
  Lwt_log.ign_warning ~section "Reloading config file" ;
  (try
     match parse_config ?file () with
     | [] -> ()
     | s::_ -> reload_conf s
   with e -> errlog (fst (errmsg e)));

  (try
     match parse_config ?file () with
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
    | ["shutdown"] -> Ocsigen_cohttp_server.shutdown_server None; Lwt.return ()
    | ["shutdown"; f] ->
      Ocsigen_cohttp_server.shutdown_server (Some (float_of_string f));
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

let start_server () = try

    (* initialization functions for modules (Ocsigen extensions or application
       code) loaded from now on will be executed directly. *)
    Ocsigen_loader.set_init_on_load true;

    let config_servers = parse_config () in

    let number_of_servers = List.length config_servers in

    if number_of_servers > 1
    then Lwt_log.ign_warning ~section "Multiple servers not supported anymore";

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

    let run (user, group) (ssl, ports, sslports) (minthreads, maxthreads) s =

      Ocsigen_messages.open_files ~user ~group () >>= fun () ->

      (*let wait_end_init, wait_end_init_awakener = wait () in *)
      (* Listening on all ports: *)
      (*
         sockets := List.fold_left
           (fun a i -> (listen false i wait_end_init extensions_connector)@a) [] ports;
         sslsockets := List.fold_left
           (fun a i -> (listen true i wait_end_init extensions_connector)@a) [] sslports;
      *)

      let connection = match ports with
        | [] -> [(Ocsigen_socket.All, 80)]
        | l -> l
      in

      let ssl_connection =
        let ssl = match ssl with
          | None
          | Some {
              Ocsigen_parseconfig.ssl_certificate = None ;
              Ocsigen_parseconfig.ssl_privatekey = None
            } ->
            None
          | Some {
              Ocsigen_parseconfig.ssl_certificate = Some crt ;
              Ocsigen_parseconfig.ssl_privatekey = Some key
          } ->
            Some (crt, key)
          | Some { Ocsigen_parseconfig.ssl_privatekey = None } ->
            raise (Ocsigen_config.Config_file_error "SSL key is missing")
          | Some { Ocsigen_parseconfig.ssl_certificate = None } ->
            raise (Ocsigen_config.Config_file_error "SSL certificate is missing")
        in match sslports, ssl with
        | [], Some (crt, key) -> [(Ocsigen_socket.All, 443, (crt, key))]
        | l, Some (crt, key) ->
          List.map (fun (a, p) -> (a, p, (crt, key))) l
        | _ -> []
      in

      begin match ports with
        | (_, p)::_ -> Ocsigen_config.set_default_port p
        | _ -> ()
      end;
      begin match sslports with
        | (_, p)::_ -> Ocsigen_config.set_default_sslport p
        | _ -> ()
      end;

      let current_uid = Unix.getuid () in

      let gid = match group with
        | None -> Unix.getgid ()
        | Some group ->
          try (Unix.getgrnam group).Unix.gr_gid
          with Not_found as e ->
            errlog ("Error: Wrong group");
            raise e
      in

      let uid = match user with
        | None -> current_uid
        | Some user ->
          try (Unix.getpwnam user).Unix.pw_uid
          with Not_found as e ->
            errlog ("Error: Wrong user");
            raise e
      in

      (* A pipe to communicate with the server *)
      let commandpipe = get_command_pipe () in
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

      Ocsigen_config.set_user user;
      Ocsigen_config.set_group group;

      (* Je suis fou :
         let rec f () =
           print_endline "-";
           Lwt_unix.yield () >>= f
           in f (); *)

      if maxthreads < minthreads
      then
        raise
          (Config_file_error "maxthreads should be greater than minthreads");

      ignore (Ocsigen_config.init_preempt
                minthreads
                maxthreads
                (fun s -> Lwt_log.ign_error ~section s));

      (* Now I can load the modules *)
      Dynlink_wrapper.init ();
      Dynlink_wrapper.allow_unsafe_modules true;

      Ocsigen_extensions.start_initialisation ();

      parse_server false s;

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

      let pipe = Lwt_chan.in_channel_of_descr
          (Lwt_unix.of_unix_file_descr
             (Unix.openfile commandpipe
                [Unix.O_RDWR; Unix.O_NONBLOCK; Unix.O_APPEND] 0o660)) in

      let rec f () =
        Lwt_chan.input_line pipe >>= fun s ->
        Ocsigen_messages.warning ("Command received: "^s);
        (Lwt.catch
           (fun () ->
              let prefix, c =
                match String.split ~multisep:true ' ' s with
                | [] -> raise Ocsigen_command.Unknown_command
                | a::l ->
                  try
                    let aa, ab = String.sep ':' a in
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

      (* Lwt.wakeup wait_end_init_awakener (); *)
       (*
       let config = {
         Server.callback =
           Ocsigen_cohttp_server.service_cohttp
             ~address
             ~port
             ~extensions_connector;
         Server.conn_closed = (fun _ _ () -> ())
       } in

       let process = [ Server.create ~address ~port config ] in
       let process = match ssl with
         | Some (address, port) ->
           let config = {
             Server.callback =
               Ocsigen_brouette.service_cohttp
                 ~address
                 ~port
                 ~extensions_connector;
             Server.conn_closed = (fun _ _ () -> ())

           } in Server.create ~address ~port config :: process
         | None -> process
       in Lwt.join process
       *)

      Lwt.join
        ((List.map (fun (address, port) -> Ocsigen_cohttp_server.service
                       ~address
                       ~port
                       ~connector:extensions_connector ()) connection)
         @
         (List.map (fun (address, port, (crt, key)) -> Ocsigen_cohttp_server.service
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

    let rec launch = function
      | [] -> ()
      | [h] ->
        let user_info, sslinfo, threadinfo = extract_info h in
        (* set_passwd_if_needed sslinfo; *)
        if (get_daemon ())
        then
          let pid = Unix.fork () in
          if pid = 0
          then
            Lwt_main.run (run user_info sslinfo threadinfo h)
          else begin
            Ocsigen_messages.console
              (fun () -> "Process "^(string_of_int pid)^" detached");
            write_pid pid
          end
        else begin
          write_pid (Unix.getpid ());
          Lwt_main.run (run user_info sslinfo threadinfo h)
        end
      | _ -> () (* Multiple servers not supported any more *)

    in
    launch config_servers

  with e ->
    let msg, errno = errmsg e in
    errlog msg; exit errno
