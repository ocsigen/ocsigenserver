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
let () = Sys.set_signal Sys.sigpipe Sys.Signal_ignore

(* Exit gracefully on SIGINT so that profiling will work *)
let () = Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> exit 0))
let section = Logs.Src.create "ocsigen:main"

(* Initialize exception handler for Lwt timeouts: *)
let () =
  Lwt_timeout.set_exn_handler (fun e ->
    Logs.err ~src:section (fun fmt ->
      fmt
        ("Uncaught Exception after lwt timeout" ^^ "@\n%s")
        (Printexc.to_string e)))

(* fatal errors messages *)
let errmsg = function
  | Ocsigen_base.Dynlink_wrapper.Error e ->
      "Fatal - Dynamic linking error: " ^ Ocsigen_base.Dynlink_wrapper.error_message e, 6
  | Unix.Unix_error _ as e -> "Fatal - " ^ Printexc.to_string e, 9
  | Ssl.Private_key_error msg -> "Fatal - bad password: " ^ msg, 10
  | Config.Config_file_error msg
  | Extensions.Error_in_config_file msg ->
      "Fatal - Error in configuration file: " ^ msg, 50
  | Xml.Error (s, loc) ->
      let begin_char, end_char = Xml.range loc and line = Xml.line loc in
      ( Printf.sprintf
          "Fatal - Error in configuration file, line %d, characters %d-%d: %s"
          line begin_char end_char (Xml.error_msg s)
      , 51 )
  | Ocsigen_base.Loader.Dynlink_error (s, Dynlink.Error err) ->
      "Fatal - While loading " ^ s ^ ": " ^ Dynlink.error_message err, 52
  | Ocsigen_base.Loader.Dynlink_error (s, exn) ->
      "Fatal - While loading " ^ s ^ ": " ^ Printexc.to_string exn, 52
  | Ocsigen_base.Loader.Findlib_error _ as e -> "Fatal - " ^ Printexc.to_string e, 53
  | exn -> (
    try Extensions.get_init_exn_handler () exn, 20
    with exn -> "Fatal - Uncaught exception: " ^ Printexc.to_string exn, 100)

(* loading new configuration *)
let reload_conf s =
  try
    Extensions.start_initialisation ();
    Parseconfig.later_pass s;
    Extensions.end_initialisation ()
  with e ->
    Extensions.end_initialisation ();
    Logs.err ~src:section (fun fmt -> fmt "%s" (fst (errmsg e)))

(* reloading the config file *)
let reload ?file () =
  (* That function cannot be interrupted??? *)
  Logs.warn ~src:section (fun fmt -> fmt "Reloading config file");
  (try
     match Parseconfig.parse_config ?file () with
     | [] -> ()
     | s :: _ -> reload_conf s
   with e -> Messages.errlog (fst (errmsg e)));
  (try
     match Parseconfig.parse_config ?file () with
     | [] -> ()
     | s :: _ -> reload_conf s
   with e -> Logs.err ~src:section (fun fmt -> fmt "%s" (fst (errmsg e))));
  Logs.warn ~src:section (fun fmt -> fmt "Config file reloaded")

let () =
  let f _s = function
    | ["reopen_logs"] ->
        Messages.open_files () >>= fun () ->
        Logs.warn ~src:section (fun fmt -> fmt "Log files reopened");
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
        Logs.warn ~src:section (fun fmt ->
          fmt "Heap compaction requested by user");
        Lwt.return ()
    | ["clearcache"] ->
        Ocsigen_base.Cache.clear_all_caches ();
        Lwt.return ()
    | _ -> Lwt.fail Command.Unknown_command
  in
  Command.register_command_function f

type instruction =
  Extensions.virtual_hosts
  -> Extensions.config_info
  -> Ocsigen_base.Lib.Url.path
  -> Extensions.extension

let default_re_string = ".*"

let host
      ?(regexp = default_re_string)
      ?port
      ?default_hostname
      ?default_httpport
      ?default_httpsport
      ?default_protocol_is_https
      ?mime_assoc
      ?charset_assoc
      ?default_directory_index
      ?list_directory_content
      ?follow_symlinks
      ?do_not_serve_404
      ?do_not_serve_403
      ?uploaddir
      ?maxuploadfilesize
      instructions
  =
  let def = Extensions.default_config_info () in
  let default default o = Option.value o ~default in
  let config_info =
    { Extensions.default_hostname =
        default def.default_hostname default_hostname
    ; default_httpport = default def.default_httpport default_httpport
    ; default_httpsport = default def.default_httpsport default_httpsport
    ; default_protocol_is_https =
        default def.default_protocol_is_https default_protocol_is_https
    ; mime_assoc = default def.mime_assoc mime_assoc
    ; charset_assoc = default def.charset_assoc charset_assoc
    ; default_directory_index =
        default def.default_directory_index default_directory_index
    ; list_directory_content =
        default def.list_directory_content list_directory_content
    ; follow_symlinks = default def.follow_symlinks follow_symlinks
    ; do_not_serve_404 = default def.do_not_serve_404 do_not_serve_404
    ; do_not_serve_403 = default def.do_not_serve_403 do_not_serve_403
    ; uploaddir = default def.uploaddir uploaddir
    ; maxuploadfilesize = default def.maxuploadfilesize maxuploadfilesize }
  in
  let vh = [regexp, Ocsigen_base.Lib.Netstring_pcre.regexp regexp, port] in
  ( vh
  , config_info
  , Extensions.compose
      (List.map (fun i -> i vh config_info []) instructions) )

let site ?charset path instructions vh config_info parent_path =
  let path = parent_path @ Extensions.preprocess_site_path path in
  let composite =
    Extensions.compose
      (List.map (fun i -> i vh config_info path) instructions)
  in
  Extensions.site_ext composite charset path

let main_loop_is_running = ref false

let main config =
  if !main_loop_is_running
  then Logs.err (fun fmt -> fmt "Cannot run main loop twice");
  main_loop_is_running := true;
  try
    (* initialization functions for modules (Ocsigen extensions or application
       code) loaded from now on will be executed directly. *)
    Ocsigen_base.Loader.set_init_on_load true;
    let ask_for_passwd sslports _ =
      print_string
        "Please enter the password for the HTTPS server listening on port(s) ";
      print_string
        (String.concat ", " (List.map (fun (_, p) -> string_of_int p) sslports));
      print_string ": ";
      let old_term = Unix.tcgetattr Unix.stdin in
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
    let extensions_connector = Extensions.compute_result in
    let run () =
      Messages.open_files () >>= fun () ->
      let ports = Config.get_ports ()
      and ssl_ports = Config.get_ssl_ports () in
      let connection = match ports with [] -> [`All, 80] | l -> l in
      let ssl_connection =
        let ssl =
          match Config.get_ssl_info () with
          | None
          | Some
              { Config.ssl_certificate = None
              ; Config.ssl_privatekey = None
              ; _ } ->
              None
          | Some
              { Config.ssl_certificate = Some crt
              ; Config.ssl_privatekey = Some key
              ; _ } ->
              Some (crt, key)
          | Some {Config.ssl_privatekey = None; _} ->
              raise (Config.Config_file_error "SSL key is missing")
          | Some {Config.ssl_certificate = None; _} ->
              raise
                (Config.Config_file_error "SSL certificate is missing")
        in
        match ssl_ports, ssl with
        | [], Some (crt, key) -> [`All, 443, (crt, key)]
        | l, Some (crt, key) -> List.map (fun (a, p) -> a, p, (crt, key)) l
        | _ -> []
      in
      (* A pipe to communicate with the server *)
      let commandpipe = Config.get_command_pipe () in
      let with_commandpipe =
        try
          ignore (Unix.stat commandpipe : Unix.stats);
          true
        with Unix.Unix_error _ -> (
          try
            let umask = Unix.umask 0 in
            Unix.mkfifo commandpipe 0o660;
            ignore (Unix.umask umask : int);
            Logs.warn ~src:section (fun fmt -> fmt "Command pipe created");
            true
          with e ->
            Logs.warn ~src:section (fun fmt ->
              fmt
                ("Cannot create the command pipe %s. I will continue without."
               ^^ "@\n%s")
                commandpipe (Printexc.to_string e));
            false)
      in
      let minthreads = Config.get_minthreads ()
      and maxthreads = Config.get_maxthreads () in
      if minthreads > maxthreads
      then
        raise
          (Config.Config_file_error
             "maxthreads should be greater than minthreads");
      (Lwt.async_exception_hook :=
         fun e ->
           (* replace the default "exit 2" behaviour *)
           match e with
           | Unix.Unix_error (Unix.EPIPE, _, _) -> ()
           | _ ->
               Logs.err ~src:section (fun fmt ->
                 fmt ("Uncaught Exception" ^^ "@\n%s") (Printexc.to_string e)));
      (* Now apply host configuration: *)
      config ();
      if Config.get_silent ()
      then (
        (* Close stderr, stdout stdin if silent *)
        (* redirect stdout and stderr to /dev/null *)
        let devnull = Unix.openfile "/dev/null" [Unix.O_WRONLY] 0 in
        Unix.dup2 devnull Unix.stdout;
        Unix.dup2 devnull Unix.stderr;
        Unix.close devnull;
        Unix.close Unix.stdin);
      (* detach from the terminal *)
      if Config.get_daemon () then ignore (Unix.setsid () : int);
      Extensions.end_initialisation ();
      (if with_commandpipe
       then
         let pipe =
           Unix.(openfile commandpipe [O_RDWR; O_NONBLOCK; O_APPEND]) 0o660
           |> Lwt_unix.of_unix_file_descr
           |> Lwt_io.(of_fd ~mode:input)
         in
         let rec f () =
           Lwt_io.read_line pipe >>= fun s ->
           Messages.warning ("Command received: " ^ s);
           Lwt.catch
             (fun () ->
                let prefix, c =
                  match Ocsigen_base.Lib.String.split ~multisep:true ' ' s with
                  | [] -> raise Command.Unknown_command
                  | a :: l -> (
                    try
                      let aa, ab = Ocsigen_base.Lib.String.sep ':' a in
                      Some aa, ab :: l
                    with Not_found -> None, a :: l)
                in
                Command.get_command_function () ?prefix s c)
             (function
               | Command.Unknown_command ->
                   Logs.warn ~src:section (fun fmt -> fmt "Unknown command");
                   Lwt.return ()
               | e ->
                   Logs.err ~src:section (fun fmt ->
                     fmt
                       ("Uncaught Exception after command" ^^ "@\n%s")
                       (Printexc.to_string e));
                   Lwt.fail e)
           >>= f
         in
         ignore (f () : 'a Lwt.t));
      Lwt.join
        (List.map
           (fun (address, port) ->
              Ocsigen_cohttp.service ~address ~port
                ~connector:extensions_connector ())
           connection
        @ (List.map (fun (address, port, (crt, key)) ->
             Ocsigen_cohttp.service
               ~ssl:(crt, key, Some (ask_for_passwd [address, port]))
               ~address ~port ~connector:extensions_connector ()))
            ssl_connection)
      (*
         Messages.warning "Ocsigen has been launched (initialisations ok)";

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
      | Some (None, _) -> raise (Config.Config_file_error
                                   "SSL certificate is missing")
      | Some (_, None) -> raise (Config.Config_file_error
                                   "SSL key is missing")
      | Some ((Some c), (Some k)) ->
        Ssl.set_password_callback !Server.ssl_context (ask_for_passwd sslports);
        Ssl.use_certificate !Server.ssl_context c k
  in
    *)
    let write_pid pid =
      match Config.get_pidfile () with
      | None -> ()
      | Some p ->
          let spid = string_of_int pid ^ "\n" in
          let len = String.length spid in
          let f =
            Unix.openfile p [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o640
          in
          ignore (Unix.write_substring f spid 0 len : int);
          Unix.close f
    in
    (* set_passwd_if_needed sslinfo; *)
    if Config.get_daemon ()
    then
      let pid = Unix.fork () in
      if pid = 0
      then Lwt_main.run (run ())
      else (
        Messages.console (fun () ->
          "Process " ^ string_of_int pid ^ " detached");
        write_pid pid)
    else (
      write_pid (Unix.getpid ());
      Lwt_main.run (run ()))
  with e ->
    let msg, errno = errmsg e in
    Messages.errlog msg;
    exit errno

let exec config =
  Config.has_config_file := true;
  match config with
  | [] -> ()
  | [h] ->
      (try Parseconfig.first_pass h
       with e ->
         let msg, errno = errmsg e in
         Messages.errlog msg;
         exit errno);
      main (fun () ->
        (* Now I can load the modules *)
        Ocsigen_base.Dynlink_wrapper.allow_unsafe_modules true;
        Extensions.start_initialisation ();
        Parseconfig.later_pass h;
        (* As libraries are reloaded each time the config file is
          read, we do not allow to register extensions in
          libraries. Seems it does not work :-/ *)
        Ocsigen_base.Dynlink_wrapper.prohibit ["Extensions.R"])
  | _ :: _ :: _ ->
      Logs.warn ~src:section (fun fmt ->
        fmt "Multiple servers not supported anymore")
(* Multiple servers not supported any more *)

let start
      ?(ports = [`All, 8080])
      ?ssl_ports
      ?ssl_info
      ?default_charset
      ?logdir
      ?datadir
      ?uploaddir
      ?maxuploadfilesize
      ?syslog_facility
      ?configfile
      ?usedefaulthostname
      ?pidfile
      ?mimefile
      ?verbose
      ?veryverbose
      ?silent
      ?daemon
      ?debug
      ?debugmode
      ?minthreads
      ?maxthreads
      ?max_number_of_connections
      ?client_timeout
      ?server_timeout
      ?shutdown_timeout
      ?filebuffersize
      ?maxrequestbodysize
      ?maxrequestbodysizeinmemory
      ?bindir
      ?extdir
      ?command_pipe
      ?disablepartialrequests
      ?respect_pipeline
      ?maxretries
      instructions
  =
  Config.set_ports ports;
  Option.iter Config.set_ssl_ports ssl_ports;
  Option.iter Config.set_logdir logdir;
  Option.iter Config.set_syslog_facility syslog_facility;
  Option.iter Config.set_uploaddir uploaddir;
  Option.iter Config.set_maxuploadfilesize maxuploadfilesize;
  Option.iter Config.set_datadir datadir;
  Option.iter Config.set_configfile configfile;
  Option.iter Config.set_pidfile pidfile;
  Option.iter Config.set_mimefile mimefile;
  Option.iter Config.set_verbose verbose;
  Option.iter Config.set_silent silent;
  Option.iter Config.set_daemon daemon;
  Option.iter Config.set_veryverbose veryverbose;
  Option.iter Config.set_debug debug;
  Option.iter Config.set_minthreads minthreads;
  Option.iter Config.set_maxthreads maxthreads;
  Option.iter Config.set_max_number_of_connections
    max_number_of_connections;
  Option.iter Config.set_client_timeout client_timeout;
  Option.iter Config.set_server_timeout server_timeout;
  Option.iter Config.set_filebuffersize filebuffersize;
  Option.iter Config.set_maxrequestbodysize maxrequestbodysize;
  Option.iter Config.set_maxrequestbodysizeinmemory
    maxrequestbodysizeinmemory;
  Option.iter Config.set_default_charset default_charset;
  Option.iter Config.set_bindir bindir;
  Option.iter Config.set_extdir extdir;
  Option.iter Config.set_command_pipe command_pipe;
  Option.iter Config.set_debugmode debugmode;
  Option.iter Config.set_disablepartialrequests disablepartialrequests;
  Option.iter Config.set_usedefaulthostname usedefaulthostname;
  Option.iter Config.set_respect_pipeline respect_pipeline;
  Option.iter Config.set_maxretries maxretries;
  Option.iter Config.set_shutdown_timeout shutdown_timeout;
  Option.iter Config.set_ssl_info ssl_info;
  main (fun () ->
    Extensions.start_initialisation ();
    Extensions.set_hosts instructions)
