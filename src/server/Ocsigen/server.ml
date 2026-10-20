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

(* Silent mode (-s) or daemon mode (-d) requested while the logs go to the
   terminal, which these modes redirect to /dev/null. *)
exception Silent_with_console_logs

(* fatal errors messages *)
let errmsg = function
  | Silent_with_console_logs ->
      ( "Fatal - silent mode (-s) and daemon mode (-d) need log files or syslog, but the logs go to the terminal, which these modes redirect to /dev/null. Set a log directory or syslog (<logdir> or <syslog> in the configuration file, ~logdir or ~syslog_facility for Ocsigen.Server.start), or drop -s and -d."
      , 50 )
  | Dynlink_wrapper.Error e ->
      "Fatal - Dynamic linking error: " ^ Dynlink_wrapper.error_message e, 6
  | Unix.Unix_error _ as e -> "Fatal - " ^ Printexc.to_string e, 9
  | Ssl.Private_key_error msg -> "Fatal - bad password: " ^ msg, 10
  | Config.Config_file_error msg | Extensions.Error_in_config_file msg ->
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
  | Ocsigen_base.Loader.Findlib_error _ as e ->
      "Fatal - " ^ Printexc.to_string e, 53
  | exn -> (
    try Extensions.get_init_exn_handler () exn, 20
    with exn -> "Fatal - Uncaught exception: " ^ Printexc.to_string exn, 100)

(* Until the server configures its logging, its messages go to the terminal,
   unless the program has installed a reporter: without one, Logs would drop
   them, and a program using Ocsigen Server as a library would not see why the
   server stops at start-up. *)
let report_to_terminal_by_default () =
  if Logs.reporter () == Logs.nop_reporter
  then Logs.set_reporter Messages.stdio_reporter

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
    | ["shutdown"; "notimeout"] ->
        (* There is no graceful shutdown yet, which this order would wait for
           without a time limit: the process exits at once, as with no
           delay. *)
        Ocsigen_cohttp.shutdown None;
        Lwt.return ()
    | ["shutdown"; delay] ->
        (match float_of_string_opt delay with
        | Some delay -> Ocsigen_cohttp.shutdown (Some delay)
        | None ->
            Logs.err ~src:section (fun fmt ->
              fmt "shutdown: %S is not a delay in seconds" delay));
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
    { Extensions.default_hostname = default def.default_hostname default_hostname
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
  , Extensions.compose (List.map (fun i -> i vh config_info []) instructions) )

let site ?charset path instructions vh config_info parent_path =
  let path = parent_path @ Extensions.preprocess_site_path path in
  let composite =
    Extensions.compose (List.map (fun i -> i vh config_info path) instructions)
  in
  Extensions.site_ext composite charset path

(* Files kept for the rest of the server: logs, listening sockets, connections
   to a database... *)
let reserved_files = 100

(* The number of files that [connections] connections need: each one may use a
   second file, a static file it sends or a connection to a proxied server. *)
let needed_files connections =
  if connections > (max_int - reserved_files) / 2
  then max_int
  else reserved_files + (2 * connections)

(* The number of connections that [files] open files afford. *)
let affordable_connections files = (files - reserved_files) / 2
let limit_to_string n = if n = max_int then "none" else string_of_int n

(* The select engine of Lwt handles file descriptors below FD_SETSIZE only,
   1024 on most systems, and a socket above it would stop the server. *)
let select_max_files = 1024

let lwt_uses_select () =
  match Lwt_engine.id () with
  | Lwt_engine.Engine_id__select -> true
  | _ -> false

(* The number of files the server may open, once it has raised its soft limit
   to what [connections] connections need, and no further: the programs that
   the server runs inherit this limit, and some of them behave badly when it is
   high. [None] if there is no such limit. *)
let open_files ~connections =
  let max_files = if lwt_uses_select () then select_max_files else max_int in
  let fit (soft, hard) =
    Logs.debug ~src:section (fun fmt ->
      fmt "Limit on open files: %s (hard limit: %s)" (limit_to_string soft)
        (limit_to_string hard));
    min soft max_files
  in
  let limits =
    try Open_files.raise_soft_limit (min (needed_files connections) max_files)
    with Unix.Unix_error (error, f, _) ->
      Logs.warn ~src:section (fun fmt ->
        fmt "Cannot raise the limit on open files (%s: %s)" f
          (Unix.error_message error));
      Open_files.limits ()
  in
  match limits with
  | Some limits -> Some (fit limits)
  | None -> if max_files = max_int then None else Some max_files

(* How to accept more connections than the limit on open files affords. *)
let advice () =
  if not (lwt_uses_select ())
  then
    Some
      "raise the hard limit on open files (ulimit -Hn, or LimitNOFILE with systemd)"
  else if Lwt_sys.have `libev
  then Some "use the libev engine of Lwt (Lwt_engine.libev)"
  else if Sys.win32
  then None
  else Some "install conf-libev, so that Lwt uses libev rather than select"

(* The maximum number of simultaneous connections, lowered to what the limit on
   open files affords: beyond it, the server could not accept connections any
   more. The server listens on [ports] ports, and each one counts as a
   connection while it waits for a new one. *)
let max_number_of_connections ~ports =
  let wanted = Config.get_max_number_of_connections () in
  match open_files ~connections:wanted with
  | Some files when affordable_connections files < wanted ->
      let affordable = max 0 (affordable_connections files) in
      let pp_advice fmt =
        Option.iter (Format.fprintf fmt " To accept more, %s.") (advice ())
      in
      if affordable < ports
      then
        Logs.err ~src:section (fun fmt ->
          fmt
            "%d open files afford %d connections, and the server needs one for each port it listens on (%d): it cannot accept any.%t"
            files affordable ports pp_advice)
      else if Config.max_number_of_connections_is_set ()
      then
        Logs.warn ~src:section (fun fmt ->
          fmt
            "<maxconnected> is %d, but %d open files afford %d connections only, which the server accepts.%t"
            wanted files affordable pp_advice)
      else
        Logs.debug ~src:section (fun fmt ->
          fmt "Accepting %d connections at most, which %d open files afford.%t"
            affordable files pp_advice);
      affordable
  | Some _ | None -> wanted

(* Create the command pipe [path] if needed, and open it. The pipe is a control
   channel (shutdown, reload <file>, ...), so only the user running the server
   may write to it. An existing file is checked with [fstat] on the open
   descriptor, so that it cannot be swapped between the check and its use:
   anything but a FIFO owned by this user is refused. Our own FIFO with group or
   other permissions (Ocsigen Server 7 created it with mode 0o660) is replaced
   with a new one: changing its mode would not revoke a descriptor that another
   process opened before. The umask can only narrow the 0o600 mode given to
   [mkfifo]. The descriptor is not inherited by child processes. *)
let open_command_pipe path =
  let create () =
    Unix.mkfifo path 0o600;
    Logs.warn ~src:section (fun fmt -> fmt "Command pipe created")
  in
  let check {Unix.st_kind; st_uid; st_perm; _} =
    if st_kind <> Unix.S_FIFO
    then Error (`Refused "it is not a named pipe")
    else if st_uid <> Unix.geteuid ()
    then Error (`Refused "it belongs to another user")
    else if st_perm land 0o077 <> 0
    then Error `Permissive
    else Ok ()
  in
  let refuse reason =
    Logs.err ~src:section (fun fmt ->
      fmt
        ("Refusing the command pipe %s: %s. Remove it to let the server create"
       ^^ " a new one. I will continue without.")
        path reason);
    None
  in
  let rec use ~replace =
    let fd = Unix.(openfile path [O_RDWR; O_NONBLOCK; O_CLOEXEC]) 0 in
    match check (Unix.fstat fd) with
    | Ok () -> Some fd
    | Error error -> (
        Unix.close fd;
        match error with
        | `Refused reason -> refuse reason
        | `Permissive when replace ->
            Logs.warn ~src:section (fun fmt ->
              fmt "Command pipe %s has group or other permissions: replacing it"
                path);
            Unix.unlink path;
            create ();
            use ~replace:false
        | `Permissive -> refuse "it has group or other permissions")
    | exception e -> Unix.close fd; raise e
  in
  try
    (try create () with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
    use ~replace:true
  with Unix.Unix_error _ as e ->
    Logs.warn ~src:section (fun fmt ->
      fmt
        ("Cannot set up the command pipe %s. I will continue without."
       ^^ "@\n%s")
        path (Printexc.to_string e));
    None

let main_loop_is_running = ref false

let main config =
  report_to_terminal_by_default ();
  if !main_loop_is_running
  then Logs.err (fun fmt -> fmt "Cannot run main loop twice");
  main_loop_is_running := true;
  try
    (* Silent and daemon modes redirect the standard output and error to
       /dev/null: if the logs go there, nothing at all would be logged, errors
       included. Checked before detaching, so that -d fails at once. *)
    if Config.get_silent () && Messages.logs_to_console ()
    then raise Silent_with_console_logs;
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
      let ports = Config.get_ports () and ssl_ports = Config.get_ssl_ports () in
      let connection = match ports with [] -> [`All, 80] | l -> l in
      let ssl_connection =
        let ssl =
          match Config.get_ssl_info () with
          | None
          | Some {Config.ssl_certificate = None; Config.ssl_privatekey = None; _}
            ->
              None
          | Some
              { Config.ssl_certificate = Some crt
              ; Config.ssl_privatekey = Some key
              ; _ } ->
              Some (crt, key)
          | Some {Config.ssl_privatekey = None; _} ->
              raise (Config.Config_file_error "SSL key is missing")
          | Some {Config.ssl_certificate = None; _} ->
              raise (Config.Config_file_error "SSL certificate is missing")
        in
        match ssl_ports, ssl with
        | [], Some (crt, key) -> [`All, 443, (crt, key)]
        | l, Some (crt, key) -> List.map (fun (a, p) -> a, p, (crt, key)) l
        | _ -> []
      in
      (* A pipe to communicate with the server *)
      let commandpipe =
        Option.bind (Config.get_command_pipe ()) open_command_pipe
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
      (commandpipe
      |> Option.iter @@ fun fd ->
         let pipe =
           Lwt_unix.of_unix_file_descr fd |> Lwt_io.(of_fd ~mode:input)
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
                   (* A failing order must not stop the command pipe: the
                      later ones, such as the reopen_logs of a log rotation,
                      would be ignored. *)
                   Logs.err ~src:section (fun fmt ->
                     fmt "Command %S failed: %s" s (Printexc.to_string e));
                   Lwt.return ())
           >>= f
         in
         ignore (f () : 'a Lwt.t));
      (* Enforce the maximum number of simultaneous connections: conduit
         throttles its accept loop (across all listening sockets, as file
         descriptors are a global resource) while the number of active
         connections exceeds this cap. Without this call the configured limit
         would have no effect. *)
      let max_connections =
        max_number_of_connections
          ~ports:(List.length connection + List.length ssl_connection)
      in
      Config.set_max_number_of_connections_in_effect max_connections;
      Conduit_lwt_server.set_max_active max_connections;
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
        (* Only the detached process is silent: the user who launched the
           server learns its process id. *)
        Logs.app ~src:section (fun fmt -> fmt "Process %d detached" pid);
        write_pid pid)
    else (
      write_pid (Unix.getpid ());
      Lwt_main.run (run ()))
  with e ->
    let msg, errno = errmsg e in
    Messages.errlog msg; exit errno

let exec config =
  report_to_terminal_by_default ();
  Config.has_config_file := true;
  match config with
  | [] -> ()
  | [h] ->
      (try Parseconfig.first_pass h
       with e ->
         let msg, errno = errmsg e in
         Messages.errlog msg; exit errno);
      main (fun () ->
        (* Now I can load the modules *)
        Dynlink_wrapper.allow_unsafe_modules true;
        Extensions.start_initialisation ();
        Parseconfig.later_pass h;
        (* As libraries are reloaded each time the config file is
          read, we do not allow to register extensions in
          libraries. Seems it does not work :-/ *)
        Dynlink_wrapper.prohibit ["Extensions.R"])
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
      ?keepalive_timeout
      ?min_data_rate
      ?max_connections_per_address
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
  Option.iter Config.set_max_number_of_connections max_number_of_connections;
  Option.iter Config.set_client_timeout client_timeout;
  Option.iter Config.set_keepalive_timeout keepalive_timeout;
  Option.iter Config.set_min_data_rate min_data_rate;
  Option.iter Config.set_max_connections_per_address max_connections_per_address;
  Option.iter Config.set_server_timeout server_timeout;
  Option.iter Config.set_filebuffersize filebuffersize;
  Option.iter Config.set_maxrequestbodysize maxrequestbodysize;
  Option.iter Config.set_maxrequestbodysizeinmemory maxrequestbodysizeinmemory;
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
    (* Logged here rather than above, where the logs are not open yet. *)
    Option.iter
      (fun _ ->
         Logs.warn ~src:section (fun fmt ->
           fmt
             "Ocsigen.Server.start: ~server_timeout has no effect and is ignored."))
      server_timeout;
    Extensions.start_initialisation ();
    Extensions.set_hosts instructions)

(* Registries through which the Staticmod and Revproxy extensions, once loaded,
   publish their serving functions. This lets the one-command serve mode use
   them without statically linking them (which would clash with the
   configuration-file path, where the extensions are loaded dynamically). *)
let static_server : (dir:string -> instruction) option ref = ref None
let register_static_server f = static_server := Some f
let reverse_proxy_server : (target:string -> instruction) option ref = ref None
let register_reverse_proxy_server f = reverse_proxy_server := Some f

(* Likewise for the Securityheaders extension, whose serving instruction takes
   no argument. *)
let security_headers : instruction option ref = ref None
let register_security_headers f = security_headers := Some f

(* Likewise for the Deflatemod extension, which publishes a compression
   instruction with safe defaults so that serve mode compresses responses. *)
let compression : instruction option ref = ref None
let register_compression f = compression := Some f

(* Common setup of the one-command modes: no configuration file and no log
   directory are required. Logs go to stderr, and there is no command pipe:
   the server is stopped with a signal (Ctrl-C, SIGTERM). *)
let one_command_setup () =
  report_to_terminal_by_default ();
  Config.set_log_to_stderr true

(* Load the extension [package] on demand. On failure: exit if [required],
   otherwise log a warning and continue. *)
let load_extension ?(required = true) package =
  (* Extensions may use C stubs, as bytesrw's zlib and zstd bindings do, and
     the bytecode dynamic linker rejects those unless they are allowed. The
     configuration-file path does the same before loading extensions. *)
  Dynlink_wrapper.allow_unsafe_modules true;
  try
    Ocsigen_base.Loader.loadfiles
      (fun () -> ())
      (fun () -> ())
      false
      (Ocsigen_base.Loader.findfiles package)
  with e ->
    let msg, errno = errmsg e in
    if required then (Messages.errlog msg; exit errno) else Messages.warning msg

(* Load the extension [package], which a one-command mode cannot work without,
   and return the serving function it published in [registry]. *)
let load_one_command_extension package registry =
  load_extension package;
  match !registry with
  | Some f -> f
  | None ->
      Messages.errlog (package ^ " did not register its serving function");
      exit 1

(* The one-command modes listen on the loopback interface unless told
   otherwise: a server started in one command, often by mistake in the wrong
   directory, should not be reachable from the network by default. *)
let default_one_command_address = `IPv4 Unix.inet_addr_loopback

(* The URL that the startup banner of the one-command modes shows. *)
let listen_url address port =
  match address with
  | `IPv4 a when a = Unix.inet_addr_any ->
      Printf.sprintf "http://0.0.0.0:%d (all interfaces)" port
  | `IPv4 a -> Printf.sprintf "http://%s:%d" (Unix.string_of_inet_addr a) port
  | `IPv6 a when a = Unix.inet6_addr_any ->
      Printf.sprintf "http://[::]:%d (all interfaces)" port
  | `IPv6 a -> Printf.sprintf "http://[%s]:%d" (Unix.string_of_inet_addr a) port
  | `All -> Printf.sprintf "port %d of all interfaces" port
  | `Unix path -> "unix:" ^ path

let serve
      ?(address = default_one_command_address)
      ?(port = 8080)
      ?(directory_listing = false)
      ~dir
      ()
  =
  one_command_setup ();
  let static =
    load_one_command_extension "ocsigenserver.ext.staticmod" static_server
  in
  (* Securityheaders and Deflatemod are best-effort: they give safe headers and
     compression by default, but serving files must not depend on them. They
     publish their instruction through the registries above. *)
  load_extension ~required:false "ocsigenserver.ext.securityheaders";
  load_extension ~required:false "ocsigenserver.ext.deflatemod";
  (* The compression and security-header filters must come after the file
     server. Compression comes first, so that the security headers are those of
     the response actually sent. *)
  let instructions =
    static ~dir
    :: (Option.to_list !compression @ Option.to_list !security_headers)
  in
  Logs.app ~src:section (fun fmt ->
    fmt "Serving %s on %s" dir (listen_url address port));
  start
    ~ports:[address, port]
    [host ~list_directory_content:directory_listing instructions]

let reverse_proxy
      ?(address = default_one_command_address)
      ?(port = 8080)
      ~target
      ()
  =
  one_command_setup ();
  let proxy =
    load_one_command_extension "ocsigenserver.ext.revproxy" reverse_proxy_server
  in
  Logs.app ~src:section (fun fmt ->
    fmt "Proxying %s to %s" (listen_url address port) target);
  start ~ports:[address, port] [host [proxy ~target]]
