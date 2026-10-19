let usage =
  "usage: ocsigenserver [-c configfile | [--serve] DIR | --reverse-proxy URL [options]]"

let section = Logs.Src.create "ocsigen:main"

(* Report command-line errors through Logs. A stderr reporter is installed
   below so that these messages are visible before the server configures its own
   logging. *)
let () = Logs.set_reporter Ocsigen.Messages.stdio_reporter

let fatal fmt =
  Format.kasprintf
    (fun msg ->
       Logs.err ~src:section (fun m -> m "%s" msg);
       exit 2)
    fmt

(* Serve-mode options. When a directory to serve is given (either through
   [--serve] or as a positional argument), the server starts without a
   configuration file. Otherwise the legacy configuration-file path is used. *)
let serve_dir = ref None
let serve_port = ref None
let serve_address = ref None
let directory_listing = ref false
let reverse_proxy = ref None
let config_given = ref false

let set_serve_dir d =
  match !serve_dir with
  | None -> serve_dir := Some d
  | Some d' when d' = d -> ()
  | Some _ -> fatal "only one directory to serve can be given"

(* The address to listen on in the one-command modes: an IPv4 or IPv6 address,
   such as 0.0.0.0 to listen on all the IPv4 interfaces. *)
let set_serve_address s =
  match Unix.inet_addr_of_string s with
  | a when Unix.domain_of_sockaddr (Unix.ADDR_INET (a, 0)) = Unix.PF_INET6 ->
      serve_address := Some (`IPv6 a)
  | a -> serve_address := Some (`IPv4 a)
  | exception Failure _ -> fatal "--address: %s is not an IP address" s

let set_configfile f =
  config_given := true;
  Ocsigen.Config.set_configfile f

let () =
  let alt_msg =
    "Alternate config file (default " ^ Ocsigen.Config.get_config_file () ^ ")"
  and serve_msg = "Serve the static files of DIR without a configuration file"
  and proxy_msg =
    "Forward every request to the base URL given, without a configuration file"
  and port_msg = "Port to listen on in the one-command modes (default 8080)"
  and address_msg =
    "IP address to listen on in the one-command modes (default 127.0.0.1; 0.0.0.0 for all interfaces)"
  and listing_msg = "List directory contents in serve mode (when no index file)"
  and silent_msg =
    "Silent mode (error messages in errors.log only; needs log files or syslog)"
  and pid_msg = "Specify a file where to write the PIDs of servers"
  and daemon_msg = "Daemon mode (detach the process; implies -s)"
  and verbose_msg = "Verbose mode"
  and veryverbose_msg = "Very verbose mode (info)"
  and debug_msg = "Extremely verbose mode (debug)"
  and version_msg = "Display version number and exit" in
  try
    Arg.parse_argv Sys.argv
      [ "-c", Arg.String set_configfile, alt_msg
      ; "--config", Arg.String set_configfile, alt_msg
      ; "--serve", Arg.String set_serve_dir, serve_msg
      ; ( "--reverse-proxy"
        , Arg.String (fun u -> reverse_proxy := Some u)
        , proxy_msg )
      ; "-P", Arg.Int (fun p -> serve_port := Some p), port_msg
      ; "--port", Arg.Int (fun p -> serve_port := Some p), port_msg
      ; "--address", Arg.String set_serve_address, address_msg
      ; "--directory-listing", Arg.Set directory_listing, listing_msg
      ; "-s", Arg.Unit Ocsigen.Config.set_silent, silent_msg
      ; "--silent", Arg.Unit Ocsigen.Config.set_silent, silent_msg
      ; "-p", Arg.String Ocsigen.Config.set_pidfile, pid_msg
      ; "--pidfile", Arg.String Ocsigen.Config.set_pidfile, pid_msg
      ; "-v", Arg.Unit Ocsigen.Config.set_verbose, verbose_msg
      ; "--verbose", Arg.Unit Ocsigen.Config.set_verbose, verbose_msg
      ; "-vv", Arg.Unit Ocsigen.Config.set_veryverbose, veryverbose_msg
      ; ( "--veryverbose"
        , Arg.Unit Ocsigen.Config.set_veryverbose
        , veryverbose_msg )
      ; "-vvv", Arg.Unit Ocsigen.Config.set_debug, debug_msg
      ; "--debug", Arg.Unit Ocsigen.Config.set_debug, debug_msg
      ; "-d", Arg.Unit Ocsigen.Config.set_daemon, daemon_msg
      ; "--daemon", Arg.Unit Ocsigen.Config.set_daemon, daemon_msg
      ; "--version", Arg.Unit Ocsigen.Config.display_version, version_msg ]
      set_serve_dir usage
  with
  | Arg.Help s -> print_endline s; exit 0
  | Arg.Bad s -> fatal "%s" (String.trim s)

(* Resolve [dir] to an absolute path and check that it is an existing directory.
   The directory is never created. *)
let check_serve_dir dir =
  let dir =
    if Filename.is_relative dir
    then Filename.concat (Sys.getcwd ()) dir
    else dir
  in
  match Sys.is_directory dir with
  | true -> dir
  | false -> fatal "%s is not a directory" dir
  | exception Sys_error _ -> fatal "no such directory: %s" dir

let () =
  (* The one-command modes log to the terminal, which -s and -d redirect to
     /dev/null. *)
  if
    (!serve_dir <> None || !reverse_proxy <> None)
    && Ocsigen.Config.get_silent ()
  then
    fatal
      "-s and -d cannot be combined with serve mode or --reverse-proxy, which log to the terminal";
  match !serve_dir, !reverse_proxy with
  | Some _, Some _ -> fatal "--serve and --reverse-proxy cannot be combined"
  | Some dir, None ->
      if !config_given
      then fatal "-c/--config cannot be combined with serve mode";
      let dir = check_serve_dir dir in
      Ocsigen.Server.serve ~dir ?address:!serve_address ?port:!serve_port
        ~directory_listing:!directory_listing ()
  | None, Some target ->
      if !config_given
      then fatal "-c/--config cannot be combined with --reverse-proxy";
      if !directory_listing
      then fatal "--directory-listing is only valid when serving a directory";
      Ocsigen.Server.reverse_proxy ~target ?address:!serve_address
        ?port:!serve_port ()
  | None, None ->
      if !serve_port <> None || !serve_address <> None || !directory_listing
      then
        fatal
          "--port, --address and --directory-listing require a directory to serve or --reverse-proxy";
      Ocsigen.Server.exec (Ocsigen.Parseconfig.parse_config ())
