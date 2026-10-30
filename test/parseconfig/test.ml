(* Tests for configuration tags parsed by [Ocsigen.Parseconfig]:

   - the SI and binary units accepted by [parse_size_tag], shared by
     [<maxrequestbodysize>], [<maxuploadfilesize>] and
     [<maxrequestbodysizeinmemory>];
   - the wiring of each of these tags to its [Ocsigen.Config] setter, which is
     what a missing tag branch would break;
   - the defaults and the wiring of the connection tags [<keepalivetimeout>],
     [<mindatarate>] and [<maxconnectionsperaddress>];
   - the default and the wiring of [<commandpipe>] and [<logdir>];
   - the log levels set by [<loglevel>] and by the [logs:] command;
   - the warnings for the tags that are accepted but have no effect;
   - the check of the sizes given through the API;
   - the default ports, which skip the Unix-domain sockets. *)

let show_int n = if n = max_int then "max_int" else string_of_int n
let show_int64_option = function None -> "None" | Some n -> Int64.to_string n

let show_size s =
  let result =
    match Ocsigen.Parseconfig.parse_size_tag "test" s with
    | n -> show_int64_option n
    | exception Ocsigen.Config.Config_file_error _ -> "Config_file_error"
  in
  Printf.printf "%-12s -> %s\n" (Printf.sprintf "%S" s) result

(* Bare single-letter suffixes (k, M, G, T) are binary; two-letter suffixes
   (kB/ko, MB/Mo, ...) are SI; three-letter suffixes (kiB/kio, MiB, ...) are
   binary. *)
let () =
  print_endline "parse_size_tag:";
  List.iter show_size
    [ "8192"
    ; "8192B"
    ; "8192o"
    ; "10kB"
    ; "10ko"
    ; "10kiB"
    ; "10kio"
    ; "2MB"
    ; "2MiB"
    ; "1GB"
    ; "1TB"
    ; "1k"
    ; "1M"
    ; "infinity"
    ; ""
    ; "not-a-size"
    ; "-1"
    ; "-1k"
    ; "10000000T" ]

(* Feed a single tag through [later_pass], as if it were a child of
   [<server>] in the configuration file, then read the resulting value. *)
let show_tag tag value get =
  Ocsigen.Parseconfig.later_pass [Xml.Element (tag, [], [Xml.PCData value])];
  Printf.printf "<%s>%s -> %s\n" tag value (get ())

let () =
  let in_memory () = show_int (Ocsigen.Config.get_maxrequestbodysizeinmemory ())
  and body_size () =
    show_int64_option (Ocsigen.Config.get_maxrequestbodysize ())
  and upload_size () =
    show_int64_option (Ocsigen.Config.get_maxuploadfilesize ())
  in
  print_newline ();
  Printf.printf "default in-memory request body size -> %s\n" (in_memory ());
  print_newline ();
  print_endline "tags:";
  show_tag "maxrequestbodysize" "2MB" body_size;
  show_tag "maxrequestbodysize" "infinity" body_size;
  show_tag "maxuploadfilesize" "3MB" upload_size;
  show_tag "maxrequestbodysizeinmemory" "1MB" in_memory;
  show_tag "maxrequestbodysizeinmemory" "infinity" in_memory

(* The connection tags: their defaults, then their wiring. *)
let () =
  let int get () = string_of_int (get ()) in
  print_newline ();
  Printf.printf
    "defaults -> keepalivetimeout %d, mindatarate %d, maxconnectionsperaddress %d\n"
    (Ocsigen.Config.get_keepalive_timeout ())
    (Ocsigen.Config.get_min_data_rate ())
    (Ocsigen.Config.get_max_connections_per_address ());
  show_tag "keepalivetimeout" "20" (int Ocsigen.Config.get_keepalive_timeout);
  show_tag "mindatarate" "1000" (int Ocsigen.Config.get_min_data_rate);
  show_tag "maxconnectionsperaddress" "16"
    (int Ocsigen.Config.get_max_connections_per_address)

(* There is no command pipe unless the configuration gives one. *)
let () =
  let command_pipe () =
    Option.value (Ocsigen.Config.get_command_pipe ()) ~default:"none"
  in
  print_newline ();
  Printf.printf "default command pipe -> %s\n" (command_pipe ());
  show_tag "commandpipe" "local.cmd" command_pipe

(* There is no log directory unless the configuration gives one, and an empty
   path is none. [<logdir>] is read by [first_pass]. Choosing no syslog keeps
   the log directory. *)
let () =
  let logdir () = Option.value (Ocsigen.Config.get_logdir ()) ~default:"none" in
  let show_logdir value =
    Ocsigen.Parseconfig.first_pass
      [Xml.Element ("logdir", [], [Xml.PCData value])];
    Printf.printf "<logdir>%s -> %s\n" value (logdir ())
  in
  print_newline ();
  Printf.printf "default log directory -> %s\n" (logdir ());
  show_logdir "log";
  Ocsigen.Config.set_syslog_facility None;
  Printf.printf "no syslog -> %s\n" (logdir ());
  show_logdir ""

(* [<loglevel source="..." level="..."/>] sets the level of the log sources
   of that name once the configuration is loaded; the [logs:] command of
   the command pipe sets it at run time. Both act on the existing sources:
   they make no new one. *)
let () =
  let src = Logs.Src.create "test:app" in
  let level () = Logs.level_to_string (Logs.Src.level src) in
  let same_name () =
    List.length
      (List.filter (fun s -> Logs.Src.name s = "test:app") (Logs.Src.list ()))
  in
  let loglevel atts =
    match
      Ocsigen.Parseconfig.later_pass [Xml.Element ("loglevel", atts, [])]
    with
    | () -> Printf.printf "-> %s\n" (level ())
    | exception Ocsigen.Config.Config_file_error msg ->
        Printf.printf "-> Config_file_error: %s\n" msg
  in
  let command args =
    Lwt_main.run (Ocsigen.Messages.command_f Exit "" args);
    Printf.printf "-> %s\n" (level ())
  in
  print_newline ();
  print_endline "loglevel:";
  Printf.printf "default -> %s\n" (level ());
  print_string "<loglevel source=\"test:app\" level=\"debug\"/> ";
  loglevel ["source", "test:app"; "level", "debug"];
  print_string "<loglevel level=\"info\" source=\"test:app\"/> ";
  loglevel ["level", "info"; "source", "test:app"];
  print_string "<loglevel source=\"test:app\" level=\"loud\"/> ";
  loglevel ["source", "test:app"; "level", "loud"];
  print_string "<loglevel level=\"debug\"/> ";
  loglevel ["level", "debug"];
  print_string "<loglevel source=\"test:none\" level=\"debug\"/> ";
  loglevel ["source", "test:none"; "level", "debug"];
  print_string "logs:test:app warning ";
  command ["test:app"; "warning"];
  print_string "logs:test:app ";
  command ["test:app"];
  print_string "logs:test:app loud ";
  command ["test:app"; "loud"];
  Printf.printf "sources named test:app -> %d\n" (same_name ())

(* A tag that is accepted but has no effect says so with a warning, printed
   here on the standard output. *)
let () =
  Logs.set_reporter
    { Logs.report =
        (fun _src level ~over k msgf ->
          msgf (fun ?header:_ ?tags:_ fmt ->
            Format.kasprintf
              (fun message ->
                 Printf.printf "%s: %s\n"
                   (Logs.level_to_string (Some level))
                   message;
                 over ();
                 k ())
              fmt)) };
  print_newline ();
  print_endline "tags without effect:";
  Ocsigen.Parseconfig.later_pass
    [ Xml.Element ("servertimeout", [], [Xml.PCData "20"])
    ; Xml.Element ("datadir", [], [Xml.PCData "data"])
    ; Xml.Element ("filebuffersize", [], [Xml.PCData "8192"])
    ; Xml.Element ("minthreads", [], [Xml.PCData "10"])
    ; Xml.Element ("maxthreads", [], [Xml.PCData "30"])
    ; Xml.Element ("disablepartialrequests", [], [])
    ; Xml.Element ("respectpipeline", [], []) ]

(* The -vv option sets the level of the existing Ocsigen Server sources, and
   the default level of the sources created afterwards (those of the
   extensions loaded from the configuration file, of Eliom, of the
   application). The other existing sources keep theirs. *)
let () =
  let library = Logs.Src.create "test:library" in
  let ocsigen =
    List.find (fun s -> Logs.Src.name s = "ocsigen:config") (Logs.Src.list ())
  in
  Ocsigen.Config.set_veryverbose ();
  let later = Logs.Src.create "test:later" in
  let level src = Logs.level_to_string (Logs.Src.level src) in
  print_newline ();
  print_endline "-vv:";
  Printf.printf "existing Ocsigen source -> %s\n" (level ocsigen);
  Printf.printf "existing other source -> %s\n" (level library);
  Printf.printf "source created afterwards -> %s\n" (level later)

(* The sizes given through the API are checked as in the configuration file:
   a negative one would make every request body fail. *)
let () =
  print_endline "\nnegative sizes from the API:";
  let show name set =
    match set () with
    | () -> Printf.printf "%s -> accepted\n" name
    | exception Invalid_argument msg ->
        Printf.printf "%s -> Invalid_argument: %s\n" name msg
  in
  show "set_maxrequestbodysize (Some (-1L))" (fun () ->
    Ocsigen.Config.set_maxrequestbodysize (Some (-1L)));
  show "set_maxuploadfilesize (Some (-1L))" (fun () ->
    Ocsigen.Config.set_maxuploadfilesize (Some (-1L)));
  show "Server.host ~maxuploadfilesize:(Some (-1L))" (fun () ->
    ignore
      (Ocsigen.Server.host ~maxuploadfilesize:(Some (-1L)) []
       : Ocsigen.Server.host_config));
  show "set_maxrequestbodysize (Some 0L)" (fun () ->
    Ocsigen.Config.set_maxrequestbodysize (Some 0L));
  show "set_maxrequestbodysize None" (fun () ->
    Ocsigen.Config.set_maxrequestbodysize None)

(* The default ports of the hosts are the first ones of the server that are
   not Unix-domain sockets, which have no port, or 80 and 443. *)
let () =
  print_endline "\ndefault ports (HTTP, HTTPS):";
  let show name ports ssl_ports =
    Ocsigen.Config.set_ports ports;
    Ocsigen.Config.set_ssl_ports ssl_ports;
    Printf.printf "%s -> %d, %d\n" name
      (Ocsigen.Config.get_default_port ())
      (Ocsigen.Config.get_default_sslport ())
  in
  show "Unix-domain sockets first"
    [`Unix "http.sock", 1; `All, 8080]
    [`Unix "https.sock", 2; `All, 8443];
  show "Unix-domain sockets only" [`Unix "http.sock", 1] [`Unix "https.sock", 2];
  show "no ports" [] []
