(* A configuration with an error leaves the settings of the tags of [<server>]
   as they were: [Ocsigen.Parseconfig.later_pass], which reads them each time
   the configuration file is reloaded, puts them back on an error. Each tag
   below gets a value other than the current one, first followed by a tag
   that does not exist, then alone. A configuration with only an error then
   keeps them, and one without these tags gives back the values they had
   before the first reading. *)

module C = Ocsigen.Config

let int get () = string_of_int (get ())
let bool get () = string_of_bool (get ())
let option f get () = Option.fold ~none:"none" ~some:f (get ())

(* Each tag, with its value and the function that reads its setting. *)
let tags =
  [ "charset", "iso-8859-1", option Fun.id C.get_default_charset
  ; "uploaddir", "upload", option Fun.id C.get_uploaddir
  ; "mimefile", "other.types", option Fun.id C.get_mimefile
  ; "maxconnected", "7", int C.get_max_number_of_connections
  ; "maxretries", "3", int C.get_maxretries
  ; "clienttimeout", "5", int C.get_client_timeout
  ; "servertimeout", "6", int C.get_server_timeout
  ; "keepalivetimeout", "20", int C.get_keepalive_timeout
  ; "mindatarate", "1000", int C.get_min_data_rate
  ; "maxconnectionsperaddress", "16", int C.get_max_connections_per_address
  ; "netbuffersize", "4096", int Ocsigen_base.Ocsigen_stream.get_net_buffer_size
  ; "filebuffersize", "4096", int C.get_filebuffersize
  ; "maxrequestbodysize", "1MB", option Int64.to_string C.get_maxrequestbodysize
  ; "maxrequestbodysizeinmemory", "1kB", int C.get_maxrequestbodysizeinmemory
  ; "maxuploadfilesize", "1kB", option Int64.to_string C.get_maxuploadfilesize
  ; "commandpipe", "local.cmd", option Fun.id C.get_command_pipe
  ; "shutdowntimeout", "1", option string_of_float C.get_shutdown_timeout
  ; "debugmode", "", bool C.get_debugmode
  ; "usedefaulthostname", "", bool C.get_usedefaulthostname
  ; "disablepartialrequests", "", bool C.get_disablepartialrequests
  ]

let configuration =
  List.map
    (fun (name, value, _) ->
      Xml.Element (name, [], if value = "" then [] else [Xml.PCData value])
    )
    tags

let values () = List.map (fun (_, _, get) -> get ()) tags

let pass config =
  match Ocsigen.Parseconfig.later_pass config with
  | () -> print_endline "accepted"
  | exception C.Config_file_error msg -> print_endline msg

let error = Xml.Element ("nosuchtag", [], [])

let () =
  let before = values () in
  pass (configuration @ [error]);
  let after_error = values () in
  pass configuration;
  let after = values () in
  pass [error];
  let after_second_error = values () in
  pass [];
  let without = values () in
  List.iteri
    (fun i (name, _, _) ->
      let value l = List.nth l i in
      Printf.printf
        "<%s>: %s, after the error %s, then %s, after the error %s, without it \
         %s\n"
        name (value before) (value after_error) (value after)
        (value after_second_error) (value without)
    )
    tags
