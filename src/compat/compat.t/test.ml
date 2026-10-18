(* Code written for Ocsigen Server 7 uses the old module names of the server,
   of the base library and of the HTTP layer. The ocsigenserver-compat library
   alone must provide all of them. *)

let () =
  print_endline (Ocsigen_lib.Url.decode "a%20b");
  print_endline (Ocsigen_header.Name.to_string Ocsigen_header.Name.host);
  print_endline Ocsigen_config.server_name
