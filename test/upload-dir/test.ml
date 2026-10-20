(* The <uploaddir> tag of <server>: a directory allows uploads, an empty
   element forbids them. *)

let show content =
  Ocsigen.Parseconfig.later_pass [Xml.Element ("uploaddir", [], content)];
  print_endline
    (match Ocsigen.Config.get_uploaddir () with
    | None -> "None"
    | Some dir -> Printf.sprintf "Some %S" dir)

let () =
  show [Xml.PCData "/var/upload"];
  show []
