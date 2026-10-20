(* Tests of the header parsers of Ocsigen_http.Header. *)

module Entity_tag = Ocsigen_http.Header.Entity_tag

let show_list values =
  Printf.printf "%s -> %s\n"
    (String.concat " | " (List.map (Printf.sprintf "%S") values))
    (match Entity_tag.parse_list values with
    | `Any -> "*"
    | `Tags tags -> String.concat " " (List.map Entity_tag.to_string tags))

let tag weak opaque = {Entity_tag.weak; opaque}

let () =
  print_endline "== Entity_tag.parse_list";
  show_list ["*"];
  show_list [" * "];
  show_list ["\"a\""];
  show_list ["W/\"a\", \"b\",\"c\""];
  show_list ["\"a\""; "W/\"b\""];
  show_list ["*"; "\"a\""];
  show_list ["\"a,b\", \"c\""];
  show_list ["\"\""];
  show_list [""];
  show_list ["a, \"b\""];
  show_list ["\"unterminated"];
  show_list ["w/\"a\""];
  show_list ["\"a\"x, \"b\""];
  show_list ["\"a\" , \"b\" "];
  print_endline "== Entity_tag comparisons";
  List.iter
    (fun (a, b) ->
       Printf.printf "%s %s: weak %b, strong %b\n" (Entity_tag.to_string a)
         (Entity_tag.to_string b)
         (Entity_tag.weak_equal a b)
         (Entity_tag.strong_equal a b))
    [ tag false "1", tag false "1"
    ; tag false "1", tag true "1"
    ; tag true "1", tag true "1"
    ; tag false "1", tag false "2" ]
