(* Unit tests for [Ocsigen_http.Header.add_vary]. *)

let show_lines = function [] -> "(none)" | l -> String.concat " | " l

let show lines =
  let h = Cohttp.Header.of_list (List.map (fun v -> "Vary", v) lines) in
  let h = Ocsigen_http.Header.add_vary h Ocsigen_http.Header.Name.origin in
  Printf.printf "%-28s -> %s\n" (show_lines lines)
    (show_lines (Cohttp.Header.get_multi h "vary"))

let () =
  List.iter show
    [ []
    ; ["Cookie"]
    ; ["Cookie, Accept-Encoding"]
    ; ["Cookie"; "Accept-Encoding"]
    ; ["Cookie, ,Accept-Encoding,"]
    ; ["Cookie"; "Origin"]
    ; ["Cookie"; "ORIGIN"]
    ; ["Cookie, *"]
    ; ["Cookie"; "  *"] ]
