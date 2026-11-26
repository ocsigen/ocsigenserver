(* Tests of the header parsers of Ocsigen_http.Header. *)

module Entity_tag = Ocsigen_http.Header.Entity_tag

let show_list values =
  Printf.printf "%s -> %s\n"
    (String.concat " | " (List.map (Printf.sprintf "%S") values))
    (match Entity_tag.parse_list values with
    | `Any -> "*"
    | `Tags tags -> String.concat " " (List.map Entity_tag.to_string tags))

let tag weak opaque = Entity_tag.make ~weak opaque

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
  show_list ["\"a b\", \"c\""];
  show_list ["\"a\x7f\", \"c\""];
  print_endline "== Entity_tag.make";
  List.iter
    (fun opaque ->
       Printf.printf "%S -> %s\n" opaque
         (match Entity_tag.make ~weak:false opaque with
         | (_ : Entity_tag.t) -> "valid"
         | exception Invalid_argument _ -> "Invalid_argument"))
    ["a-1"; "!#~\x80"; "a\"b"; "a b"; "a\tb"; "a\x7f"];
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

(* Times of 2026-10-17, 2044-01-01 and 2051-01-01, 00:00 GMT. *)
let now_2026 = 1792195200.
let now_2044 = 2335219200.
let now_2051 = 2556144000.

let show_date ?(now = now_2026) s =
  Printf.printf "%S -> %s\n" s
    (match Ocsigen_http.Header.Date.parse ~now s with
    | Some t -> Printf.sprintf "%.0f" t
    | None -> "invalid")

let () =
  print_endline "== Date.parse";
  List.iter show_date
    [ "Sun, 06 Nov 1994 08:49:37 GMT"
    ; "Sunday, 06-Nov-94 08:49:37 GMT"
    ; "Sun Nov  6 08:49:37 1994"
    ; "Thu, 01 Jan 1970 00:00:00 GMT"
    ; "Wed, 01 Jan 2020 12:00:00 GMT"
    ; "Wednesday, 01-Jan-20 12:00:00 GMT"
    ; "Wed Jan  1 12:00:00 2020"
    ; "Wed Jan 01 12:00:00 2020"
    ; "Thu, 29 Feb 2024 23:59:60 GMT"
    ; "Sat, 31 Dec 2016 23:59:60 GMT"
    ; "Sat, 29 Feb 2025 00:00:00 GMT"
    ; "Thu, 29 Feb 1900 00:00:00 GMT"
    ; "Tue, 29 Feb 2000 00:00:00 GMT"
    ; "Wed, 01 Jan 2020 24:00:00 GMT"
    ; "Wed, 01 Jan 2020 12:00:00 CET"
    ; "Wed, 01 Foo 2020 12:00:00 GMT"
    ; "Xyz, 01 Jan 2020 12:00:00 GMT"
    ; "wed, 01 jan 2020 12:00:00 GMT"
    ; "Wed, 01 Jan 2020 12:00:00 GMT trailing"
    ; "Sun, 06 Nov 94 08:49:37 GMT"
    ; "Sun Nov  6 08:49:37 94"
    ; "Sun, 06 Nov 1_94 08:49:37 GMT"
    ; "Sun, 6 Nov 1994 08:49:37 GMT"
    ; "Sun, 06 Nov 1994 8:49:37 GMT"
    ; "Wed,01 Jan 2020 12:00:00 GMT"
    ; "Wed,  01 Jan 2020 12:00:00 GMT"
    ; "Wed, 01 Jan 2020 12:00:00  GMT"
    ; "Wed, 01 Jan 2020 12:00:00\tGMT"
    ; "Wednesday,01-Jan-20 12:00:00 GMT"
    ; "Wed Jan 1 12:00:00 2020"
    ; "Wed Jan  01 12:00:00 2020"
    ; "Wed Jan 01  12:00:00 2020"
    ; "garbage"
    ; "" ];
  print_endline "== Date.parse, two-digit years";
  show_date ~now:now_2044 "Sunday, 06-Nov-94 08:49:37 GMT";
  show_date ~now:now_2051 "Saturday, 01-Jan-00 00:00:00 GMT";
  show_date ~now:now_2051 "Saturday, 01-Jan-01 00:00:00 GMT"
