(* Unit tests for [Ocsigen_base.Ocsigen_stream.string_of_stream], which
   collects a string stream into a single string, failing with
   [String_too_large] when the total length exceeds the given limit. *)

module S = Ocsigen_base.Ocsigen_stream

(* A stream made of the given chunks. *)
let chunks l =
  let rec step = function
    | [] -> S.empty None
    | c :: r -> S.cont c (fun () -> step r)
  in
  S.make (fun () -> step l)

let show limit l =
  let result =
    Lwt_main.run
      (Lwt.catch
         (fun () ->
            Lwt.map (Printf.sprintf "%S")
              (S.string_of_stream limit (S.get (chunks l))))
         (function
           | S.String_too_large -> Lwt.return "String_too_large"
           | e -> Lwt.fail e))
  in
  Printf.printf "limit %-7s [%s] -> %s\n"
    (if limit = max_int then "max_int" else string_of_int limit)
    (String.concat "; " (List.map (Printf.sprintf "%S") l))
    result

let () =
  show 0 [];
  show 3 ["abc"];
  show 2 ["abc"];
  show 6 ["ab"; "cd"; "ef"];
  show 5 ["ab"; "cd"; "ef"];
  (* A huge limit must not be preallocated. *)
  show max_int ["ab"; "cd"; "ef"]
