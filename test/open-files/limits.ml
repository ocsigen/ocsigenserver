(* How Ocsigen.Open_files raises the soft limit that the cram test sets. *)

let raise_soft_limit n =
  match Ocsigen.Open_files.raise_soft_limit n with
  | Some limits -> limits
  | None -> failwith "no limits once raised"

let () =
  match Ocsigen.Open_files.limits () with
  | None -> print_endline "no limits"
  | Some (soft, hard) ->
      Printf.printf "soft limit: %d\n" soft;
      Printf.printf "raised to 300: %d\n" (fst (raise_soft_limit 300));
      Printf.printf "not lowered to 100: %d\n" (fst (raise_soft_limit 100));
      let ((soft', hard') as limits) = raise_soft_limit max_int in
      Printf.printf "raised further, up to the hard limit: %b\n"
        (soft' > 300 && soft' <= hard);
      Printf.printf "hard limit unchanged: %b\n" (hard' = hard);
      Printf.printf "read again: %b\n"
        (Ocsigen.Open_files.limits () = Some limits)
