(* Unit tests for the percent-encoding helpers of [Ocsigen_base.Lib.Url]. *)

open Ocsigen_base.Lib

let show f s =
  match f s with
  | s' -> Printf.printf "%-24s -> %s\n" s s'
  | exception Failure msg -> Printf.printf "%-24s -> Failure %S\n" s msg

let () =
  print_endline "decode ~plus:false:";
  List.iter
    (show (Url.decode ~plus:false))
    [
      "unreserved";
      "caf%C3%A9";
      "a%20b";
      "a+b";
      "%41%42%43";
      "%2e%2e%2fetc%2fpasswd";
      "100%";
      "a%2";
      "%zz";
    ];
  print_newline ();
  print_endline "decode ~plus:true:";
  List.iter (show (Url.decode ~plus:true)) [ "a+b"; "a%2Bb" ];
  print_newline ();
  print_endline "encode ~plus:false:";
  List.iter
    (show (Url.encode ~plus:false))
    [ "unreserved"; "café"; "a b"; "a/b" ];
  print_newline ();
  print_endline "encode ~plus:true:";
  List.iter (show (Url.encode ~plus:true)) [ "a b"; "a+b" ]
