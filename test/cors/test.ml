(* The check of the arguments of [Cors.run]. *)

let show name f =
  match f () with
  | (_ : Ocsigen.Server.instruction) -> Printf.printf "%s -> accepted\n" name
  | exception Invalid_argument msg ->
      Printf.printf "%s -> Invalid_argument: %s\n" name msg

let () =
  show "Cors.run ~max_age:(-1) ()" (fun () -> Cors.run ~max_age:(-1) ());
  show "Cors.run ~max_age:0 ()" (fun () -> Cors.run ~max_age:0 ());
  show "Cors.run ()" (fun () -> Cors.run ())
