(* Loads the libraries given on the command line as Loader.loadfiles does for
   a findlib package: first the package dep alone, then the package app with
   its dependencies dep and dep2. See run.t for what is checked. *)

let pre () =
  Ctx.config := "set";
  print_endline "pre"

let post () =
  Ctx.config := "none";
  print_endline "post"

let () =
  match Array.to_list Sys.argv with
  | [_; dep; dep2; app] ->
      Ocsigen_base.Loader.loadfiles pre post true [dep];
      Ocsigen_base.Loader.loadfiles pre post true [dep; dep2; app]
  | _ ->
      prerr_endline "usage: test.bc dep.cma dep2.cma app.cma";
      exit 2
