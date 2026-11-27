(* Differential tests of HTTP/1.1 parsers: send the same bytes to several
   echo servers (see echo.mli) and compare what they understood.

   httpdiff show [options] FILE...   print what each server answered
   httpdiff run [options] FILE...    group the servers that agree, case by
                                     case, and show the disagreements

   FILE is a corpus file (see corpus.mli). A case is named after its file
   and its name in the file, such as host/ipv6. Options:
   --target NAME=[tls:]HOST:PORT   a server (repeatable); HOST may be an IPv6
                                   address in brackets
   --case FILE/NAME                only this case (repeatable)
   --underscore-names NAME          the server NAME writes the hyphens of
                                   header names as underscores (repeatable)
   --ignore-field FIELD            compare without this header field
                                   (repeatable)
   --first-byte SECONDS, --idle SECONDS, --settle SECONDS
                                   as for rawhttp (see Raw_http.receive)

   run exits with status 1 when a server could not be asked a case.
   See README.md for running it against the servers of HTTP Garden. *)

open Differential

type target = {name : string; target : Raw_http.target}

let targets = ref []
let selected = ref []
let files = ref []
let first_byte = ref 2.
let idle = ref 0.5
let settle = ref None
let underscore_names = ref []
let ignored = ref []

let parse_target s =
  let bad () = raise (Arg.Bad ("invalid --target " ^ s)) in
  match String.index_opt s '=' with
  | None -> bad ()
  | Some i -> (
      let name = String.sub s 0 i in
      let address = String.sub s (i + 1) (String.length s - i - 1) in
      let transport, address =
        if String.starts_with ~prefix:"tls:" address
        then
          ( Raw_http.Tls {max_version = Tls_1_3; sni = None}
          , String.sub address 4 (String.length address - 4) )
        else Raw_http.Plain, address
      in
      if List.exists (fun t -> t.name = name) !targets
      then raise (Arg.Bad ("two targets named " ^ name));
      match String.rindex_opt address ':' with
      | Some j when name <> "" -> (
          let host = String.sub address 0 j in
          let host =
            let n = String.length host in
            if n >= 2 && host.[0] = '[' && host.[n - 1] = ']'
            then String.sub host 1 (n - 2)
            else host
          in
          match
            int_of_string_opt
              (String.sub address (j + 1) (String.length address - j - 1))
          with
          | Some port when host <> "" && port > 0 ->
              targets := !targets @ [{name; target = {host; port; transport}}]
          | Some _ | None -> bad ()
        )
      | Some _ | None -> bad ()
    )

let options =
  Arg.align
    [ "--target", Arg.String parse_target, "NAME=[tls:]HOST:PORT A server"
    ; ( "--case"
      , Arg.String (fun name -> selected := name :: !selected)
      , "FILE/NAME Only this case" )
    ; ( "--first-byte"
      , Arg.Set_float first_byte
      , "SECONDS Wait for the first byte of an answer (default 2)" )
    ; ( "--idle"
      , Arg.Set_float idle
      , "SECONDS Stop reading after that long without data (default 0.5)" )
    ; ( "--underscore-names"
      , Arg.String (fun name -> underscore_names := name :: !underscore_names)
      , "NAME The server NAME writes the hyphens of header names as \
         underscores (WSGI, CGI)" )
    ; ( "--ignore-field"
      , Arg.String (fun field -> ignored := field :: !ignored)
      , "FIELD Compare the requests without this header field, which a server \
         adds or removes itself" )
    ; ( "--settle"
      , Arg.Float (fun s -> settle := Some s)
      , "SECONDS The same, after complete responses that keep the connection \
         open (default: as --idle)" )
    ]

let fail message =
  prerr_endline ("httpdiff: " ^ message);
  exit 2

let load_cases () =
  let cases =
    List.concat_map
      (fun file ->
        let stem = Filename.remove_extension (Filename.basename file) in
        match Corpus.load file with
        | Ok cases ->
            List.map
              (fun case ->
                {case with Corpus.name = stem ^ "/" ^ case.Corpus.name}
              )
              cases
        | Error message -> fail message
      )
      (List.rev !files)
  in
  List.iter
    (fun name ->
      if not (List.exists (fun {Corpus.name = n; _} -> n = name) cases)
      then fail ("no case named " ^ name)
    )
    !selected;
  List.filter
    (fun {Corpus.name; _} -> !selected = [] || List.mem name !selected)
    cases

type result = Outcome of Outcome.t | Failed of string

(* Every server is asked at the same time, each on its own connection: the
   time of a case is the one of the slowest server. *)
let ask input =
  (* Each thread replaces its element, whatever happens to its exchange. *)
  let results = Array.make (List.length !targets) (Failed "not asked") in
  let threads =
    List.mapi
      (fun i {target; _} ->
        Thread.create
          (fun () ->
            results.(i) <-
              ( match
                  Raw_http.exchange ~first_byte:!first_byte ~idle:!idle
                    ?settle:!settle target input
                with
              | outcome -> Outcome (Outcome.of_outcome outcome)
              | exception Raw_http.Connect_failed message -> Failed message
              (* Any other failure is reported as such, not lost with the
                 thread. *)
              | exception e -> Failed (Printexc.to_string e)
              )
          )
          ()
      )
      !targets
  in
  List.iter Thread.join threads;
  List.mapi (fun i {name; _} -> name, results.(i)) !targets

let show () =
  List.iter
    (fun {Corpus.name; input} ->
      Printf.printf "== %s\n" name;
      List.iter
        (fun (target, result) ->
          Printf.printf "-- %s\n%s" target
            ( match result with
            | Outcome outcome -> Outcome.to_string outcome
            | Failed message -> Printf.sprintf "[failed: %s]\n" message
            )
        )
        (ask input)
    )
    (load_cases ())

(* The number of cases with disagreements, and with failures. *)
let run () =
  let cases = load_cases () in
  let disagreements, failures =
    List.fold_left
      (fun (disagreements, failures) {Corpus.name; input} ->
        let results = ask input in
        let failed =
          List.filter_map
            (function name, Failed message -> Some (name, message) | _ -> None)
            results
        in
        let outcomes =
          List.filter_map
            (function
              | name, Outcome o ->
                  Some
                    ( name
                    , { Outcome.underscore_names =
                          List.mem name !underscore_names
                      }
                    , o
                    )
              | _, Failed _ -> None
              )
            results
        in
        List.iter
          (fun (target, message) ->
            Printf.printf "== %s: %s: [failed: %s]\n" name target message
          )
          failed;
        let failures = if failed = [] then failures else failures + 1 in
        match Outcome.clusters ~ignored:!ignored outcomes with
        | [_] when List.length outcomes >= 2 ->
            Printf.printf "== %s: agree\n" name;
            disagreements, failures
        | [] | [_] ->
            Printf.printf "== %s: not compared\n" name;
            disagreements, failures
        | clusters ->
            Printf.printf "== %s: %d groups\n" name (List.length clusters);
            List.iter
              (fun (names, view) ->
                Printf.printf "  %s: %s\n" (String.concat ", " names)
                  (Outcome.view_to_string view)
              )
              clusters;
            disagreements + 1, failures
      )
      (0, 0) cases
  in
  Printf.printf "%d cases, %d with disagreements, %d with failures\n"
    (List.length cases) disagreements failures;
  if failures > 0 then exit 1

let usage = "usage: httpdiff (show|run) [options] FILE..."

let () =
  match Array.to_list Sys.argv with
  | _ :: (("show" | "run") as command) :: _ -> (
      let argv = Array.sub Sys.argv 1 (Array.length Sys.argv - 1) in
      match
        Arg.parse_argv argv options
          (fun file -> files := file :: !files)
          ("usage: httpdiff " ^ command ^ " [options] FILE...")
      with
      | () when !targets = [] || !files = [] ->
          fail "give at least one --target and one FILE"
      | () ->
          Raw_http.init ();
          if command = "show" then show () else run ()
      | exception Arg.Help message -> print_string message
      | exception Arg.Bad message -> prerr_string message; exit 2
    )
  | [_; ("-help" | "--help")] -> print_endline usage
  | _ -> prerr_endline usage; exit 2
