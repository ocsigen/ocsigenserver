(* Command line client for black-box tests of an HTTP server.

   rawhttp send [options] [FILE]    send the raw request in FILE (default:
                                    standard input), print the answer
   rawhttp repeat -n N [options] [FILE]
                                    send the request N times, one connection
                                    each, print a summary of the answers
   rawhttp hold -n N [options] [FILE]
                                    open N connections, send FILE on each and
                                    keep them open, then check that a fresh
                                    request is still answered
   rawhttp wait [options]           wait until the server accepts connections

   See the usage message of each command for its options. *)

let host = ref "127.0.0.1"
let port = ref 0
let tls = ref false
let tls_max = ref Raw_http.Tls_1_3
let sni = ref None
let idle = ref 2.
let trickle = ref None
let status_only = ref false
let validators = ref false
let count = ref 1
let file = ref None

let target () =
  if !port <= 0 then raise (Arg.Bad "--port is required");
  let transport =
    if !tls
    then Raw_http.Tls {max_version = !tls_max; sni = !sni}
    else Raw_http.Plain
  in
  {Raw_http.host = !host; port = !port; transport}

let parse_trickle s =
  match String.split_on_char ':' s with
  | [size; delay] -> (
    match int_of_string_opt size, float_of_string_opt delay with
    | Some size, Some delay when size > 0 && delay >= 0. ->
        trickle := Some (size, delay)
    | _ -> raise (Arg.Bad ("invalid --trickle " ^ s)))
  | _ -> raise (Arg.Bad ("invalid --trickle " ^ s))

let parse_tls_max = function
  | "1.2" -> tls_max := Raw_http.Tls_1_2
  | "1.3" -> tls_max := Raw_http.Tls_1_3
  | s -> raise (Arg.Bad ("invalid --tls-max " ^ s))

let target_options =
  [ "--host", Arg.Set_string host, "HOST Server address (default 127.0.0.1)"
  ; "--port", Arg.Set_int port, "PORT Server port (required)"
  ; "--tls", Arg.Set tls, " Use TLS (no certificate verification)"
  ; ( "--tls-max"
    , Arg.String parse_tls_max
    , "VERSION Highest TLS version offered: 1.2 or 1.3 (default 1.3)" )
  ; ( "--sni"
    , Arg.String (fun s -> sni := Some s)
    , "NAME Server name sent in the TLS handshake" ) ]

let exchange_options =
  [ ( "--idle"
    , Arg.Set_float idle
    , "SECONDS Stop reading after that long without data (default 2)" )
  ; ( "--trickle"
    , Arg.String parse_trickle
    , "N:SECONDS Send N bytes at a time, pausing between pieces" ) ]

let count_option =
  ["-n", Arg.Set_int count, "N Number of connections or requests"]

let read_request () =
  match !file with
  | None -> In_channel.input_all stdin
  | Some path -> In_channel.with_open_bin path In_channel.input_all

let connect_failed message =
  Printf.printf "[connect failed: %s]\n" message;
  exit 1

let send () =
  let request = read_request () in
  match Raw_http.exchange ?trickle:!trickle ~idle:!idle (target ()) request with
  | outcome ->
      print_string
        (Raw_http.render ~status_only:!status_only ~validators:!validators
           outcome)
  | exception Raw_http.Connect_failed message -> connect_failed message

let ending_name = function
  | Raw_http.Closed -> "closed"
  | Reset -> "reset"
  | Idle -> "idle"
  | Overflow -> "overflow"

(* One line per distinct answer, in order of first appearance:
   "<count> x <status codes> [<ending>]". *)
let repeat () =
  let request = read_request () in
  let target = target () in
  let summary = Hashtbl.create 8 in
  let order = ref [] in
  for _ = 1 to !count do
    let key =
      match Raw_http.exchange ?trickle:!trickle ~idle:!idle target request with
      | {received; ending} ->
          let codes = Raw_http.status_codes received in
          Printf.sprintf "%s [%s]"
            (if codes = []
             then "-"
             else String.concat "," (List.map string_of_int codes))
            (ending_name ending)
      | exception Raw_http.Connect_failed message ->
          Printf.sprintf "[connect failed: %s]" message
    in
    match Hashtbl.find_opt summary key with
    | Some n -> Hashtbl.replace summary key (n + 1)
    | None ->
        Hashtbl.add summary key 1;
        order := key :: !order
  done;
  List.iter
    (fun key -> Printf.printf "%d x %s\n" (Hashtbl.find summary key) key)
    (List.rev !order)

let probe_request = "GET / HTTP/1.1\r\nHost: probe\r\nConnection: close\r\n\r\n"

(* Connections that cannot be opened are counted, not fatal: running out of
   connections is one of the behaviours under test. *)
let hold () =
  let request = read_request () in
  let target = target () in
  let rec open_all n held refused =
    if n = 0
    then List.rev held, refused
    else
      match Raw_http.connect target with
      | conn ->
          let (_ : bool) = Raw_http.send conn request in
          open_all (n - 1) (conn :: held) refused
      | exception Raw_http.Connect_failed _ ->
          open_all (n - 1) held (refused + 1)
  in
  let held, refused = open_all !count [] 0 in
  Printf.printf "held %d connections, %d refused\n" (List.length held) refused;
  (match Raw_http.exchange ~idle:!idle target probe_request with
  | outcome ->
      print_string "probe: ";
      print_string (Raw_http.render ~status_only:true outcome)
  | exception Raw_http.Connect_failed message ->
      Printf.printf "probe: [connect failed: %s]\n" message);
  List.iter Raw_http.close held

let wait () =
  let target = target () in
  let deadline = Unix.gettimeofday () +. !idle in
  let rec attempt () =
    match Raw_http.connect target with
    | conn -> Raw_http.close conn
    | exception Raw_http.Connect_failed message ->
        if Unix.gettimeofday () > deadline
        then connect_failed message
        else (Unix.sleepf 0.05; attempt ())
  in
  attempt ()

let commands =
  [ ( "send"
    , send
    , target_options @ exchange_options
      @ [ "--status", Arg.Set status_only, " Only print the status lines"
        ; ( "--validators"
          , Arg.Set validators
          , " Keep the ETag and Last-Modified header lines" ) ] )
  ; "repeat", repeat, target_options @ exchange_options @ count_option
  ; "hold", hold, target_options @ count_option @ exchange_options
  ; ( "wait"
    , wait
    , target_options
      @ [ ( "--idle"
          , Arg.Set_float idle
          , "SECONDS Give up after that long (default 2)" ) ] ) ]

let usage = "usage: rawhttp (send|repeat|hold|wait) [options] [FILE]"

let () =
  match Array.to_list Sys.argv with
  | _ :: name :: _ -> (
    match List.find_opt (fun (n, _, _) -> n = name) commands with
    | None -> prerr_endline usage; exit 2
    | Some (_, run, options) -> (
        let argv = Array.sub Sys.argv 1 (Array.length Sys.argv - 1) in
        (try
           Arg.parse_argv argv (Arg.align options)
             (fun path -> file := Some path)
             (Printf.sprintf "usage: rawhttp %s [options] [FILE]" name)
         with
        | Arg.Help message -> print_string message; exit 0
        | Arg.Bad message -> prerr_string message; exit 2);
        try run () with Arg.Bad message -> prerr_endline message; exit 2))
  | _ -> prerr_endline usage; exit 2
