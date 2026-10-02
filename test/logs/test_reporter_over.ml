(* The reporters installed by [Ocsigen.Messages.open_files] must call [over]
   exactly once per message, as Logs requires: [Logs.set_reporter_mutex]
   releases its lock in [over], so a second call unlocks a free mutex (and
   OCaml 5's error-checking [Mutex.unlock] raises [Sys_error]). The lock is
   modelled by a counter that must never go negative. *)

let depth = ref 0
let lock () = incr depth

let unlock () =
  decr depth;
  if !depth < 0 then failwith "over called more than once for a message"

let fail fmt = Printf.ksprintf (fun s -> prerr_endline s; exit 1) fmt

let make_log_dir () =
  let dir = Filename.temp_file "ocsigenserver-logs" "" in
  Sys.remove dir; Sys.mkdir dir 0o700; dir

let contains s sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s && (String.sub s i n = sub || go (i + 1))
  in
  go 0

let check_file dir file ~present ~absent =
  let path = Filename.concat dir file in
  let contents = In_channel.with_open_text path In_channel.input_all in
  List.iter
    (fun msg -> if not (contains contents msg) then fail "%s lacks %S" file msg)
    present;
  List.iter
    (fun msg -> if contains contents msg then fail "%s contains %S" file msg)
    absent

let () =
  let dir = make_log_dir () in
  Ocsigen.Config.set_logdir dir;
  Ocsigen.Config.set_silent ();
  Lwt_main.run (Ocsigen.Messages.open_files ());
  Logs.set_reporter_mutex ~lock ~unlock;
  Logs.set_level (Some Logs.Debug);
  let src = Logs.Src.create "test:reporter-over" in
  Logs.debug ~src (fun m -> m "debug message");
  Logs.info ~src (fun m -> m "info message");
  Logs.app ~src (fun m -> m "app message");
  Logs.warn ~src (fun m -> m "warning message");
  Logs.err ~src (fun m -> m "error message");
  if !depth <> 0 then fail "unbalanced reporter lock: depth %d" !depth;
  let others = ["debug message"; "info message"; "app message"] in
  check_file dir "errors.log" ~present:["error message"]
    ~absent:("warning message" :: others);
  check_file dir "warnings.log" ~present:["warning message"]
    ~absent:("error message" :: others);
  List.iter
    (fun file -> Sys.remove (Filename.concat dir file))
    ["access.log"; "warnings.log"; "errors.log"];
  Sys.rmdir dir
