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

let () =
  let dir = Log_test.open_log_dir () in
  Logs.set_reporter_mutex ~lock ~unlock;
  Logs.set_level (Some Logs.Debug);
  let src = Logs.Src.create "test:reporter-over" in
  Logs.debug ~src (fun m -> m "debug message");
  Logs.info ~src (fun m -> m "info message");
  Logs.app ~src (fun m -> m "app message");
  Logs.warn ~src (fun m -> m "warning message");
  Logs.err ~src (fun m -> m "error message");
  if !depth <> 0 then Log_test.fail "unbalanced reporter lock: depth %d" !depth;
  let others = ["debug message"; "info message"; "app message"] in
  Log_test.check_file dir "errors.log" ~present:["error message"]
    ~absent:("warning message" :: others);
  Log_test.check_file dir "warnings.log" ~present:["warning message"]
    ~absent:("error message" :: others);
  Log_test.remove_log_dir dir
