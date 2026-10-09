(* An application may install its own reporter mutex, here a plain
   (non-reentrant) [Mutex.t]. Logs takes it before calling the reporters that
   [Ocsigen.Messages.open_files] installs, which then take the locks of their
   destinations: they must work together, also when formatting a message
   raises, which must leave no lock taken. *)

let () =
  let dir = Log_test.open_log_dir () in
  let mutex = Mutex.create () in
  Logs.set_reporter_mutex
    ~lock:(fun () -> Mutex.lock mutex)
    ~unlock:(fun () -> Mutex.unlock mutex);
  let src = Logs.Src.create "test:reporter-app-mutex" in
  Logs.warn ~src (fun m -> m "first message");
  let pp_raising _ () = raise Exit in
  (match Logs.warn ~src (fun m -> m "raising message %a" pp_raising ()) with
  | () -> Log_test.fail "the raising printer did not raise"
  | exception Exit -> ());
  Log_test.on_other_thread (fun () ->
    Logs.warn ~src (fun m -> m "message from a thread");
    Ocsigen.Messages.accesslog "access line from a thread");
  Log_test.check_file dir "warnings.log"
    ~present:["first message"; "message from a thread"]
    ~absent:[];
  Log_test.check_file dir "access.log"
    ~present:["access line from a thread"]
    ~absent:[];
  Log_test.remove_log_dir dir
