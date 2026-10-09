(* A printer may take a lock of the application while another thread holds
   that lock and logs. The reporters that [Ocsigen.Messages.open_files]
   installs must not hold a lock of their own while a message is formatted,
   or the two threads deadlock. *)

let () =
  let dir = Log_test.open_log_dir () in
  let src = Logs.Src.create "test:reporter-printer-lock" in
  let app_mutex = Mutex.create () in
  let in_printer = Atomic.make false in
  let pp_locked ppf () =
    Atomic.set in_printer true;
    Mutex.lock app_mutex;
    Mutex.unlock app_mutex;
    Format.pp_print_string ppf "printed"
  in
  Mutex.lock app_mutex;
  let printing =
    Thread.create
      (fun () -> Logs.warn ~src (fun m -> m "locked message %a" pp_locked ()))
      ()
  in
  Log_test.wait_for "the printer did not start" (fun () ->
    if Atomic.get in_printer then Some () else None);
  (* The printer is formatting its message and waits for [app_mutex]. *)
  Log_test.on_other_thread (fun () ->
    Logs.warn ~src (fun m -> m "message while the printer waits"));
  Mutex.unlock app_mutex;
  Thread.join printing;
  Log_test.check_file dir "warnings.log"
    ~present:["locked message printed"; "message while the printer waits"]
    ~absent:[];
  Log_test.remove_log_dir dir
