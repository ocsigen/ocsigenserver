(* The logs are reopened (on log rotation, for instance) while a domain logs
   and writes access lines. No exception may reach the domain, and no line may
   be lost: each one is written to the old files or to the new ones. *)

let messages = 2000

let () =
  let dir = Log_test.open_log_dir () in
  let src = Logs.Src.create "test:reporter-reopen" in
  let finished = Atomic.make false in
  let logging =
    Domain.spawn (fun () ->
      Fun.protect
        ~finally:(fun () -> Atomic.set finished true)
        (fun () ->
           for i = 1 to messages do
             Logs.warn ~src (fun m -> m "message %d" i);
             Ocsigen.Messages.accesslog (Printf.sprintf "access %d" i)
           done))
  in
  let reopened = ref 0 in
  while not (Atomic.get finished) do
    Lwt_main.run (Ocsigen.Messages.open_files ());
    incr reopened
  done;
  (match Domain.join logging with
  | () -> ()
  | exception exn ->
      Log_test.fail "logging raised %s after %d reopenings"
        (Printexc.to_string exn) !reopened);
  let check file parse =
    Log_test.check_lines ~what:file ~writers:1 ~messages parse
      (Log_test.lines dir file)
  in
  check "warnings.log" (fun line ->
    Scanf.sscanf line "%_s@] message %d%!" (fun i -> 0, i));
  check "access.log" (fun line ->
    Scanf.sscanf line "access %d%!" (fun i -> 0, i));
  Log_test.remove_log_dir dir
