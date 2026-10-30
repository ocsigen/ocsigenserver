(* Writing a log line fails, as on a full disk: in the serve mode
   ([Ocsigen.Messages.open_files] with [log_to_stderr]), [stdout] is made
   read-only. Logging and writing access lines must not raise. The error is
   logged on [stderr] once, then again only after a write has succeeded. *)

(* [redirect fd path flags] makes [fd] use the file [path] opened with
   [flags], and is a copy of the former [fd], to restore it. *)
let redirect fd path flags =
  let saved = Unix.dup fd in
  let file = Unix.openfile path flags 0o600 in
  Unix.dup2 file fd; Unix.close file; saved

let restore fd saved = Unix.dup2 saved fd; Unix.close saved

(* [failing_stdout dir f] runs [f ()] with [stdout] read-only, so that
   writing to it fails. *)
let failing_stdout dir f =
  let path = Filename.concat dir "read-only" in
  close_out (open_out path);
  let saved = redirect Unix.stdout path [Unix.O_RDONLY] in
  Fun.protect ~finally:(fun () -> restore Unix.stdout saved) f

let write_all src n =
  Logs.app ~src (fun m -> m "notice %d" n);
  Ocsigen.Messages.accesslog (Printf.sprintf "access %d" n)

let () =
  let dir = Log_test.temp_dir () in
  Ocsigen.Config.set_log_to_stderr true;
  let saved_stdout =
    redirect Unix.stdout
      (Filename.concat dir "stdout")
      [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC]
  in
  let saved_stderr =
    redirect Unix.stderr
      (Filename.concat dir "stderr")
      [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC]
  in
  Ocsigen.Messages.open_files ();
  let src = Logs.Src.create "test:reporter-write-error" in
  let result =
    match
      failing_stdout dir (fun () -> write_all src 1; write_all src 2);
      write_all src 3;
      failing_stdout dir (fun () -> write_all src 4)
    with
    | () -> Ok ()
    | exception exn -> Error exn
  in
  flush stdout;
  flush stderr;
  restore Unix.stdout saved_stdout;
  restore Unix.stderr saved_stderr;
  (match result with
  | Ok () -> ()
  | Error exn -> Log_test.fail "logging raised %s" (Printexc.to_string exn));
  let errors =
    List.filter
      (fun line -> Log_test.contains line "Cannot write to the standard output")
      (Log_test.lines dir "stderr")
  in
  if List.length errors <> 2
  then
    Log_test.fail "expected 2 write errors on stderr, got:\n%s"
      (String.concat "\n" errors);
  Log_test.check_file dir "stdout" ~present:["notice 3"; "access 3"] ~absent:[];
  Log_test.remove_log_dir dir
