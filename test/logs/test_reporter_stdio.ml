(* Several domains log at the same time through the reporter of the serve
   mode ([Ocsigen.Messages.open_files] with [log_to_stderr]): warnings to
   [stderr], other messages and access lines to [stdout]. Every line is one
   whole message, and no message is lost. *)

let domains = 4
let messages = 500

(* [redirect fd path] makes [fd] write to the file [path], and is a copy of
   the former [fd], to restore it. *)
let redirect fd path =
  let saved = Unix.dup fd in
  let file =
    Unix.openfile path [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o600
  in
  Unix.dup2 file fd; Unix.close file; saved

let restore fd saved = Unix.dup2 saved fd; Unix.close saved

let () =
  let dir = Log_test.temp_dir () in
  Ocsigen.Config.set_log_to_stderr true;
  let saved_stdout = redirect Unix.stdout (Filename.concat dir "stdout") in
  let saved_stderr = redirect Unix.stderr (Filename.concat dir "stderr") in
  Lwt_main.run (Ocsigen.Messages.open_files ());
  let src = Logs.Src.create "test:reporter-stdio" in
  List.init domains (fun d ->
    Domain.spawn (fun () ->
      for i = 1 to messages do
        Logs.warn ~src (fun m -> m "domain %d warning %d" d i);
        Logs.app ~src (fun m -> m "notice|%d|%d" d i);
        Ocsigen.Messages.accesslog (Printf.sprintf "access %d %d" d i)
      done))
  |> List.iter Domain.join;
  flush stdout;
  flush stderr;
  restore Unix.stdout saved_stdout;
  restore Unix.stderr saved_stderr;
  let check what parse lines =
    Log_test.check_lines ~what ~writers:domains ~messages parse lines
  in
  check "stderr"
    (fun line ->
       Scanf.sscanf line "%_s@] domain %d warning %d%!" (fun d i -> d, i))
    (Log_test.lines dir "stderr");
  let access, notices =
    List.partition
      (String.starts_with ~prefix:"access ")
      (Log_test.lines dir "stdout")
  in
  check "stdout notices"
    (fun line -> Scanf.sscanf line "%_s@|%d|%d%!" (fun d i -> d, i))
    notices;
  check "stdout access lines"
    (fun line -> Scanf.sscanf line "access %d %d%!" (fun d i -> d, i))
    access;
  Log_test.remove_log_dir dir
