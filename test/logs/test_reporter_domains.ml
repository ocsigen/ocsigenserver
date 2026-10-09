(* Several domains log at the same time through the reporters that
   [Ocsigen.Messages.open_files] installs, and write access lines. Every line
   of the files is one whole message, and no message is lost. *)

let domains = 4
let messages = 500

let () =
  let dir = Log_test.open_log_dir () in
  let src = Logs.Src.create "test:reporter-domains" in
  let in_parallel write =
    List.init domains (fun d ->
      Domain.spawn (fun () ->
        for i = 1 to messages do
          write d i
        done))
    |> List.iter Domain.join
  in
  in_parallel (fun d i ->
    Logs.warn ~src (fun m -> m "domain %d message %d" d i));
  in_parallel (fun d i ->
    Ocsigen.Messages.accesslog (Printf.sprintf "access %d %d" d i));
  let check file parse =
    Log_test.check_lines ~what:file ~writers:domains ~messages parse
      (Log_test.lines dir file)
  in
  check "warnings.log" (fun line ->
    Scanf.sscanf line "%_s@] domain %d message %d%!" (fun d i -> d, i));
  check "access.log" (fun line ->
    Scanf.sscanf line "access %d %d%!" (fun d i -> d, i));
  Log_test.remove_log_dir dir
