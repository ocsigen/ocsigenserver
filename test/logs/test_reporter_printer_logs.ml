(* A printer may log: the reporters that [Ocsigen.Messages.open_files]
   installs must neither deadlock nor raise, and each message must be a line
   of its own, also when both messages go to the same file. *)

let () =
  let dir = Log_test.open_log_dir () in
  let src = Logs.Src.create "test:reporter-printer-logs" in
  let pp_logging ppf () =
    Logs.warn ~src (fun m -> m "inner message");
    Format.pp_print_string ppf "printed"
  in
  Logs.warn ~src (fun m -> m "outer message %a" pp_logging ());
  Log_test.on_other_thread (fun () ->
    Logs.warn ~src (fun m -> m "message from a thread"));
  let ends_with suffix line = String.ends_with ~suffix line in
  match Log_test.lines dir "warnings.log" with
  | [inner; outer; other]
    when ends_with "] inner message" inner
         && ends_with "] outer message printed" outer
         && ends_with "] message from a thread" other ->
      Log_test.remove_log_dir dir
  | lines ->
      Log_test.fail "warnings.log: unexpected lines\n%s"
        (String.concat "\n" lines)
