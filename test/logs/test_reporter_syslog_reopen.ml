(* Reopening the logs (the reopen_logs order of a log rotation) in syslog
   mode must not open a new socket to syslog each time: Logs_syslog_unix gives
   no way to close a reporter, so each new one would leak its socket. Counted
   in /proc/self/fd, so this runs on Linux with a syslog daemon listening at
   /dev/log, and is skipped elsewhere. *)

let count_fds () = Array.length (Sys.readdir "/proc/self/fd")
let skip reason = Printf.eprintf "test skipped: %s\n" reason

let () =
  if not (Sys.file_exists "/proc/self/fd" && Sys.file_exists "/dev/log")
  then skip "needs /proc/self/fd and /dev/log"
  else (
    Ocsigen.Config.set_syslog_facility (Some Syslog_message.Local0);
    match Lwt_main.run (Ocsigen.Messages.open_files ()) with
    | exception Failure msg ->
        skip ("no syslog daemon at /dev/log: " ^ String.trim msg)
    | () ->
        let before = count_fds () in
        for _ = 1 to 10 do
          Lwt_main.run (Ocsigen.Messages.open_files ())
        done;
        let after = count_fds () in
        if after > before
        then (
          Printf.eprintf "file descriptors: %d before, %d after 10 reopenings\n"
            before after;
          exit 1))
