(* Reopening the logs (the reopen_logs order of a log rotation) in syslog
   mode must not open a new socket to syslog each time: Logs_syslog_unix gives
   no way to close a reporter, so each new one would leak its socket. Counted
   in /proc/self/fd, so this runs on Linux with a syslog socket at /dev/log,
   and does nothing elsewhere. *)

let count_fds () = Array.length (Sys.readdir "/proc/self/fd")

let () =
  if Sys.file_exists "/proc/self/fd" && Sys.file_exists "/dev/log"
  then (
    Ocsigen.Config.set_syslog_facility (Some Syslog_message.Local0);
    Lwt_main.run (Ocsigen.Messages.open_files ());
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
