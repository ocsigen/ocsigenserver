(* The reporter of the syslog mode formats each message once, with no lock
   held, for syslog and for stderr: a printer runs once, and a tag printer may
   log. The messages must reach syslog as the [Logs_syslog] reporter alone
   would send them, and stderr as in the other modes. A Unix-domain datagram
   socket plays the syslog daemon. *)

let dir = Log_test.temp_dir ()
let path = Filename.concat dir "log"

let daemon =
  let socket = Unix.socket Unix.PF_UNIX Unix.SOCK_DGRAM 0 in
  Unix.bind socket (Unix.ADDR_UNIX path);
  Unix.setsockopt_float socket Unix.SO_RCVTIMEO 10.;
  socket

let receive () =
  let buffer = Bytes.create 65536 in
  match Unix.recv daemon buffer 0 (Bytes.length buffer) [] with
  | n -> Bytes.sub_string buffer 0 n
  | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
      Log_test.fail "no syslog message received"

(* A local syslog message is ["<priority>Mmm dd hh:mm:ss ..."]: the date is
   removed, as two messages may be sent at different seconds. *)
let without_date message =
  let start = String.index message '>' + 1 and date = 15 in
  String.sub message 0 start
  ^ String.sub message (start + date) (String.length message - start - date)

(* [captured_stderr f] runs [f ()] with [stderr] redirected to a file, and is
   its result and what [f] wrote there. *)
let captured_stderr f =
  let file = Filename.concat dir "stderr" in
  let fd =
    Unix.openfile file [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o600
  in
  let saved = Unix.dup Unix.stderr in
  flush stderr;
  Unix.dup2 fd Unix.stderr;
  Unix.close fd;
  let result =
    Fun.protect
      ~finally:(fun () ->
        flush stderr;
        Unix.dup2 saved Unix.stderr;
        Unix.close saved)
      f
  in
  let printed = Log_test.read dir "stderr" in
  Sys.remove file; result, printed

let syslog =
  match Logs_syslog_unix.unix_reporter ~socket:path () with
  | Ok reporter -> reporter
  | Error msg -> Log_test.fail "cannot connect to the syslog socket: %s" msg

let reporter = Ocsigen.Messages.syslog_reporter syslog
let src = Logs.Src.create "test:reporter-syslog"
let user = Logs.Tag.def "user" Format.pp_print_string

(* The line of a message on stderr is
   ["<date>: test:reporter-syslog: <message>"]: [after_source printed] is
   [printed] from [<message>] on, and the column where [<message>] starts. *)
let after_source printed =
  let source = ": test:reporter-syslog: " in
  let rec find i =
    if i + String.length source > String.length printed
    then Log_test.fail "no log line on stderr: %S" printed
    else if String.sub printed i (String.length source) = source
    then i + String.length source
    else find (i + 1)
  in
  let start = find 0 in
  String.sub printed start (String.length printed - start), start

(* [check_message log expected_stderr] checks that the message that [log]
   sends to a reporter reaches syslog as with [Logs_syslog] alone, and is
   printed on stderr as [expected_stderr column], [column] being where the
   header starts. *)
let check_message log expected_stderr =
  log syslog;
  let expected = without_date (receive ()) in
  let (), printed = captured_stderr (fun () -> log reporter) in
  let message = without_date (receive ()) in
  if message <> expected
  then Log_test.fail "syslog message\n%s\ninstead of\n%s" message expected;
  let printed, column = after_source printed in
  let expected = expected_stderr column in
  if printed <> expected
  then Log_test.fail "stderr: %S instead of %S" printed expected

let message ?header ?tags reporter =
  reporter.Logs.report src Logs.Warning ~over:ignore Fun.id (fun m ->
    m ?header ?tags "message %d" 42)

let () =
  let facility = Logs.Tag.(add Logs_syslog.facility Syslog_message.Local0) in
  let someone = Logs.Tag.(add user "someone") in
  check_message message (fun _ -> "[WARNING] message 42\n");
  check_message (message ~header:"header") (fun _ -> "[header] message 42\n");
  check_message
    (message ~tags:(someone Logs.Tag.empty))
    (fun _ -> "[WARNING] message 42\n");
  check_message
    (message ~header:"header" ~tags:(facility (someone Logs.Tag.empty)))
    (fun _ -> "[header] message 42\n");
  check_message
    (message ~tags:(facility Logs.Tag.empty))
    (fun _ -> "[WARNING] message 42\n")

(* The lines after the first one of a message are indented to its box on
   stderr, and not in syslog, whether they come from a forced newline or from
   a newline printed by [%s]. *)
let () =
  let check log =
    check_message log (fun column ->
      let header = "[WARNING] " in
      header ^ "first line\n"
      ^ String.make (column + String.length header) ' '
      ^ "second line\n")
  in
  check (fun reporter ->
    reporter.Logs.report src Logs.Warning ~over:ignore Fun.id (fun m ->
      m "first line@\nsecond line"));
  check (fun reporter ->
    reporter.Logs.report src Logs.Warning ~over:ignore Fun.id (fun m ->
      m "%s" "first line\nsecond line"))

(* The empty lines of a message, a final newline included, stay empty on
   stderr. *)
let () =
  check_message
    (fun reporter ->
       reporter.Logs.report src Logs.Warning ~over:ignore Fun.id (fun m ->
         m "%s" "first line\n\nthird line\n"))
    (fun column ->
       let header = "[WARNING] " in
       header ^ "first line\n\n"
       ^ String.make (column + String.length header) ' '
       ^ "third line\n\n")

(* A header wider than the maximal indentation of Format does not put the
   message on a line of its own. *)
let () =
  let header = String.make 70 'h' in
  check_message (message ~header) (fun _ -> "[" ^ header ^ "] message 42\n")

(* The break hints of a message are laid out from column 0, as with
   [Logs_syslog] alone, whatever the width of the prefix. On stderr, the
   lines after the first one are indented. *)
let () =
  let words = String.concat " " (List.init 40 (Printf.sprintf "word%d")) in
  check_message
    (fun reporter ->
       reporter.Logs.report src Logs.Warning ~over:ignore Fun.id (fun m ->
         m "%a" Format.pp_print_text words))
    (fun column ->
       let header = "[WARNING] " in
       let indentation = String.make (column + String.length header) ' ' in
       header
       ^ String.concat ("\n" ^ indentation)
           (String.split_on_char '\n'
              (Format.asprintf "%a" Format.pp_print_text words))
       ^ "\n")

let () =
  Logs.set_reporter reporter;
  let count = ref 0 in
  let pp_counted ppf () =
    incr count;
    Format.pp_print_string ppf "counted"
  in
  let (), printed =
    captured_stderr (fun () ->
      Logs.warn ~src (fun m -> m "message %a" pp_counted ()))
  in
  let received = receive () in
  if
    not
      (!count = 1
      && Log_test.contains received "message counted"
      && Log_test.contains printed "message counted")
  then
    Log_test.fail "printer called %d times\nsyslog: %s\nstderr: %s" !count
      received printed

(* A message reaches syslog even if writing to stderr fails: here, stderr is
   a pipe that nobody reads. Writing does not raise, and the error is logged,
   so it reaches syslog after the message. It runs within [captured_stderr],
   so that what [stderr] may still buffer afterwards is written to the file. *)
let () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let raised, (_ : string) =
    captured_stderr (fun () ->
      let reader, writer = Unix.pipe () in
      Unix.close reader;
      let saved = Unix.dup Unix.stderr in
      Unix.dup2 writer Unix.stderr;
      Unix.close writer;
      Fun.protect
        ~finally:(fun () ->
          Unix.dup2 saved Unix.stderr;
          Unix.close saved)
        (fun () ->
           match
             reporter.Logs.report src Logs.Warning ~over:ignore Fun.id (fun m ->
               m "lost on stderr")
           with
           | () -> false
           | exception Sys_error _ -> true))
  in
  let received = receive () in
  let error = receive () in
  if
    raised
    || not
         (Log_test.contains received "lost on stderr"
         && Log_test.contains error "Cannot write to the standard error")
  then
    Log_test.fail "writing to stderr %s\nsyslog: %s\n%s"
      (if raised then "raised" else "did not raise")
      received error

let () =
  let logging =
    Logs.Tag.def "logging" (fun ppf () ->
      Logs.warn ~src (fun m -> m "inner message");
      Format.pp_print_string ppf "printed")
  in
  let result, printed =
    captured_stderr (fun () ->
      Log_test.try_on_other_thread (fun () ->
        Logs.warn ~src (fun m ->
          m ~tags:Logs.Tag.(add logging () empty) "outer message")))
  in
  Result.iter_error
    (fun msg -> Log_test.fail "%s\nstderr: %s" msg printed)
    result;
  let inner = receive () in
  let outer = receive () in
  if
    not
      (Log_test.contains inner "inner message"
      && Log_test.contains outer "printed"
      && Log_test.contains outer "outer message")
  then Log_test.fail "unexpected syslog messages\n%s\n%s" inner outer;
  (match String.split_on_char '\n' printed with
  | [inner; outer; ""]
    when Log_test.contains inner "inner message"
         && Log_test.contains outer "outer message" ->
      ()
  | _ -> Log_test.fail "unexpected lines on stderr\n%s" printed);
  Unix.close daemon; Sys.remove path; Sys.rmdir dir
