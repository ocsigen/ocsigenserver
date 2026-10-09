(* The reporter of the syslog mode formats the tags of a message with no lock
   held, so a tag printer may log. The messages must reach syslog as the
   [Logs_syslog] reporter alone would send them. A Unix-domain datagram socket
   plays the syslog daemon. *)

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

let syslog =
  match Logs_syslog_unix.unix_reporter ~socket:path () with
  | Ok reporter -> reporter
  | Error msg -> Log_test.fail "cannot connect to the syslog socket: %s" msg

let reporter = Ocsigen.Messages.syslog_reporter syslog
let src = Logs.Src.create "test:reporter-syslog"
let user = Logs.Tag.def "user" Format.pp_print_string

let check_same_message ?header ?tags () =
  let send reporter =
    reporter.Logs.report src Logs.Warning ~over:ignore Fun.id (fun m ->
      m ?header ?tags "message %d" 42);
    without_date (receive ())
  in
  let expected = send syslog in
  let message = send reporter in
  if message <> expected
  then Log_test.fail "syslog message\n%s\ninstead of\n%s" message expected

let () =
  check_same_message ();
  check_same_message ~header:"header" ();
  check_same_message ~tags:Logs.Tag.(add user "someone" empty) ();
  check_same_message ~header:"header"
    ~tags:
      Logs.Tag.(
        empty |> add user "someone"
        |> add Logs_syslog.facility Syslog_message.Local0)
    ();
  check_same_message
    ~tags:Logs.Tag.(add Logs_syslog.facility Syslog_message.Local0 empty)
    ()

let () =
  Logs.set_reporter reporter;
  let logging =
    Logs.Tag.def "logging" (fun ppf () ->
      Logs.warn ~src (fun m -> m "inner message");
      Format.pp_print_string ppf "printed")
  in
  Log_test.on_other_thread (fun () ->
    Logs.warn ~src (fun m ->
      m ~tags:Logs.Tag.(add logging () empty) "outer message"));
  let inner = receive () in
  let outer = receive () in
  if
    not
      (Log_test.contains inner "inner message"
      && Log_test.contains outer "printed"
      && Log_test.contains outer "outer message")
  then Log_test.fail "unexpected syslog messages\n%s\n%s" inner outer;
  Unix.close daemon;
  Sys.remove path;
  Sys.rmdir dir
