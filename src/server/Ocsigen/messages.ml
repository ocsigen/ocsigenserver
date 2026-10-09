(* Ocsigen
 * Copyright (C) 2005 Vincent Balat
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(** Writing messages in the logs *)

let access_file = "access.log"
let warning_file = "warnings.log"
let error_file = "errors.log"
let access_sect = Logs.Src.create "ocsigen:access"
let full_path f = Filename.concat (Config.get_logdir ()) f
let error_log_path () = full_path error_file

(* The date is computed once per second: [last_date] is the last one, with
   its second. Two domains may compute it at once: either result is kept, and
   if an older date replaces a newer one, the newer one is computed again. *)
let last_date = Atomic.make (neg_infinity, "")

(* This is the date format inherited from [Lwt_log]. *)
let date_string () =
  let now = Unix.time () in
  match Atomic.get last_date with
  | second, date when second = now -> date
  | _ ->
      let tm = Ocsigen_base.Lib.Date.localtime now in
      let date =
        Printf.sprintf "%s %2d %02d:%02d:%02d"
          (Ocsigen_base.Lib.Date.name_of_month tm.Unix.tm_mon)
          tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
      in
      Atomic.set last_date (now, date);
      date

let pp_date ppf = Format.pp_print_string ppf (date_string ())

(* The logs may be written from several domains or threads at once. Each
   message is formatted first, with a formatter of its own and no lock held,
   so that the printers of the application never run with a lock of this
   module. The line is then written to each of its destinations ([Sink.t])
   under the mutex of that destination only. No lock of this module is taken
   while another one is held, so they cannot deadlock, between themselves or
   with the locks of the application.

   One exception: OCaml may run signal handlers, finalisers and Memprof
   callbacks while a channel is flushed, so with the mutex of a destination
   locked. These functions must not log. Logging to the same destination
   would raise [Sys_error] (OCaml mutexes detect it), and logging to another
   one would take two locks at once.

   The reporter mutex of Logs ([Logs.set_reporter_mutex]) is left to the
   application: the reporters below do not need it. If the application
   installs one, Logs keeps it locked while the message is formatted, so the
   printers run with it locked. *)

let with_lock = Ocsigen_base.Lib.with_lock

(* A destination of log lines: a terminal, or a log file. *)
module Sink : sig
  type t

  val make : out_channel option -> t
  (** [make c] is a destination that writes to [c], or nowhere if [c] is
      [None]. *)

  val write : t -> string -> unit
  (** [write sink s] writes [s] to [sink] and flushes it. Several domains or
      threads may write to [sink] at the same time: each [s] is written
      whole. *)

  val replace : t -> out_channel option -> unit
  (** [replace sink c] makes [sink] write to [c] from now on, and closes the
      channel it wrote to before. A write that runs at the same time, from
      another domain or thread, goes to one channel or the other, never to a
      closed channel. *)
end = struct
  (* [channel] is only read or changed with [mutex] locked. *)
  type t = {mutex : Mutex.t; mutable channel : out_channel option}

  let make channel = {mutex = Mutex.create (); channel}

  let with_channel sink f =
    with_lock sink.mutex (fun () -> Option.iter f sink.channel)

  let write sink s =
    with_channel sink (fun channel -> output_string channel s; flush channel)

  let replace sink channel =
    with_lock sink.mutex (fun () ->
      (* The old channel is closed with [mutex] locked, so that what it still
         buffers is written before anything written to the new one. *)
      Option.iter close_out_noerr sink.channel;
      sink.channel <- channel)
end

let stdout_sink = Sink.make (Some Stdlib.stdout)
let stderr_sink = Sink.make (Some Stdlib.stderr)

(* The log files, opened in the log directory by [open_log_files_in_dir].
   They are kept when the logs are reopened, and their channels replaced, so
   that reopening does not race with the domains or threads that log. *)
let access_sink = Sink.make None
let warning_sink = Sink.make None
let error_sink = Sink.make None

let close_log_files () =
  List.iter
    (fun sink -> Sink.replace sink None)
    [access_sink; warning_sink; error_sink]

(* [reporter sinks] writes each message, whole and in one call, to the
   destinations [sinks level]. A message without destination is not
   formatted. A message logged by a printer is written before the message it
   prints. *)
let reporter sinks =
  let report src level ~over k msgf =
    match sinks level with
    | [] -> over (); k ()
    | sinks ->
        msgf @@ fun ?header ?tags:_ fmt ->
        Format.kasprintf
          (fun line ->
             List.iter (fun sink -> Sink.write sink line) sinks;
             over ();
             k ())
          ("%t: %s: %a @[" ^^ fmt ^^ "@]@.")
          pp_date (Logs.Src.name src) Logs.pp_header (level, header)
  in
  {Logs.report}

(* [Logs_syslog] formats every message in one buffer, shared by its
   reporters, and keeps the state of its connection: [syslog_mutex]
   serializes them. The message is formatted before, with no lock held, and
   given to [syslog] whole, with its tags and header, as [Logs_syslog] would
   print them. Only the facility tag is given to [syslog], which does not
   print it: no printer of the application runs with [syslog_mutex] locked. *)
let syslog_mutex = Mutex.create ()

let syslog_reporter syslog =
  let report src level ~over k msgf =
    msgf @@ fun ?header ?(tags = Logs.Tag.empty) fmt ->
    Format.kasprintf
      (fun msg ->
         let facility = Logs.Tag.find Logs_syslog.facility tags in
         let tags = Logs.Tag.rem Logs_syslog.facility tags in
         let content =
           String.concat " "
             ((if Logs.Tag.is_empty tags
               then []
               else [Format.asprintf "%a" Logs.Tag.pp_set tags])
             @ Option.to_list header @ [msg])
         in
         let facility_tag =
           match facility with
           | Some facility -> Logs.Tag.(add Logs_syslog.facility facility empty)
           | None -> Logs.Tag.empty
         in
         with_lock syslog_mutex (fun () ->
           syslog.Logs.report src level ~over:ignore Fun.id (fun m ->
             m ~tags:facility_tag "%s" content));
         over ();
         k ())
      fmt
  in
  {Logs.report}

(* Send each message to all [reporters], in order. Logs requires [over] to be
   called exactly once per message ([Logs.set_reporter_mutex] releases its lock
   there), so the sub-reporters get a no-op and the real [over] runs after the
   last one. *)
let broadcast reporters =
  let report src level ~over k msgf =
    let rec loop = function
      | [] -> over (); k ()
      | r :: rs -> r.Logs.report src level ~over:ignore (fun () -> loop rs) msgf
    in
    loop reporters
  in
  {Logs.report}

(* Access logging bypasses Logs and Format: a complete Combined Log Format line
   is written directly to its destination. [access_out] is installed by
   [open_files] according to the logging mode. *)
let access_out = Atomic.make (fun (_ : string) -> ())

(* Echo an access line on the console with the same readable date/source prefix
   as the other logs ([access.log] keeps the verbatim Combined format). *)
let console_access s =
  Printf.sprintf "%s: %s: %s\n" (date_string ()) (Logs.Src.name access_sect) s

(* Reporter for the non-access logs in serve mode: warnings and errors to
   [stderr], everything else to [stdout]. Access lines are written directly to
   [stdout] (see [log_to_stdio]). Also used to report command-line errors
   before the logging system is configured. *)
let stdio_reporter =
  reporter (function
    | Logs.Warning | Logs.Error -> [stderr_sink]
    | _ -> [stdout_sink])

let log_to_stdio () =
  Logs.set_reporter stdio_reporter;
  (* In serve mode the terminal is the access log. *)
  Atomic.set access_out (fun s -> Sink.write stdout_sink (s ^ "\n"));
  close_log_files ();
  Lwt.return ()

(* Write logs to the access/warnings/errors files in the log directory. *)
let open_log_files_in_dir () =
  let open_channel path =
    let path = full_path path in
    try
      open_out_gen [Open_append; Open_wronly; Open_creat; Open_text] 0o640 path
    with
    | Unix.Unix_error (error, _, _) ->
        raise
          (Config.Config_file_error
             (Printf.sprintf "can't open log file %s: %s" path
                (Unix.error_message error)))
    | exn -> raise exn
  in
  (* [with_channel file f] is [f c], [c] being a new channel to [file], which
     is closed if [f] raises: no channel is left open when a later file
     cannot be opened. *)
  let with_channel file f =
    let channel = open_channel file in
    match f channel with
    | v -> v
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        close_out_noerr channel;
        Printexc.raise_with_backtrace exn bt
  in
  let access, warnings, errors =
    with_channel access_file @@ fun access ->
    with_channel warning_file @@ fun warnings ->
    with_channel error_file @@ fun errors -> access, warnings, errors
  in
  (* The channels are given to the sinks only once they are all open, so
     that [with_channel] never closes a channel that a sink writes to. *)
  Sink.replace access_sink (Some access);
  Sink.replace warning_sink (Some warnings);
  Sink.replace error_sink (Some errors);
  (* Access lines: verbatim Combined format to [access.log], plus a prefixed
     echo on the console (unless silent). *)
  Atomic.set access_out (fun s ->
    Sink.write access_sink (s ^ "\n");
    if not (Config.get_silent ()) then Sink.write stdout_sink (console_access s));
  Logs.set_reporter
    (reporter (fun level ->
       let silent = Config.get_silent () in
       match level with
       | Logs.Error -> if silent then [error_sink] else [error_sink; stderr_sink]
       | Logs.Warning ->
           if silent then [warning_sink] else [warning_sink; stderr_sink]
       | _ -> if silent then [] else [stdout_sink]));
  Lwt.return ()

let open_log_files () =
  match Config.get_syslog_facility () with
  | Some facility ->
      (* log to syslog *)
      (* Syslog reporter cannot be closed *)
      let syslog =
        match Logs_syslog_unix.unix_reporter ~facility () with
        | Ok r -> r
        | Error msg -> failwith msg
      in
      Logs.set_reporter
        (broadcast [syslog_reporter syslog; reporter (fun _ -> [stderr_sink])]);
      (* No access.log file in syslog mode: access lines reach syslog (and
         stderr) through Logs. *)
      Atomic.set access_out (fun s ->
        Logs.app ~src:access_sect (fun fmt -> fmt "%s" s));
      close_log_files ();
      Lwt.return ()
  | None ->
      (* When no log directory is configured, log to stdout/stderr instead of
         creating files in the current directory. *)
      if Config.get_logdir () = ""
      then log_to_stdio ()
      else open_log_files_in_dir ()

let open_files () =
  if Config.get_log_to_stderr () then log_to_stdio () else open_log_files ()

(****)

let accesslog s = (Atomic.get access_out) s
let errlog ?section s = Logs.err ?src:section (fun fmt -> fmt "%s" s)
let warning ?section s = Logs.warn ?src:section (fun fmt -> fmt "%s" s)

let unexpected_exception e s =
  Logs.warn (fun fmt ->
    fmt ("Unexpected exception in %s" ^^ "@\n%s") s (Printexc.to_string e))

(****)

let console =
  if not (Config.get_silent ())
  then fun s -> print_endline (s ())
  else fun _s -> ()

let level_of_string = function
  | "debug" -> Some Logs.Debug
  | "info" -> Some Logs.Info
  | "notice" -> Some Logs.App
  | "warning" -> Some Logs.Warning
  | "error" -> Some Logs.Error
  | "fatal" -> Some Logs.Error
  | _ -> None

(* Logs does not make source names unique: every source with that name is
   set. [Logs.Src.create] would make a new source rather than return the
   existing one. *)
let set_source_level name level =
  match
    List.filter (fun src -> Logs.Src.name src = name) (Logs.Src.list ())
  with
  | [] -> false
  | sources ->
      List.iter (fun src -> Logs.Src.set_level src level) sources;
      true

let command_section = Logs.Src.create "ocsigen:command"

(* The [logs:] command of the command pipe: [logs:name level] sets the
   level of the log source [name], [logs:name] turns it off. *)
let command_f exc _ args =
  let set name level =
    if not (set_source_level name level)
    then
      Logs.warn ~src:command_section (fun fmt ->
        fmt "logs: no log source named %s" name)
  in
  match args with
  | [name] -> set name None; Lwt.return_unit
  | [name; level_name] ->
      (match level_of_string (String.lowercase_ascii level_name) with
      | Some level -> set name (Some level)
      | None ->
          Logs.warn ~src:command_section (fun fmt ->
            fmt "logs: unknown log level %s" level_name));
      Lwt.return_unit
  | _ -> Lwt.fail exc
