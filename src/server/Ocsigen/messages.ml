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

(* This is the date format inherited from [Lwt_log]. *)
let date_string () =
  let tm = Unix.localtime (Unix.gettimeofday ()) in
  Printf.sprintf "%s %2d %02d:%02d:%02d"
    (Ocsigen_base.Lib.Date.name_of_month tm.Unix.tm_mon)
    tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

let pp_date ppf = Format.pp_print_string ppf (date_string ())

let make_reporter out_channel =
  let ppf = Format.formatter_of_out_channel out_channel in
  let report src level ~over k msgf =
    let k _ = over (); k () in
    msgf @@ fun ?header ?tags:_ fmt ->
    Format.kfprintf k ppf
      ("%t: %s: %a @[" ^^ fmt ^^ "@]@.")
      pp_date (Logs.Src.name src) Logs.pp_header (level, header)
  in
  {Logs.report}

let stderr = make_reporter stderr
let stdout = make_reporter stdout
let close_loggers = ref []

(* Access logging bypasses Logs and Format: a complete Combined Log Format line
   is written directly to the output channel. [access_out] is installed by
   [open_files] according to the logging mode. *)
let access_out = ref (fun (_ : string) -> ())
let write_line oc s = output_string oc s; output_char oc '\n'; flush oc

(* Echo an access line on the console with the same readable date/source prefix
   as the other logs ([access.log] keeps the verbatim Combined format). *)
let console_access oc s =
  Printf.fprintf oc "%s: %s: %s\n%!" (date_string ())
    (Logs.Src.name access_sect)
    s

(* Reporter for the non-access logs in serve mode: warnings and errors to
   [stderr], everything else to [stdout]. Access lines are written directly to
   [stdout] (see [log_to_stdio]). Also used to report command-line errors
   before the logging system is configured. *)
let stdio_reporter =
  { Logs.report =
      (fun src level ~over k msgf ->
        let r =
          match level with Logs.Warning | Logs.Error -> stderr | _ -> stdout
        in
        r.Logs.report src level ~over k msgf) }

let log_to_stdio () =
  Logs.set_reporter stdio_reporter;
  (* In serve mode the terminal is the access log. *)
  access_out := write_line Stdlib.stdout;
  Lwt.return ()

(* Write logs to the access/warnings/errors files in the log directory. *)
let open_log_files_in_dir () =
  let open_channel path =
    let path = full_path path in
    try
      let channel =
        open_out_gen
          [Open_append; Open_wronly; Open_creat; Open_text]
          0o640 path
      in
      channel, fun () -> close_out_noerr channel
    with
    | Unix.Unix_error (error, _, _) ->
        raise
          (Config.Config_file_error
             (Printf.sprintf "can't open log file %s: %s" path
                (Unix.error_message error)))
    | exn -> raise exn
  in
  let open_log path =
    let channel, close = open_channel path in
    make_reporter channel, close
  in
  let acc_channel, acc_close = open_channel access_file in
  let war = open_log warning_file in
  let err = open_log error_file in
  close_loggers := [acc_close; snd war; snd err];
  (* Access lines: verbatim Combined format to [access.log], plus a prefixed
     echo on the console (unless silent). *)
  (access_out :=
     fun s ->
       write_line acc_channel s;
       if not (Config.get_silent ()) then console_access Stdlib.stdout s);
  Logs.set_reporter
    (let broadcast_reporters =
       [ (let dispatch_f =
           fun _sect lev ->
            match lev with
            | Logs.Error -> fst err
            | Logs.Warning -> fst war
            | _ -> Logs.nop_reporter
          in
          { Logs.report =
              (fun src level ~over k msgf ->
                (dispatch_f src level).Logs.report src level ~over k msgf) })
       ; (let dispatch_f =
           fun _sect lev ->
            if Config.get_silent ()
            then Logs.nop_reporter
            else
              match lev with Logs.Warning | Logs.Error -> stderr | _ -> stdout
          in
          { Logs.report =
              (fun src level ~over k msgf ->
                (dispatch_f src level).Logs.report src level ~over k msgf) }) ]
     in
     { Logs.report =
         (fun src level ~over k msgf ->
           List.fold_left
             (fun k r () -> r.Logs.report src level ~over k msgf)
             k broadcast_reporters ()) });
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
        (let broadcast_reporters = [syslog; stderr] in
         { Logs.report =
             (fun src level ~over k msgf ->
               List.fold_left
                 (fun k r () -> r.Logs.report src level ~over k msgf)
                 k broadcast_reporters ()) });
      (* No access.log file in syslog mode: access lines reach syslog (and
         stderr) through Logs. *)
      (access_out := fun s -> Logs.app ~src:access_sect (fun fmt -> fmt "%s" s));
      Lwt.return ()
  | None ->
      (* When no log directory is configured, log to stdout/stderr instead of
         creating files in the current directory. *)
      if Config.get_logdir () = ""
      then log_to_stdio ()
      else open_log_files_in_dir ()

let open_files () =
  (* CHECK: we are closing asynchronously! That should be ok, though. *)
  List.iter (fun close -> close ()) !close_loggers;
  close_loggers := [];
  if Config.get_log_to_stderr () then log_to_stdio () else open_log_files ()

(****)

let accesslog s = !access_out s
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

let command_f exc _ = function
  | [sect_name] ->
      (* Lwt_log.Section.make :
       if a section with the same name
       already exists, it is returned. *)
      let sect = Logs.Src.create sect_name in
      Logs.Src.set_level sect None;
      Lwt.return_unit
  | [sect_name; level_name] ->
      (* Lwt_log.Section.make :
       if a section with the same name
       already exists, it is returned. *)
      let sect = Logs.Src.create sect_name in
      (match level_of_string (String.lowercase_ascii level_name) with
      | None -> Logs.Src.set_level sect None
      | Some l -> Logs.Src.set_level sect (Some l));
      Lwt.return ()
  | _ -> Lwt.fail exc
