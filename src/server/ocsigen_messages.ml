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
let full_path f = Filename.concat (Ocsigen_config.get_logdir ()) f
let error_log_path () = full_path error_file

(* This is the date format inherited from [Lwt_log]. *)
let pp_date ppf =
  let time = Unix.gettimeofday () in
  let tm = Unix.localtime time in
  let month_string =
    match tm.Unix.tm_mon with
    | 0 -> "Jan"
    | 1 -> "Feb"
    | 2 -> "Mar"
    | 3 -> "Apr"
    | 4 -> "May"
    | 5 -> "Jun"
    | 6 -> "Jul"
    | 7 -> "Aug"
    | 8 -> "Sep"
    | 9 -> "Oct"
    | 10 -> "Nov"
    | 11 -> "Dec"
    | _ -> ""
  in
  Format.fprintf ppf "%s %2d %02d:%02d:%02d" month_string tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

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

let open_files () =
  (* CHECK: we are closing asynchronously! That should be ok, though. *)
  List.iter (fun close -> close ()) !close_loggers;
  close_loggers := [];
  match Ocsigen_config.get_syslog_facility () with
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
                 k broadcast_reporters ()) })
  | None ->
      (* log to files *)
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
              (Ocsigen_config.Config_file_error
                 (Printf.sprintf "can't open log file %s: %s" path
                    (Unix.error_message error)))
        | exn -> raise exn
      in
      let open_log path =
        let channel, close = open_channel path in
        make_reporter channel, close
      in
      let acc = open_log access_file in
      let war = open_log warning_file in
      let err = open_log error_file in
      close_loggers := [snd acc; snd war; snd err];
      Logs.set_reporter
        (let broadcast_reporters =
           [ { Logs.report =
                 (fun src _level ~over k msgf ->
                   let r =
                     if Logs.Src.equal src access_sect
                     then fst acc
                     else Logs.nop_reporter
                   in
                   r.Logs.report src Error ~over k msgf) }
           ; (let dispatch_f =
               fun _sect lev ->
                match lev with
                | Logs.Error -> fst err
                | Logs.Warning -> fst war
                | _ -> Logs.nop_reporter
              in
              { Logs.report =
                  (fun src level ~over k msgf ->
                    (dispatch_f src level).Logs.report src level ~over k msgf)
              })
           ; (let dispatch_f =
               fun _sect lev ->
                if Ocsigen_config.get_silent ()
                then Logs.nop_reporter
                else
                  match lev with
                  | Logs.Warning | Logs.Error -> stderr
                  | _ -> stdout
              in
              { Logs.report =
                  (fun src level ~over k msgf ->
                    (dispatch_f src level).Logs.report src level ~over k msgf)
              }) ]
         in
         { Logs.report =
             (fun src level ~over k msgf ->
               List.fold_left
                 (fun k r () -> r.Logs.report src level ~over k msgf)
                 k broadcast_reporters ()) })

(****)

let accesslog s = Logs.app ~src:access_sect (fun fmt -> fmt "%s" s)
let errlog ?section s = Logs.err ?src:section (fun fmt -> fmt "%s" s)
let warning ?section s = Logs.warn ?src:section (fun fmt -> fmt "%s" s)

let unexpected_exception e s =
  Logs.warn (fun fmt -> fmt "Unexpected exception in %s@\n%a" s Eio.Exn.pp e)

(****)

let console =
  if not (Ocsigen_config.get_silent ())
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
      Logs.Src.set_level sect None
  | [sect_name; level_name] -> (
      (* Lwt_log.Section.make :
       if a section with the same name
       already exists, it is returned. *)
      let sect = Logs.Src.create sect_name in
      match level_of_string (String.lowercase_ascii level_name) with
      | None -> Logs.Src.set_level sect None
      | Some l -> Logs.Src.set_level sect (Some l))
  | _ -> raise exc
