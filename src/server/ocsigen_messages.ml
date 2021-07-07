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

let (>>=) = Lwt.bind

let access_file = "access.log"
let warning_file = "warnings.log"
let error_file = "errors.log"


let access_sect = Lwt_log.Section.make "ocsigen:access"

let full_path f = Filename.concat (Ocsigen_config.get_logdir ()) f

let error_log_path () = full_path error_file

let stderr = Lwt_log.channel ~close_mode:`Keep ~channel:Lwt_io.stderr ()
let stdout = Lwt_log.channel ~close_mode:`Keep ~channel:Lwt_io.stdout ()

let loggers = ref []

let access_logger = ref Lwt_log_core.null

let open_files ?(user = Ocsigen_config.get_user ()) ?(group = Ocsigen_config.get_group ()) () =

  (* CHECK: we are closing asynchronously!  That should be ok, though. *)
  List.iter (fun l -> ignore (Lwt_log.close l : unit Lwt.t)) !loggers;

  match Ocsigen_config.get_syslog_facility () with
  | Some facility ->
    (* log to syslog *)
    let syslog = Lwt_log.syslog ~facility () in
    loggers := [syslog];
    Lwt_log.default := Lwt_log.broadcast [syslog; stderr];
    Lwt.return ()

  | None ->
    (* log to files *)

    let open_log path =
      let path = full_path path in
      Lwt.catch
        (fun () -> Lwt_log.file ~file_name:path ())
        (function
          | Unix.Unix_error (error, _, _) ->
            Lwt.fail
              (Ocsigen_config.Config_file_error
                 (Printf.sprintf "can't open log file %s: %s"
                    path (Unix.error_message error)))
          | exn ->
            Lwt.fail exn)
    in

    open_log access_file >>= fun acc ->
    access_logger := acc;
    open_log warning_file >>= fun war ->
    open_log error_file >>= fun err ->
    loggers := [acc; war; err];

    Lwt_log.default :=
      Lwt_log.broadcast
        [Lwt_log.dispatch
           (fun _sect lev ->
              match lev with
              | Lwt_log.Error | Lwt_log.Fatal -> err
              | Lwt_log.Warning               -> war
              | _                             -> Lwt_log.null);
         Lwt_log.dispatch
           (fun _sect lev ->
              if Ocsigen_config.get_silent () then Lwt_log.null else
              match lev with
              | Lwt_log.Warning | Lwt_log.Error | Lwt_log.Fatal -> stderr
              | _ -> stdout)];

    let gid = match group with
      | None -> Unix.getgid ()
      | Some group -> (try
                         (Unix.getgrnam group).Unix.gr_gid
                       with Not_found as e -> ignore (Lwt_log.error "Error: Wrong group"); raise e)
    in

    let uid = match user with
      | None -> Unix.getuid ()
      | Some user -> (try
                        (Unix.getpwnam user).Unix.pw_uid
                      with Not_found as e -> ignore (Lwt_log.error "Error: Wrong user"); raise e)
    in
    Lwt.catch
      (fun () ->
         Lwt_unix.chown (full_path access_file) uid gid >>= fun () ->
         Lwt_unix.chown (full_path warning_file) uid gid >>= fun () ->
         Lwt_unix.chown (full_path error_file) uid gid)
      (fun e -> match e with
         | Unix.Unix_error (Unix.EPERM, _, _) ->
             (* to allow for symlinks to /dev/null *)
             Lwt.return_unit
         | _ -> Lwt.fail e)

(****)

let accesslog s =
  (* not really fatal, but log in all cases; does not affect console *)
  Lwt_log.ign_fatal ~section:access_sect ~logger:!access_logger s;
  Lwt_log.ign_notice ~section:access_sect s

let errlog ?section s = Lwt_log.ign_error ?section s

let warning ?section s = Lwt_log.ign_warning ?section s

let unexpected_exception e s =
  Lwt_log.ign_warning_f ~exn:e "Unexpected exception in %s" s

(****)

let console =
  if (not (Ocsigen_config.get_silent ())) then
    (fun s -> print_endline (s ()))
  else
    (fun _s -> ())

let level_of_string = function
  | "debug"  -> Some Lwt_log.Debug
  | "info"   -> Some Lwt_log.Info
  | "notice" -> Some Lwt_log.Notice
  | "warning"-> Some Lwt_log.Warning
  | "error"  -> Some Lwt_log.Error
  | "fatal"  -> Some Lwt_log.Fatal
  | _ -> None

let command_f exc _ = function
  | [sect_name] ->
    (* Lwt_log.Section.make :
       if a section with the same name
       already exists, it is returned. *)
    let sect = Lwt_log.Section.make sect_name in
    Lwt_log.Section.reset_level sect;
    Lwt.return_unit
  | [sect_name; level_name] ->
    (* Lwt_log.Section.make :
       if a section with the same name
       already exists, it is returned. *)
    let sect = Lwt_log.Section.make sect_name in
    (match level_of_string (String.lowercase_ascii level_name) with
    | None -> Lwt_log.Section.reset_level sect
    | Some l -> Lwt_log.Section.set_level sect l);
    Lwt.return ()
  | _ -> Lwt.fail exc
