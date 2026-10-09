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

(** Writing messages in the logs

    The log reporters that Ocsigen Server installs, and its writes to the
    access log, may be used from several domains or threads at once, and
    while the logs are reopened. Each message is formatted with no lock held,
    so a printer may log, or take a lock of the application. The formatted
    message is then written whole, in one call, to each of its destinations,
    so that messages never mix. A message logged by a printer is written
    before the message that the printer is part of. Signal handlers and
    finalisers must not log: they may run while a message is written, with
    the lock of its destination held.

    The reporter mutex of Logs ([Logs.set_reporter_mutex]) is left to the
    application. The reporters of Ocsigen Server do not need it, and work
    with it if the application installs one. But Logs keeps that mutex
    locked while the message is formatted: a printer may then log only if
    the mutex is reentrant, and must not take a lock that another thread may
    hold while it logs. An application that installs its own reporter is
    responsible for its thread safety. *)

val access_sect : Logs.src

val accesslog : string -> unit
(** Write a message in access.log *)

val errlog : ?section:Logs.src -> string -> unit
(** Write a message in errors.log *)

val warning : ?section:Logs.src -> string -> unit
(** Write a message in warnings.log *)

val console : (unit -> string) -> unit
(** Write a message in the console (if not called in silent mode) *)

val unexpected_exception : exn -> string -> unit
(** Use that function for all impossible cases in exception handlers
    ([try ... with ... | e -> unexpected_exception ...] or [Lwt.catch ...]).
    A message will be written in [warnings.log].
    Put something in the string to help locating the problem (usually the name
    of the function where is has been called).
*)

val error_log_path : unit -> string
(** Path to the error log file *)

val stdio_reporter : Logs.reporter
(** A reporter that writes warnings and errors to [stderr] and everything else
    to [stdout], without opening any log file. It is used by the one-command
    serve mode (where access lines are written directly to [stdout]) and to
    report command-line errors before the logging system is configured. *)

val level_of_string : string -> Logs.level option
(** [level_of_string s] is the level named [s]: ["debug"], ["info"],
    ["notice"], ["warning"], ["error"] or ["fatal"] (the same as
    ["error"]). *)

val set_source_level : string -> Logs.level option -> bool
(** [set_source_level name level] sets the level of every log source named
    [name] to [level] ([None] turns them off), and is [false] when there is
    no such source. *)

(**/**)

val open_files : unit -> unit Lwt.t
val command_f : exn -> string -> string list -> unit Lwt.t

val syslog_reporter : Logs.reporter -> Logs.reporter
(** [syslog_reporter r] is the reporter that the syslog mode installs, which
    sends each message, formatted with no lock held, to the [Logs_syslog]
    reporter [r]. Exposed for the tests. *)
