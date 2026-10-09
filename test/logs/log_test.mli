(** Helpers for the tests of the log reporters installed by
    {!Ocsigen.Messages.open_files}. *)

val fail : ('a, unit, string, 'b) format4 -> 'a
(** [fail fmt ...] prints the message on [stderr] and exits with code 1. *)

val open_log_dir : unit -> string
(** [open_log_dir ()] creates a temporary log directory, makes the server log
    there in silent mode with {!Ocsigen.Messages.open_files}, and is the
    directory. *)

val read : string -> string -> string
(** [read dir file] is the contents of [file] in [dir]. *)

val contains : string -> string -> bool
(** [contains s sub] is [true] if [sub] occurs in [s]. *)

val check_file :
   string
  -> string
  -> present:string list
  -> absent:string list
  -> unit
(** [check_file dir file ~present ~absent] fails unless [file] in [dir]
    contains every message of [present] and none of [absent]. *)

val remove_log_dir : string -> unit
(** [remove_log_dir dir] removes the log files and the directory made by
    {!open_log_dir}. *)
