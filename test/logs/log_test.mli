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

val lines : string -> string -> string list
(** [lines dir file] is the non-empty lines of [file] in [dir]. *)

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

val check_lines :
   what:string
  -> writers:int
  -> messages:int
  -> (string -> int * int)
  -> string list
  -> unit
(** [check_lines ~what ~writers ~messages parse lines] fails unless [lines]
    are, in some order, the messages [1] to [messages] of each of the writers
    [0] to [writers - 1], [parse] giving the writer and the number of a message
    from its line. Every line must be whole: a line mixed with another one
    does not parse, or is a duplicate. [what] names the lines in the error
    messages. *)

val remove_log_dir : string -> unit
(** [remove_log_dir dir] removes the log files and the directory made by
    {!open_log_dir}. *)
