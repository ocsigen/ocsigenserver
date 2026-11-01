(** Count the files that a test server keeps open. *)

val open_files : unit -> int
(** [open_files ()] is the number of regular files open in the process, on
    Linux and macOS. The sockets and pipes, which come and go with the
    connections, are not counted. *)

val instruction : Ocsigen.Server.instruction
(** [instruction] answers [open-files] with [open_files ()], and leaves the
    other paths to the next instructions. *)
