(* Windows named-pipe command channel. On Windows the command pipe cannot be a
   Unix FIFO (Unix.mkfifo is unavailable); use a named pipe instead. The actual
   reading is done with these (blocking) stubs through Lwt_preemptive.detach. On
   other platforms the Unix FIFO is used and these are never called. *)

(* Whether named pipes are used (Windows only). *)
let supported = Sys.win32

(* Create an inbound named pipe (e.g. {v \\.\pipe\name v}) and return its
   handle. Raises [Failure] on error. *)
external create : string -> nativeint = "ocsigen_command_pipe_create"

(* Block until a client connects and writes, return the bytes read (empty on
   disconnect), then make the pipe ready for the next client. Run this through
   Lwt_preemptive.detach: it releases the OCaml runtime lock while blocking. *)
external read : nativeint -> string = "ocsigen_command_pipe_read"

(* Connect to a named pipe (e.g. {v \\.\pipe\name v}) as a client and write the
   given string to it. Raises [Failure] if the pipe cannot be opened. Used to
   send a command to a running server. *)
external send : string -> string -> unit = "ocsigen_command_pipe_send"
