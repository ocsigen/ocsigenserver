val section : Lwt_log_core.section
(** use Lwt_log.Section.set_level in order to debug *)

val set_compress_level : int -> unit
val set_buffer_size : int -> unit

type filter = [`Type of string option * string option | `Extension of string]

val run :
   mode:[`All_but of filter list | `Only of filter list]
  -> unit
  -> Ocsigen_server.instruction
