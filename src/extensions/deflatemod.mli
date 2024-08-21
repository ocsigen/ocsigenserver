val section : Lwt_log_core.section
(** use Lwt_log.Section.set_level in order to debug *)

val compress_level : int Ocsigen_config.Custom.key
val buffer_size : int Ocsigen_config.Custom.key

type filter = [`Type of string option * string option | `Extension of string]

val mode :
  [`All_but of filter list | `Only of filter list]
    Ocsigen_server.Site.Config.key

val extension : Ocsigen_server.Site.instruction
