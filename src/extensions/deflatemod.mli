val compress_level : int Ocsigen_config.Custom.key

val buffer_size : int Ocsigen_config.Custom.key

type filter = [
  | `Type of string option * string option
  | `Extension of string
]

val mode :
  [`All_but of filter list | `Only of filter list]
    Ocsigen_extensions.Virtual_host.Config.key

val register : Ocsigen_extensions.Virtual_host.t -> unit
