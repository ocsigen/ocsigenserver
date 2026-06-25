
# Module `Ocsigen_config`

```ocaml
type ssl_info = Ocsigen.Config.ssl_info = {
  ssl_certificate : string option;
  ssl_privatekey : string option;
  ssl_ciphers : string option;
  ssl_dhfile : string option;
  ssl_curve : string option;
}
```
```ocaml
module Socket_type = Ocsigen.Config.Socket_type
```
```ocaml
type socket_type = Socket_type.t
```
```ocaml
exception Config_file_error of string
```
```ocaml
val set_global_log_level : Logs.level option -> unit
```
Set the log level for all Ocsigen log sources. Using this function is preferable to using `Logs.set_level` directly to avoid the excessive logging from Cohttp and let you use a different log level for your application.

```ocaml
val server_name : string
```
```ocaml
val full_server_name : string
```
```ocaml
val version_number : string
```
```ocaml
val is_native : bool
```
```ocaml
val native_ext : string
```
```ocaml
val builtin_packages : Ocsigen_base.Lib.String.Set.t
```
```ocaml
val set_logdir : string -> unit
```
```ocaml
val set_syslog_facility : Syslog_message.facility option -> unit
```
```ocaml
val set_configfile : string -> unit
```
```ocaml
val set_pidfile : string -> unit
```
```ocaml
val set_mimefile : string -> unit
```
```ocaml
val set_verbose : unit -> unit
```
```ocaml
val set_silent : unit -> unit
```
```ocaml
val set_daemon : unit -> unit
```
```ocaml
val set_veryverbose : unit -> unit
```
```ocaml
val set_debug : unit -> unit
```
```ocaml
val set_minthreads : int -> unit
```
```ocaml
val set_maxthreads : int -> unit
```
```ocaml
val set_max_number_of_connections : int -> unit
```
```ocaml
val set_client_timeout : int -> unit
```
```ocaml
val set_server_timeout : int -> unit
```
```ocaml
val set_filebuffersize : int -> unit
```
```ocaml
val set_maxrequestbodysize : int64 option -> unit
```
```ocaml
val set_maxrequestbodysizeinmemory : int -> unit
```
```ocaml
val set_default_charset : string option -> unit
```
```ocaml
val set_datadir : string -> unit
```
```ocaml
val set_bindir : string -> unit
```
```ocaml
val set_extdir : string -> unit
```
```ocaml
val set_command_pipe : string -> unit
```
```ocaml
val set_debugmode : bool -> unit
```
```ocaml
val set_disablepartialrequests : bool -> unit
```
```ocaml
val set_usedefaulthostname : bool -> unit
```
```ocaml
val set_respect_pipeline : unit -> unit
```
```ocaml
val set_maxretries : int -> unit
```
```ocaml
val set_shutdown_timeout : float option -> unit
```
```ocaml
val set_ssl_info : ssl_info option -> unit
```
```ocaml
val set_ports : (socket_type * int) list -> unit
```
```ocaml
val set_ssl_ports : (socket_type * int) list -> unit
```
```ocaml
val get_logdir : unit -> string
```
```ocaml
val get_syslog_facility : unit -> Syslog_message.facility option
```
```ocaml
val get_config_file : unit -> string
```
```ocaml
val get_pidfile : unit -> string option
```
```ocaml
val get_mimefile : unit -> string
```
```ocaml
val get_verbose : unit -> bool
```
```ocaml
val get_silent : unit -> bool
```
```ocaml
val get_daemon : unit -> bool
```
```ocaml
val get_veryverbose : unit -> bool
```
```ocaml
val get_debug : unit -> bool
```
```ocaml
val get_minthreads : unit -> int
```
```ocaml
val get_maxthreads : unit -> int
```
```ocaml
val get_max_number_of_connections : unit -> int
```
```ocaml
val get_client_timeout : unit -> int
```
```ocaml
val get_server_timeout : unit -> int
```
```ocaml
val has_configuration_file : unit -> bool
```
returns true if Ocsigen Server is running with a configuration file,

```ocaml
val get_filebuffersize : unit -> int
```
```ocaml
val get_maxrequestbodysize : unit -> int64 option
```
```ocaml
val get_maxrequestbodysizeinmemory : unit -> int
```
```ocaml
val get_default_charset : unit -> string option
```
```ocaml
val get_datadir : unit -> string
```
```ocaml
val get_bindir : unit -> string
```
```ocaml
val get_extdir : unit -> string
```
```ocaml
val get_command_pipe : unit -> string
```
```ocaml
val get_debugmode : unit -> bool
```
```ocaml
val get_disablepartialrequests : unit -> bool
```
```ocaml
val get_usedefaulthostname : unit -> bool
```
```ocaml
val get_respect_pipeline : unit -> bool
```
```ocaml
val get_default_port : unit -> int
```
```ocaml
val get_default_sslport : unit -> int
```
```ocaml
val get_maxretries : unit -> int
```
```ocaml
val get_shutdown_timeout : unit -> float option
```
```ocaml
val get_ssl_info : unit -> ssl_info option
```
```ocaml
val get_ports : unit -> (socket_type * int) list
```
```ocaml
val get_ssl_ports : unit -> (socket_type * int) list
```
```ocaml
val display_version : unit -> 'a
```