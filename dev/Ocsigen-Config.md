# Module `Ocsigen.Config`

Configuring Ocsigen server

```ocaml
type ssl_info = {
  ssl_certificate : string option;
  ssl_privatekey : string option;
  ssl_ciphers : string option;
  ssl_dhfile : string option;
  ssl_curve : string option;
}
```
```ocaml
module Socket_type : sig ... end
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
`set_global_log_level l` sets the level of the existing Ocsigen Server log sources (named `ocsigen:...`) to `l`, and makes `l` the default level of the sources created afterwards. The other existing sources keep their level: unlike `Logs.set_level`, this does not turn on the excessive logging of Cohttp.

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
The directory of the log files. Without it, or with an empty path, there is no log directory: unless syslog is used, the logs go to `stdout` and `stderr`.

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
val set_log_to_stderr : bool -> unit
```
When set to `true`, logs are written to `stdout`/`stderr` instead of the log files under [`get_logdir`](./#val-get_logdir). Used by the one-command serve mode so that no log directory needs to exist.

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
val set_keepalive_timeout : int -> unit
```
Seconds a kept-alive connection may stay idle between two requests; 0 for no bound. See `<keepalivetimeout>` in the configuration file.

```ocaml
val set_min_data_rate : int -> unit
```
Bytes per second: the minimum rate of a request body, and of a response the client reads, past a grace of one client timeout; 0 for no minimum. See `<mindatarate>` in the configuration file.

```ocaml
val set_max_connections_per_address : int -> unit
```
The maximum number of simultaneous connections from a single client address; 0 for no limit. See `<maxconnectionsperaddress>` in the configuration file.

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
val set_bindir : string -> unit
```
```ocaml
val set_extdir : string -> unit
```
```ocaml
val set_command_pipe : string -> unit
```
The path of the command pipe. Without it, the server has no command pipe.

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
val get_logdir : unit -> string option
```
The directory of the log files, or `None` if there is none.

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
val get_log_to_stderr : unit -> bool
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
The maximum number of simultaneous connections: the value set, or [`default_max_number_of_connections`](./#val-default_max_number_of_connections). Once the server has started, it is the value in effect, which the server lowers if the limit on open files does not allow as many connections (see [`Open_files`](./Ocsigen-Open_files.md)).

```ocaml
val default_max_number_of_connections : int
```
10 000\.

```ocaml
val max_number_of_connections_is_set : unit -> bool
```
Whether the maximum number of simultaneous connections was set, rather than left to its default.

```ocaml
val set_max_number_of_connections_in_effect : int -> unit
```
`set_max_number_of_connections_in_effect n` records `n` as the value in effect, which [`get_max_number_of_connections`](./#val-get_max_number_of_connections) is from then on. The server calls it at startup, once it has fitted the value to the limit on open files.

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
val get_keepalive_timeout : unit -> int
```
```ocaml
val get_min_data_rate : unit -> int
```
```ocaml
val get_max_connections_per_address : unit -> int
```
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
val get_bindir : unit -> string
```
```ocaml
val get_extdir : unit -> string
```
```ocaml
val get_command_pipe : unit -> string option
```
The path of the command pipe, or `None` if the server has none.

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
