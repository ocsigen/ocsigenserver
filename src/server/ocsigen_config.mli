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

(** Configuring Ocsigen server *)

open Ocsigen_lib

type ssl_info = {
  ssl_certificate : string option ;
  ssl_privatekey  : string option ;
  ssl_ciphers     : string option ;
  ssl_dhfile      : string option ;
  ssl_curve       : string option
}

module Socket_type : sig

  type t = [
    | `All
    | `IPv4 of Unix.inet_addr
    | `IPv6 of Unix.inet_addr
  ]

  val to_string : t -> string

  val to_inet_addr : t -> Unix.inet_addr

end

type socket_type = Socket_type.t

exception Config_file_error of string

val server_name : string
val full_server_name : string
val version_number : string
val is_native : bool
val native_ext : string

val builtin_packages : String.Set.t

val set_logdir : string -> unit
val set_syslog_facility: Lwt_log.syslog_facility option -> unit
val set_configfile : string -> unit
val set_pidfile : string -> unit
val set_mimefile : string -> unit
val set_verbose : unit -> unit
val set_silent : unit -> unit
val set_daemon : unit -> unit
val set_veryverbose : unit -> unit
val set_debug : unit -> unit
val set_minthreads : int -> unit
val set_maxthreads : int -> unit
val set_max_number_of_threads_queued : int -> unit
val set_max_number_of_connections : int -> unit
val set_client_timeout : int -> unit
val set_server_timeout : int -> unit
(* val set_keepalive_timeout : int -> unit
   val set_keepopen_timeout : int -> unit *)
val set_filebuffersize : int -> unit
val set_maxrequestbodysize : int64 option -> unit
val set_maxrequestbodysizeinmemory : int -> unit
val set_default_charset : string option -> unit
val set_datadir : string -> unit
val set_bindir : string -> unit
val set_extdir : string -> unit
val set_user : string option -> unit
val set_group : string option -> unit
val set_command_pipe : string -> unit
val set_debugmode : bool -> unit
val set_disablepartialrequests : bool -> unit
val set_usedefaulthostname : bool -> unit
val set_respect_pipeline : unit -> unit
val set_maxretries : int -> unit
val set_shutdown_timeout : float option -> unit
val set_ssl_info : ssl_info option -> unit
val set_ports : (socket_type * int) list -> unit
val set_ssl_ports : (socket_type * int) list -> unit

val get_logdir : unit -> string
val get_syslog_facility: unit -> Lwt_log.syslog_facility option
val get_config_file : unit -> string
val get_pidfile : unit -> string option
val get_mimefile : unit -> string
val get_verbose : unit -> bool
val get_silent : unit -> bool
val get_daemon : unit -> bool
val get_veryverbose : unit -> bool
val get_debug : unit -> bool
val get_default_user : unit -> string
val get_default_group : unit -> string
val get_minthreads : unit -> int
val get_maxthreads : unit -> int
val get_max_number_of_threads_queued : unit -> int
val get_max_number_of_connections : unit -> int
val get_client_timeout : unit -> int
val get_server_timeout : unit -> int
(*val get_keepalive_timeout : unit -> int
  val get_keepopen_timeout : unit -> int*)
val get_filebuffersize : unit -> int
val get_maxrequestbodysize : unit -> int64 option
val get_maxrequestbodysizeinmemory : unit -> int
val get_default_charset : unit -> string option
val get_datadir : unit -> string
val get_bindir : unit -> string
val get_extdir : unit -> string
val get_user : unit -> string option
val get_group : unit -> string option
val get_command_pipe : unit -> string
val get_debugmode : unit -> bool
val get_disablepartialrequests : unit -> bool
val get_usedefaulthostname : unit -> bool
val get_respect_pipeline : unit -> bool
val get_default_port : unit -> int
val get_default_sslport : unit -> int
val get_maxretries : unit -> int
val get_shutdown_timeout : unit -> float option
val get_ssl_info : unit -> ssl_info option
val get_ports : unit -> (socket_type * int) list
val get_ssl_ports : unit -> (socket_type * int) list

val display_version : unit -> 'a

(**/**)

(* Global setting for upload directory. This can be overwritten
   on a per-site basis. Thus, use only the value inside the [ri.request_config]
   field of a request (which can be changed by the extension
   [Extendconfiguration]) *)
val set_uploaddir : string option -> unit
val get_uploaddir : unit -> string option

(* Same thing for upload size *)
val set_maxuploadfilesize : int64 option -> unit
val get_maxuploadfilesize : unit -> int64 option

module Custom : sig

  type 'a key

  val key : ?preprocess:('a -> 'a) -> unit -> 'a key

  val find : 'a key -> 'a option

  val set : 'a key -> 'a -> unit

  val unset : 'a key -> unit

end
