(* Ocsigen
 * Copyright (C) 2005-2017 Vincent Balat
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

include Ocsigen_config_static

exception Config_file_error of string

type ssl_info = {
  ssl_certificate : string option ;
  ssl_privatekey  : string option ;
  ssl_ciphers     : string option ;
  ssl_dhfile      : string option ;
  ssl_curve       : string option
}

module Socket_type = struct

  type t = [
    | `All
    | `IPv4 of Unix.inet_addr
    | `IPv6 of Unix.inet_addr
  ]

  let to_string = function
    | `All -> Unix.string_of_inet_addr Unix.inet_addr_any
    | `IPv4 u -> Unix.string_of_inet_addr u
    | `IPv6 u -> Unix.string_of_inet_addr u

  let to_inet_addr = function
    | `All -> Unix.inet_addr_any
    | `IPv4 u -> u
    | `IPv6 u -> u

end

type socket_type = Socket_type.t

(* General config *)
let verbose = ref false
let silent = ref false
let daemon = ref false
let veryverbose = ref false
let debug = ref false
let version_number = "0000000000000000"
let pidfile = ref (None : string option)
let server_name = "Ocsigen"
let full_server_name = server_name^"/"^version_number
let native_ext = if is_native then ".exe" else ".bc"

let (uploaddir : string option ref) = ref None
let syslog_facility = ref (None : Lwt_log.syslog_facility option)
let minthreads = ref 10
let maxthreads = ref 30
let max_number_of_connections = ref 350
let silent_client_timeout = ref 30 (* without speaking during sending frame *)
let silent_server_timeout = ref 30 (* without speaking during sending frame *)
let filebuffersize = ref 8192
let maxrequestbodysize = ref (Some (Int64.of_int 8000000))
let maxrequestbodysizeinmemory = ref 8192
let maxuploadfilesize = ref (Some (Int64.of_int 2000000))
let defaultcharset = ref (None : string option)
let user = ref (Some !default_user)
let group = ref (Some !default_group)
let debugmode = ref false
let disablepartialrequests = ref false
let usedefaulthostname = ref false
let respectpipeline = ref false
let maxretries = ref 10
let shutdowntimeout = ref (Some 10.)
let ssl_info = ref None
let ports = ref []
let ssl_ports = ref []

let set_uploaddir u = uploaddir := u
let set_logdir s = logdir := Some s
let set_syslog_facility f = syslog_facility := f; logdir := None
let set_configfile s = config_file := s
let set_pidfile s = pidfile := Some s
let set_mimefile s = mimefile := s
let () = Lwt_log.add_rule "ocsigen:*" Lwt_log.Warning (* without --verbose *)
let set_verbose () =
  verbose := true;
  Lwt_log.add_rule "ocsigen:*" Lwt_log.Notice
let set_silent () = silent := true
let set_daemon () = set_silent (); daemon := true
let set_veryverbose () =
  verbose := true;
  veryverbose := true;
  Lwt_log.add_rule "ocsigen:*" Lwt_log.Info
let set_debug () =
  verbose := true;
  veryverbose := true;
  debug := true;
  Lwt_log.add_rule "ocsigen:*" Lwt_log.Debug

let set_minthreads i = minthreads := i
let set_maxthreads i = maxthreads := i
let set_max_number_of_threads_queued =
  Lwt_preemptive.set_max_number_of_threads_queued
let set_max_number_of_connections i = max_number_of_connections := i
let set_client_timeout i = silent_client_timeout := i
let set_server_timeout i = silent_server_timeout := i
(* let set_keepalive_timeout i = keepalive_timeout := i
let set_keepopen_timeout i = keepopen_timeout := i *)
let set_filebuffersize i = filebuffersize := i
let set_maxuploadfilesize i = maxuploadfilesize := i
let set_maxrequestbodysize i = maxrequestbodysize := i
let set_maxrequestbodysizeinmemory i = maxrequestbodysizeinmemory := i
let set_default_charset o = defaultcharset := o
let set_datadir o = datadir := o
let set_bindir o = bindir := o
let set_extdir o = extdir := o
let set_user o = user := o
let set_group o = group := o
let set_command_pipe s = command_pipe := s
let set_debugmode s = debugmode := s
let set_disablepartialrequests s = disablepartialrequests := s
let set_usedefaulthostname s = usedefaulthostname := s
let set_respect_pipeline () = respectpipeline := true
let set_maxretries i = maxretries := i
let set_shutdown_timeout s = shutdowntimeout := s
let set_ssl_info i = ssl_info := i
let set_ports l = ports := l
let set_ssl_ports l = ssl_ports := l

let get_uploaddir () = !uploaddir
let get_logdir () =
  match !logdir with
  | Some s -> s
  | None -> raise (
    Config_file_error ("Log directory requested, but not set"));;
let get_syslog_facility () = !syslog_facility
let get_config_file () = !config_file
let get_pidfile () = !pidfile
let get_mimefile () = !mimefile
let get_verbose () = !verbose
let get_silent () = !silent
let get_daemon () = !daemon
let get_veryverbose () = !veryverbose
let get_debug () = !debug
let get_default_user () = !default_user
let get_default_group () = !default_group
let get_minthreads () = !minthreads
let get_maxthreads () = !maxthreads
let get_max_number_of_threads_queued =
  Lwt_preemptive.get_max_number_of_threads_queued
let get_max_number_of_connections () = !max_number_of_connections
let get_client_timeout () = !silent_client_timeout
let get_server_timeout () = !silent_server_timeout
let get_filebuffersize () = !filebuffersize
let get_maxuploadfilesize () = !maxuploadfilesize
let get_maxrequestbodysize () = !maxrequestbodysize
let get_maxrequestbodysizeinmemory () = !maxrequestbodysizeinmemory
let get_default_charset () = !defaultcharset
let get_datadir () = !datadir
let get_bindir () = !bindir
let get_extdir () = !extdir
let get_user () = !user
let get_group () = !group
let get_command_pipe () = !command_pipe
let get_debugmode () = !debugmode
let get_disablepartialrequests () = !disablepartialrequests
let get_usedefaulthostname () = !usedefaulthostname
let get_respect_pipeline () = !respectpipeline
let get_maxretries () = !maxretries
let get_shutdown_timeout () = !shutdowntimeout
let get_ssl_info () = !ssl_info
let get_ports () = !ports
let get_ssl_ports () = !ssl_ports

let get_default_port () =
  match !ports with
  | (_, p) :: _ ->
    p
  | [] ->
    80

let get_default_sslport () =
  match !ssl_ports with
  | (_, p) :: _ ->
    p
  | [] ->
    443

let display_version () =
  print_string version_number;
  print_newline ();
  exit 0

module Custom = struct

  let m = ref Hmap.empty

  (* TODO : two type variables? *)
  type 'a key = ('a -> 'a) option * 'a Hmap.key

  let key ?preprocess () = preprocess, Hmap.Key.create ()

  let find (_, k) = Hmap.find k !m

  let set (f, k) v =
    let v =
      match f with
      | Some f ->
        f v
      | None ->
        v
    in
    m := Hmap.add k v !m

  let unset (_, k) = m := Hmap.rem k !m

end
