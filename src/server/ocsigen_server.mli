(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2005
 * Vincent Balat, Denis Berthod, Nataliya Guts, Jérôme Vouillon
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

val section : Lwt_log_core.section
(** use Lwt_log.Section.set_level in order to debug *)

val reload : ?file:string -> unit -> unit
(** Reload the configuration of the server. The optional parameter
    [?file] may be used to read the configuration from another
    file. *)

val exec : Xml.xml list list -> unit
(** Start the server with a configuration file. Never returns. *)

val start :
   ?ports:(Ocsigen_config.Socket_type.t * int) list
  -> ?ssl_ports:(Ocsigen_config.Socket_type.t * int) list
  -> ?ssl_info:Ocsigen_config.ssl_info option
  -> ?default_charset:string option
  -> ?logdir:string
  -> ?datadir:string
  -> ?uploaddir:string option
  -> ?maxuploadfilesize:int64 option
  -> ?syslog_facility:Lwt_log.syslog_facility option
  -> ?configfile:string
  -> ?usedefaulthostname:bool
  -> ?pidfile:string
  -> ?mimefile:string
  -> ?verbose:unit
  -> ?veryverbose:unit
  -> ?silent:unit
  -> ?daemon:unit
  -> ?debug:unit
  -> ?debugmode:bool
  -> ?minthreads:int
  -> ?maxthreads:int
  -> ?max_number_of_threads_queued:int
  -> ?max_number_of_connections:int
  -> ?client_timeout:int
  -> ?server_timeout:int
  -> ?shutdown_timeout:float option
  -> ?filebuffersize:int
  -> ?maxrequestbodysize:int64 option
  -> ?maxrequestbodysizeinmemory:int
  -> ?bindir:string
  -> ?extdir:string
  -> ?command_pipe:string
  -> ?disablepartialrequests:bool
  -> ?respect_pipeline:unit
  -> ?maxretries:int
  -> Ocsigen_extensions.host_config list
  -> unit
(** Start the server with some instructions. Never returns.
    It takes as main parameter a list of virtual hosts (see {!host} below). 

{% Options behave exactly like their <<a_manual chapter="config"|configuration file>>%}
counterparts.
*)

type instruction =
  Ocsigen_extensions.virtual_hosts
  -> Ocsigen_extensions.config_info
  -> Ocsigen_lib.Url.path
  -> Ocsigen_extensions.extension
(** The type of instructions to be used inside an host or site.
    Instructions are defined by extensions (Staticmod, Eliom, etc.) *)

val host :
   ?regexp:string
  -> ?port:int
  -> ?default_hostname:string
  -> ?default_httpport:int
  -> ?default_httpsport:int
  -> ?default_protocol_is_https:bool
  -> ?mime_assoc:Ocsigen_charset_mime.mime_assoc
  -> ?charset_assoc:Ocsigen_charset_mime.charset_assoc
  -> ?default_directory_index:string list
  -> ?list_directory_content:bool
  -> ?follow_symlinks:[`Always | `No | `Owner_match]
  -> ?do_not_serve_404:Ocsigen_extensions.do_not_serve
  -> ?do_not_serve_403:Ocsigen_extensions.do_not_serve
  -> ?uploaddir:string option
  -> ?maxuploadfilesize:int64 option
  -> instruction list
  -> Ocsigen_extensions.host_config
(** You can define one or several virtual hosts corresponding to a given
    server name or port. *)

val site :
   ?charset:string
  -> Ocsigen_lib.Url.path
  -> instruction list
  -> instruction
(** Each host may contain some sub-sites corresponding to
    subdirectories in the URL.*)
