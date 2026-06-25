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

val section : Logs.src

val reload : ?file:string -> unit -> unit
(** Reload the configuration of the server. The optional parameter
    [?file] may be used to read the configuration from another
    file. *)

val exec : Xml.xml list list -> unit
(** Start the server with a configuration file. Never returns. *)

val start :
   ?ports:(Config.Socket_type.t * int) list
  -> ?ssl_ports:(Config.Socket_type.t * int) list
  -> ?ssl_info:Config.ssl_info option
  -> ?default_charset:string option
  -> ?logdir:string
  -> ?datadir:string
  -> ?uploaddir:string option
  -> ?maxuploadfilesize:int64 option
  -> ?syslog_facility:Syslog_message.facility option
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
  -> Extensions.host_config list
  -> unit
(** Start the server with some instructions. Never returns.
    It takes as main parameter a list of virtual hosts (see {!host} below). 

Options behave exactly like their {{!page-"config"}configuration file}
counterparts.
*)

type instruction =
  Extensions.virtual_hosts
  -> Extensions.config_info
  -> Ocsigen_base.Lib.Url.path
  -> Extensions.extension
(** The type of instructions to be used inside an host or site.
    Instructions are defined by extensions (Staticmod, Eliom, etc.) *)

val host :
   ?regexp:string
  -> ?port:int
  -> ?default_hostname:string
  -> ?default_httpport:int
  -> ?default_httpsport:int
  -> ?default_protocol_is_https:bool
  -> ?mime_assoc:Ocsigen_http.Charset_mime.mime_assoc
  -> ?charset_assoc:Ocsigen_http.Charset_mime.charset_assoc
  -> ?default_directory_index:string list
  -> ?list_directory_content:bool
  -> ?follow_symlinks:[`Always | `No | `Owner_match]
  -> ?do_not_serve_404:Extensions.do_not_serve
  -> ?do_not_serve_403:Extensions.do_not_serve
  -> ?uploaddir:string option
  -> ?maxuploadfilesize:int64 option
  -> instruction list
  -> Extensions.host_config
(** You can define one or several virtual hosts corresponding to a given
    server name or port. *)

val site :
   ?charset:string
  -> Ocsigen_base.Lib.Url.path
  -> instruction list
  -> instruction
(** Each host may contain some sub-sites corresponding to
    subdirectories in the URL.*)
