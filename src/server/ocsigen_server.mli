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

val start : ?config:Xml.xml list list -> unit -> unit
(** Start the server. Never returns. *)

(** Use this to create an extension that can be linked statically,
    and used without configuration file. *)
module Site : sig
  type t

  val create :
     ?config_info:Ocsigen_extensions.config_info
    -> ?id:[`Attach of t * Ocsigen_lib.Url.path | `Host of string * int option]
    -> ?charset:Ocsigen_charset_mime.charset
    -> unit
    -> t
  (** [create ?config_info ?id ?charset ()]
  creates a subsite.
  This is equivalent to the [<host>] or [<site>] config file options.
  *)

  val default_host : t
  (** Defaut host. Any hostname, any port.
      Will be used if you don not specify [?site]. *)

  type instruction =
    t
    -> Ocsigen_extensions.virtual_hosts
    -> Ocsigen_extensions.config_info
    -> Ocsigen_lib.Url.path
    -> Ocsigen_extensions.extension
  (** Instructions are defined by extensions, and correspond to the 
      configuration file options defined by extensions (<staticmod/> ...)*)

  val register : ?site:t -> instruction -> unit
  (** [register t s e] registers instruction [e] to be run inside site [s]. 
      Use this if you want to create an extension yourself. *)
end
