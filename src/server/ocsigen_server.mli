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

module type Config_nested = sig
  type t
  type 'a key

  val key : ?preprocess:('a -> 'a) -> unit -> 'a key
  val find : t -> 'a key -> 'a option
  val set : t -> 'a key -> 'a -> unit
  val unset : t -> 'a key -> unit

  type accessor = {accessor : 'a. 'a key -> 'a option}
end

(** Use this to create an extension that can be linked statically,
    and used without configuration file. *)
module Site : sig
  type t

  val create :
     ?config_info:Ocsigen_extensions.config_info
    -> ?id:[`Attach of t * Ocsigen_lib.Url.path | `Host of string * int option]
    -> ?charset:Ocsigen_charset_mime.charset
    -> ?auto_load_instructions:bool
    -> unit
    -> t
  (** [create ?config_info ?id ?charset ?auto_load_extensions ()]
  creates a subsite.
  This is equivalent to the [<host>] or [<site>] config file options.
  *)

  module Config : Config_nested with type t := t

  type instruction
  (** Instructions are defined by extensions, and correspond to the 
      configuration file options defined by extensions (<staticmod/> ...)*)

  val create_instruction :
     (Config.accessor -> Ocsigen_extensions.extension)
    -> instruction
  (** [create_instruction] makes it possible to use your extension without
      configuration file *)

  val register : t -> instruction -> unit
  (** [register t s e] registers instruction [e] to be run inside site [s] *)

  (**/**)

  val create_instruction_intrusive :
     (Ocsigen_extensions.virtual_hosts
      -> Ocsigen_extensions.config_info
      -> Ocsigen_lib.Url.path
      -> Config.accessor
      -> Ocsigen_extensions.extension)
    -> instruction
  (** Lower-level interface for creating instructions that gives the
      instruction more info. To be avoided. Currently used by Eliom. *)
end
