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

(** Reload the configuration of the server. The optional parameter
    [?file] may be used to read the configuration from another
    file. *)
val reload : ?file : string -> unit -> unit

(** Start the server. Never returns. *)
val start : ?config : Xml.xml list list -> unit -> unit

module type Config_nested = sig

  type t

  type 'a key

  val key : ?preprocess:('a -> 'a) -> unit -> 'a key

  val find : t -> 'a key -> 'a option

  val set : t -> 'a key -> 'a -> unit

  val unset : t -> 'a key -> unit

  type accessor = { accessor : 'a . 'a key -> 'a option }

end

module Site : sig

  type t

  val create :
    ?config_info:Ocsigen_extensions.config_info ->
    ?id:
      [ `Attach of t * Ocsigen_lib.Url.path
      | `Host of string * int option ] ->
    ?charset:Ocsigen_charset_mime.charset ->
    ?auto_load_extensions:bool ->
    unit -> t

  module Config : Config_nested with type t := t

  type extension

  val create_extension :
    (Config.accessor -> Ocsigen_extensions.extension) -> extension

  val register :
    t -> extension -> unit

  (**/**)

  (** Lower-level interface for creating extensions that gives the
      extension more info. To be avoided. Currently used by Eliom. *)
  val create_extension_intrusive :
    (Ocsigen_extensions.virtual_hosts ->
     Ocsigen_extensions.config_info ->
     Ocsigen_lib.Url.path ->
     Config.accessor ->
     Ocsigen_extensions.extension) ->
    extension

end
