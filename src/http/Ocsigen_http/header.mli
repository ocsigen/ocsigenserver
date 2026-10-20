(* Ocsigen
 * http://www.ocsigen.org
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

type t = Cohttp.Header.t

val of_option : t option -> t

module Name : sig
  type t

  val to_string : t -> string
  val of_string : string -> t
  val accept : t
  val accept_charset : t
  val accept_encoding : t
  val accept_language : t
  val accept_ranges : t
  val allow : t
  val authorization : t
  val cache_control : t
  val connection : t
  val content_disposition : t
  val content_encoding : t
  val content_length : t
  val content_type : t
  val content_range : t
  val cookie : t
  val date : t
  val etag : t
  val expect : t
  val expires : t
  val host : t
  val if_match : t
  val if_modified_since : t
  val if_none_match : t
  val if_unmodified_since : t
  val if_range : t
  val last_modified : t
  val location : t
  val pragma : t
  val server : t
  val set_cookie : t
  val status : t
  val transfer_encoding : t
  val vary : t
  val user_agent : t
  val referer : t
  val range : t
  val x_forwarded_for : t
  val x_forwarded_proto : t
  val origin : t
  val access_control_request_method : t
  val access_control_request_headers : t
  val access_control_allow_origin : t
  val access_control_allow_credentials : t
  val access_control_expose_headers : t
  val access_control_max_age : t
  val access_control_allow_methods : t
  val access_control_allow_headers : t
end

val connection_options : t -> string list
(** [connection_options h] is the list of the options of the Connection header
    of [h], such as [close], [keep-alive] or the names of the headers meant for
    the current connection only, in lowercase (RFC 9110, section 7.6.1). *)

module Mime_type : sig
  type t = string option * string option

  val parse : string -> t
end

module Accept : sig
  type t = (Mime_type.t * float option * (string * string) list) list

  val parse : string list -> t
end

module Accept_encoding : sig
  type t = (string option * float option) list

  val parse : string list -> t
end

module Accept_language : sig
  type t = (string * float option) list

  val parse : string list -> t
end

module Content_type : sig
  val choose : Accept.t -> string -> string list -> string
end

(** Entity tags (RFC 9110, section 8.8.3). *)
module Entity_tag : sig
  type t = {weak : bool; opaque : string}
  (** [opaque] is the tag without its quotes. *)

  val to_string : t -> string
  (** [to_string t] is [t] as written in a header, such as [W/"1-2"]. *)

  val parse_list : string list -> [`Any | `Tags of t list]
  (** [parse_list values] is the value of an [If-Match] or [If-None-Match]
      header field written on the lines [values]: [`Any] when it is just
      ["*"], otherwise the list of its entity tags. Elements that are not
      entity tags are ignored. *)

  val weak_equal : t -> t -> bool
  (** [weak_equal a b] is the weak comparison of [a] and [b]: their opaque tags
      are equal, whether they are weak or not (section 8.8.3.2). *)

  val strong_equal : t -> t -> bool
  (** [strong_equal a b] is the strong comparison of [a] and [b]: they are not
      weak and their opaque tags are equal. *)
end
