(* Ocsigen
 * http://www.ocsigen.org
 * sender_helpers.ml Copyright (C) 2005 Denis Berthod
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
(** Functions to create results for various kinds of documents *)


module File_content : Ocsigen_http_frame.HTTP_CONTENT
  with  type t =
    string * Ocsigen_charset_mime.charset_assoc * Ocsigen_charset_mime.mime_assoc

module Html_content :
  Ocsigen_http_frame.HTTP_CONTENT with type t = Tyxml.Html.doc

module Make_XML_Content(Xml : Xml_sigs.Iterable)
    (Typed_xml : Xml_sigs.Typed_xml with module Xml := Xml) :
  Ocsigen_http_frame.HTTP_CONTENT
  with type t = Typed_xml.doc
   and type options = Http_headers.accept Lazy.t


(** content * content-type *)
module Text_content :
  Ocsigen_http_frame.HTTP_CONTENT with type t = string * string

module Stream_content :
  Ocsigen_http_frame.HTTP_CONTENT with type t = bytes Ocsigen_stream.t

(** streams and content-type *)
module Streamlist_content :
  Ocsigen_http_frame.HTTP_CONTENT
  with type t = (unit -> bytes Ocsigen_stream.t Lwt.t) list
                * string

module Empty_content :
  Ocsigen_http_frame.HTTP_CONTENT with type t = unit

(** directory name and corresponding URL path *)
module Directory_content :
  Ocsigen_http_frame.HTTP_CONTENT with type t = string * string list

(** error code and/or exception *)
module Error_content :
  Ocsigen_http_frame.HTTP_CONTENT
  with type t = int option * exn option * Ocsigen_cookies.cookieset



(** Sending an error page *)
val send_error :
  ?code:int ->
  ?exn:exn ->
  Ocsigen_http_com.slot ->
  clientproto:Ocsigen_http_frame.Http_header.proto ->
  ?mode:Ocsigen_http_frame.Http_header.http_mode ->
  ?proto:Ocsigen_http_frame.Http_header.proto ->
  ?cookies:Ocsigen_cookies.cookieset ->
  head:bool ->
  sender:Ocsigen_http_com.sender_type ->
  unit ->
  unit Lwt.t
