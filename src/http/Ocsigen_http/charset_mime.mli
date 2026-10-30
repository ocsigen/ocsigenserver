(* Ocsigen
 * http://www.ocsigen.org
 * ocsigen_charset_mime.mli Copyright (C) 2008
 * Boris Yakobowski
 * Laboratoire PPS - CNRS Université Paris Diderot
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

type extension = string
type file = string
type filename = string

(** Charset *)

type charset = string
(** By convention, "no specified charset" is represented by the empty
    string *)

val no_charset : charset

type charset_assoc
(** Association between extensions and charset, with a default value. *)

val empty_charset_assoc : ?default:charset -> unit -> charset_assoc
(** All files are mapped to [no_charset] *)

val empty_mime_assoc : ?default:charset -> unit -> charset_assoc
val find_charset : string -> charset_assoc -> charset

val default_charset : charset_assoc -> charset
(** Functions related to the default charset in the association *)

val set_default_charset : charset_assoc -> charset -> charset_assoc

val update_charset_ext : charset_assoc -> extension -> charset -> charset_assoc
(** Updates the mapping between extensions from a file to its charset.
    The update can be specified using the extension of the file,
    the name of the file, or the entire file (with its path)
*)

val update_charset_file : charset_assoc -> filename -> charset -> charset_assoc

val update_charset_regexp :
   charset_assoc
  -> Re.Pcre.regexp
  -> charset
  -> charset_assoc

type mime_type = string
(** MIME types; the default value is ["application/octet-stream"] *)

val default_mime_type : mime_type

type mime_assoc
(** association between extensions and mime types, with default value *)

val default_mime_assoc : unit -> mime_assoc
(** [default_mime_assoc ()] has no rules, and the default type
    {!default_mime_type}. The mime.types file of {!set_mime_file} is not part of
    it: {!find_mime} looks it up after the rules. *)

val parse_mime_types : filename:string -> mime_assoc
(** Parsing of a file containing mime associations, such as /etc/mime.types *)

val set_mime_file : string option -> unit
(** [set_mime_file file] sets the mime.types file whose types come after the
    rules (see {!find_mime}), or none. The file is read at the next call of
    {!find_mime}. There is none by default. *)

val mime_file : unit -> string option
(** [mime_file ()] is the file that {!set_mime_file} sets. *)

(* The other functions are as for charsets *)

val find_mime : file -> mime_assoc -> string
(** [find_mime file assoc] is the MIME type of [file]: the one of the first
    rule of [assoc] that matches [file] (its extension, its name or a regular
    expression), else the one of the mime.types file of {!set_mime_file}, else
    the default type of [assoc]. *)

val default_mime : mime_assoc -> mime_type
val set_default_mime : mime_assoc -> mime_type -> mime_assoc
val update_mime_ext : mime_assoc -> extension -> mime_type -> mime_assoc
val update_mime_file : mime_assoc -> filename -> mime_type -> mime_assoc
val update_mime_regexp : mime_assoc -> Re.Pcre.regexp -> mime_type -> mime_assoc
