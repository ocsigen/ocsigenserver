(* Ocsigen
 * http://www.ocsigen.org
 * ocsigen_charset_mime.ml Copyright (C) 2008
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

open Ocsigen_base.Lib
module MapString = Map.Make (String)

type extension = string
type filename = string
type file = string

let section = Logs.Src.create "ocsigen:mimetype"

type 'a assoc_item =
  | Extension of extension * 'a
  | File of filename * 'a
  | Regexp of Re.Pcre.regexp * 'a
  | Map of 'a MapString.t

type 'a assoc = {assoc_list : 'a assoc_item list; assoc_default : 'a}

let extension file =
  try String.lowercase_ascii (Filename.extension_no_directory file)
  with Not_found -> ""

let find_in_assoc_opt file assoc =
  let filename = Filename.basename file in
  let ext = extension file in
  let rec aux = function
    | [] -> None
    | Extension (ext', v) :: q -> if ext = ext' then Some v else aux q
    | File (filename', v) :: q -> if filename = filename' then Some v else aux q
    | Regexp (reg, v) :: q ->
        if Netstring_pcre.string_match reg file 0 <> None then Some v else aux q
    | Map m :: q -> (
      match MapString.find_opt ext m with Some v -> Some v | None -> aux q)
  in
  aux assoc.assoc_list

let find_in_assoc file assoc =
  Option.value (find_in_assoc_opt file assoc) ~default:assoc.assoc_default

let default assoc = assoc.assoc_default
let set_default assoc default = {assoc with assoc_default = default}

let update_ext assoc (ext : extension) v =
  { assoc with
    assoc_list = Extension (String.lowercase_ascii ext, v) :: assoc.assoc_list
  }

let update_file assoc (file : filename) v =
  {assoc with assoc_list = File (file, v) :: assoc.assoc_list}

let update_regexp assoc r v =
  {assoc with assoc_list = Regexp (r, v) :: assoc.assoc_list}

let empty default () = {assoc_list = []; assoc_default = default}

(* Handling of charset and mime ; specific values and declarations *)

type charset = string
type mime_type = string
type charset_assoc = charset assoc
type mime_assoc = mime_type assoc

let no_charset : charset = ""
let default_mime_type : mime_type = "application/octet-stream"
let empty_charset_assoc ?(default = no_charset) = empty default
let empty_mime_assoc ?(default = default_mime_type) = empty default

(* Generic functions *)

let default_charset = default
let default_mime = default
let update_charset_ext = update_ext
let update_mime_ext = update_ext
let update_charset_file = update_file
let update_mime_file = update_file
let update_charset_regexp = update_regexp
let update_mime_regexp = update_regexp
let set_default_mime = set_default
let set_default_charset = set_default
let find_charset = find_in_assoc

(* Specific handling of content-type *)

let read_mime_types filename =
  let rec read_and_split mimemap in_ch =
    try
      let line = input_line in_ch in
      let line_upto =
        try
          let upto = String.index line '#' in
          String.sub line 0 upto
        with Not_found -> line
      in
      let strlist =
        Netstring_pcre.split (Netstring_pcre.regexp "\\s+") line_upto
      in
      match strlist with
      | [] | [_] -> (* No extension on this line *) read_and_split mimemap in_ch
      | mime :: extensions ->
          let mimemap =
            List.fold_left
              (fun mimemap ext -> MapString.add ext mime mimemap)
              mimemap extensions
          in
          read_and_split mimemap in_ch
    with End_of_file -> mimemap
  in
  try In_channel.with_open_text filename (read_and_split MapString.empty)
  with Sys_error msg ->
    Logs.err ~src:section (fun fmt ->
      fmt "Unable to read the mime.types file: %s" msg);
    MapString.empty

let parse_mime_types ~filename : mime_type assoc =
  { assoc_list = [Map (read_mime_types filename)]
  ; assoc_default = default_mime_type }

let default_mime_assoc () = empty_mime_assoc ()

(* The mime.types file of <mimefile>, and its table, read at the first lookup:
   the logs are open by then, and a reload, which sets the file again, reads
   it again. *)
let mime_file = ref None
let mime_file_table = ref (lazy MapString.empty)

let set_mime_file file =
  mime_file := file;
  mime_file_table :=
    lazy
      (match file with
      | None -> MapString.empty
      | Some filename ->
          Logs.info ~src:section (fun fmt ->
            fmt "Loading mime types in '%s'" filename);
          read_mime_types filename)

let mime_file () = !mime_file

let find_mime file assoc =
  match find_in_assoc_opt file assoc with
  | Some mime -> mime
  | None -> (
    match MapString.find_opt (extension file) (Lazy.force !mime_file_table) with
    | Some mime -> mime
    | None -> assoc.assoc_default)
