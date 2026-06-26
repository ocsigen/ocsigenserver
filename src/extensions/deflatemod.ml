(* Ocsigen
 * http://www.ocsigen.org
 * Module deflatemod.ml
 * Copyright (C) 2007 Gabriel Kerneis
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

(* Compress output sent by the server *)

open Lwt.Infix

let section = Logs.Src.create "ocsigen:ext:deflate"

(* Content-type *)
type filter = [`Type of string option * string option | `Extension of string]

let should_compress (t, t') url choice_list =
  let check = function
    | `Type (None, None) -> true
    | `Type (None, Some x') -> x' = t'
    | `Type (Some x, None) -> x = t
    | `Type (Some x, Some x') -> x = t && x' = t'
    | `Extension suff -> Filename.check_suffix url suff
  in
  match choice_list with
  | `Only l -> List.exists check l
  | `All_but l -> List.for_all (fun c -> not (check c)) l

let compress_level = ref 6
let set_compress_level i = compress_level := if i >= 0 && i <= 9 then i else 6
let buffer_size = ref 8192
let set_buffer_size s = buffer_size := if s > 0 then s else 8192

(* Compression backend, provided by bytesrw. A codec is given as a bytesrw
   writer filter (see [compress_body]). *)

let stream_error e =
  Ocsigen_base.Ocsigen_stream.Stream_error
    ("Error during compression: " ^ Bytesrw.Bytes.Stream.error_message e)

(* Compress [body] with the bytesrw writer filter [cfilter], pushing each
   produced chunk to [flush].

   bytesrw writers are synchronous, whereas [flush] is an Lwt operation. We let
   the compressor write its output into an in-memory buffer, then flush that
   buffer downstream after each input chunk. This bridges the synchronous
   compressor with the Lwt output stream without ever blocking. *)
let compress_body (cfilter : Bytesrw.Bytes.Writer.filter) body =
 fun flush ->
  let out = Buffer.create !buffer_size in
  let cw = cfilter ~eod:true (Bytesrw.Bytes.Writer.of_buffer out) in
  let flush_out () =
    if Buffer.length out = 0
    then Lwt.return_unit
    else (
      let s = Buffer.contents out in
      Buffer.clear out; flush s)
  in
  let write f =
    try f () with Bytesrw.Bytes.Stream.Error e -> raise (stream_error e)
  in
  body (fun inbuf ->
    write (fun () -> Bytesrw.Bytes.Writer.write_string cw inbuf);
    flush_out ())
  >>= fun () ->
  write (fun () -> Bytesrw.Bytes.Writer.write_eod cw);
  Logs.debug ~src:section (fun fmt -> fmt "Close stream");
  flush_out ()

(* We implement Content-Encoding, not Transfer-Encoding *)
type encoding = Deflate | Gzip | Id | Star | Not_acceptable

let qvalue = function Some x -> x | None -> 1.0

let enc_compare e e' =
  match e, e' with
  | (Star, _), (_, _) -> -1 (* star should be at the very end *)
  | (_, _), (Star, _) -> 1
  | (_, v), (_, v') when v < v' -> 1 (* then, sort by qvalue *)
  | (_, v), (_, v') when v > v' -> -1
  | (x, _), (x', _) when x = x' -> 0
  | (Deflate, _), (_, _) -> 1 (* and subsort by encoding *)
  | (_, _), (Deflate, _) -> -1
  | (Gzip, _), (_, _) -> 1
  | (_, _), (Gzip, _) -> -1
  | (Id, _), (_, _) -> 1
  | (_, _), (Id, _) -> -1
  | _ -> assert false

let rec filtermap f = function
  | [] -> []
  | t :: q -> (
    match f t with Some s -> s :: filtermap f q | None -> filtermap f q)

let convert = function
  | Some "deflate", v -> Some (Deflate, qvalue v)
  | Some "gzip", v | Some "x-gzip", v -> Some (Gzip, qvalue v)
  | Some "identity", v -> Some (Id, qvalue v)
  | None, v -> Some (Star, qvalue v)
  | _ -> None

(* Follow http's RFC to select the transfer encoding *)
let select_encoding accept_header =
  let h = List.sort enc_compare (filtermap convert accept_header) in
  let exclude, accept =
    let e, a = List.partition (fun x -> snd x = 0.) h in
    List.map fst e, List.map fst a
  in
  let rec aux = function
    | [] ->
        if List.mem Star exclude || List.mem Id exclude
        then Not_acceptable
        else Id
    | t :: q -> if List.mem t exclude then aux q else t
  in
  aux accept

(* [cfilter] is the bytesrw writer filter for the negotiated codec,
   [contentencoding] the matching Content-Encoding token, [etag_prefix] a
   per-codec tag prepended to the ETag so that variants stay distinct. *)
let stream_filter cfilter contentencoding etag_prefix url choice res =
  Lwt.return
    (Ocsigen.Extensions.Ext_found
       (fun () ->
         try
           match
             Ocsigen.Response.header res Ocsigen_http.Header.Name.content_type
           with
           | None -> Lwt.return res
           | Some contenttype -> (
               let contenttype =
                 try String.sub contenttype 0 (String.index contenttype ';')
                 with Not_found -> contenttype
               in
               match Ocsigen_http.Header.Mime_type.parse contenttype with
               | None, _ | _, None -> Lwt.return res
               | Some a, Some b when should_compress (a, b) url choice ->
                   let response =
                     let {Http.Response.headers; status; version} =
                       Ocsigen.Response.response res
                     in
                     let headers =
                       let name = Ocsigen_http.Header.Name.(to_string etag) in
                       match Cohttp.Header.get headers name with
                       | Some e ->
                           Cohttp.Header.replace headers name (etag_prefix ^ e)
                       | None -> headers
                     in
                     let headers =
                       Http.Header.replace headers
                         Ocsigen_http.Header.Name.(to_string content_encoding)
                         contentencoding
                     in
                     Http.Response.make ~headers ~status ~version ()
                   and body =
                     Ocsigen.Response.Body.make Cohttp.Transfer.Chunked
                       (compress_body cfilter
                          (Ocsigen.Response.Body.write
                             (Ocsigen.Response.body res)))
                   in
                   Lwt.return (Ocsigen.Response.update res ~body ~response)
               | _ -> Lwt.return res)
         with Not_found -> Lwt.return res))

let filter choice_list = function
  | Ocsigen.Extensions.Req_not_found (code, _) ->
      Lwt.return (Ocsigen.Extensions.Ext_next code)
  | Ocsigen.Extensions.Req_found ({Ocsigen.Extensions.request_info = ri; _}, res)
    -> (
    match
      Ocsigen.Request.header_multi ri Ocsigen_http.Header.Name.accept_encoding
      |> Ocsigen_http.Header.Accept_encoding.parse |> select_encoding
    with
    | Deflate ->
        (* HTTP "deflate" is the zlib format (RFC 1950), not raw deflate. *)
        stream_filter
          (Bytesrw_zlib.Zlib.compress_writes ~level:!compress_level ())
          "deflate" "Ddeflatemod"
          (Ocsigen.Request.sub_path_string ri)
          choice_list res
    | Gzip ->
        stream_filter
          (Bytesrw_zlib.Gzip.compress_writes ~level:!compress_level ())
          "gzip" "Gdeflatemod"
          (Ocsigen.Request.sub_path_string ri)
          choice_list res
    | Id | Star ->
        Lwt.return (Ocsigen.Extensions.Ext_found (fun () -> Lwt.return res))
    | Not_acceptable ->
        Lwt.return
          (Ocsigen.Extensions.Ext_stop_all
             (Ocsigen.Response.cookies res, `Not_acceptable)))

let rec parse_global_config = function
  | [] -> ()
  | Xml.Element ("compress", [("level", i)], []) :: ll ->
      let i =
        try int_of_string i
        with Failure _ ->
          raise
            (Ocsigen.Extensions.Error_in_config_file
               "Compress level should be an integer between 0 and 9")
      in
      set_compress_level i; parse_global_config ll
  | Xml.Element ("buffer", [("size", s)], []) :: ll ->
      let s =
        try int_of_string s
        with Failure _ ->
          raise
            (Ocsigen.Extensions.Error_in_config_file
               "Buffer size should be a positive integer")
      in
      set_buffer_size s; parse_global_config ll
  | _ ->
      raise
        (Ocsigen.Extensions.Error_in_config_file
           "Unexpected content inside deflatemod config")

let parse_config config_elem =
  let mode = ref `Only in
  let pages = ref [] in
  Ocsigen.Extensions.(
    Configuration.process_element ~in_tag:"host"
      ~other_elements:(fun t _ _ -> raise (Bad_config_tag_for_extension t))
      ~elements:
        [ Configuration.element ~name:"deflate"
            ~attributes:
              [ Configuration.attribute ~name:"compress" ~obligatory:true
                  (function
                  | "only" -> mode := `Only
                  | "allbut" -> mode := `All_but
                  | _ ->
                      badconfig
                        "Attribute 'compress' should be 'allbut' or 'only'") ]
            ~elements:
              [ Configuration.element ~name:"type"
                  ~pcdata:(fun s ->
                    let a, b = Ocsigen_http.Header.Mime_type.parse s in
                    pages := `Type (a, b) :: !pages)
                  ()
              ; Configuration.element ~name:"extension"
                  ~pcdata:(fun s -> pages := `Extension s :: !pages)
                  () ]
            () ]
      config_elem);
  match !pages with
  | [] ->
      Ocsigen.Extensions.badconfig
        "Unexpected element inside contenttype (should be <type> or <extension>)"
  | l -> filter (match !mode with `Only -> `Only l | `All_but -> `All_but l)

let () =
  Ocsigen.Extensions.register ~name:"deflatemod"
    ~fun_site:(fun _ _ _ _ _ _ -> parse_config)
    ~init_fun:parse_global_config ()

let run ~mode () _ _ _ = filter mode
