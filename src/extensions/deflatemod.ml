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
(*****************************************************************************)
(*****************************************************************************)
(* This module allows to compress output sent by the server                  *)
(*****************************************************************************)
(*****************************************************************************)

open Ocsigen_lib
open Lwt
open Ocsigen_extensions
open Simplexmlparser
open Ocsigen_headers

let section = Lwt_log.Section.make "ocsigen:ext:deflate"

(* Content-type *)
type filter = Type of string option * string option | Extension of string
type compress_choice = All_but of filter list |
                       Compress_only of filter list

let should_compress (t, t') url choice_list =
  let check = function
    |Type (None, None) -> true
    |Type (None, Some x') -> x' = t'
    |Type (Some x, None) -> x = t
    |Type (Some x, Some x') -> x = t && x' = t'
    |Extension suff -> Filename.check_suffix url suff
  in
  match choice_list with
  |Compress_only l -> List.exists check l
  |All_but l -> List.for_all (fun c -> not (check c)) l

(* Pas de filtre global pour l'instant
   let choice_list = ref (All_but [])
*)

(** Compression *)

let buffer_size = ref 8192

(*  0 = no compression ; 1 = best speed ; 9 = best compression *)
let compress_level = ref 6

(* Minimal header, by X. Leroy *)
let gzip_header_length = 10
let gzip_header = Bytes.make gzip_header_length (Char.chr 0)
let () =
  Bytes.set gzip_header 0 @@ Char.chr 0x1F;
  Bytes.set gzip_header 1 @@ Char.chr 0x8B;
  Bytes.set gzip_header 2 @@ Char.chr 8;
  Bytes.set gzip_header 9 @@ Char.chr 0xFF


(* inspired by an auxiliary function from camlzip, by Xavier Leroy *)
type output_buffer =
  {
    stream: Zlib.stream;
    buf: string;
    mutable pos: int;
    mutable avail: int;
    mutable size : int32;
    mutable crc : int32;
    mutable add_trailer : bool
  }

let write_int32 oz n =
  for i = 0 to 3 do
    Bytes.set oz.buf (oz.pos + i)
      (Char.chr (Int32.to_int (Int32.shift_right_logical n (8 * i)) land 0xff))
  done;
  oz.pos <- oz.pos + 4;
  oz.avail <- oz.avail - 4;
  assert (oz.avail >= 0)

(* puts in oz the content of buf, from pos to pos + len ;
 * f is the continuation of the current stream *)
let rec output oz f buf pos len  =
  assert (pos >= 0 && len >= 0 && pos + len <= String.length buf);
  if oz.avail = 0 then begin
    let cont () = output oz f buf pos len in
    Lwt_log.ign_info ~section "Flushing because output buffer is full";
    flush oz cont
  end else if len = 0 then
    next_cont oz f
  else begin
    let  (_, used_in, used_out) =
      try
        Zlib.deflate
          oz.stream buf pos len oz.buf oz.pos oz.avail Zlib.Z_NO_FLUSH
      with Zlib.Error(s, s') ->
        raise
          (Ocsigen_stream.Stream_error("Error during compression: "^s^" "^s'))
    in
    oz.pos <- oz.pos + used_out;
    oz.avail <- oz.avail - used_out;
    oz.size <- Int32.add oz.size (Int32.of_int used_in);
    oz.crc <- Zlib.update_crc oz.crc buf pos used_in;
    output oz f buf (pos + used_in) (len - used_in)
  end

(* Flush oz, ie. produces a new_stream with the content of oz, cleans it
 * and returns the continuation of the stream *)
and flush oz cont =
  let len = oz.pos in
  if len = 0 then
    cont ()
  else begin
    let buf_len = Bytes.length oz.buf in
    let s = if len = buf_len then oz.buf else Bytes.sub oz.buf 0 len in
    Lwt_log.ign_info ~section "Flushing!";
    oz.pos <- 0 ;
    oz.avail <- buf_len;
    Ocsigen_stream.cont s cont
  end

and next_cont oz stream =
  Ocsigen_stream.next stream >>= fun e ->
  match e with
  | Ocsigen_stream.Finished None ->
    Lwt_log.ign_info ~section "End of stream: big cleaning for zlib" ;

    (* loop until there is nothing left to compress and flush *)
    let rec finish () =
      (* buffer full *)
      if oz.avail = 0 then flush oz finish
      else (
        (* no more input, deflates only what were left because output buffer
         * was full *)
        let (finished, _, used_out) =
          Zlib.deflate oz.stream oz.buf 0 0 oz.buf oz.pos oz.avail Zlib.Z_FINISH
        in
        oz.pos <- oz.pos + used_out;
        oz.avail <- oz.avail - used_out;
        if not finished then
          finish ()
        else
          write_trailer ())
    and write_trailer () =
      if oz.add_trailer && oz.avail < 8 then
        flush oz write_trailer
      else begin
        if oz.add_trailer then begin
          write_int32 oz oz.crc;
          write_int32 oz oz.size
        end;
        Lwt_log.ign_info ~section "Zlib.deflate finished, last flush";
        flush oz (fun () -> Ocsigen_stream.empty None)
      end
    in
    finish ()
  | Ocsigen_stream.Finished (Some s) -> next_cont oz s
  | Ocsigen_stream.Cont(s,f) ->
    output oz f s 0 (String.length s)

(* deflate param : true = deflate ; false = gzip (no header in this case) *)
let compress deflate stream =
  let zstream = Zlib.deflate_init !compress_level deflate in
  let finalize status =
    Ocsigen_stream.finalize stream status >>= fun e ->
    (try
      Zlib.deflate_end zstream
    with
      (* ignore errors, deflate_end cleans everything anyway *)
      Zlib.Error _ -> ());
    return (Lwt_log.ign_info ~section "Zlib stream closed") in
  let oz =
    { stream = zstream ;
      buf = Bytes.create !buffer_size;
      pos = 0;
      avail = !buffer_size;
      size = 0l; crc = 0l; add_trailer = not deflate
    } in
  let new_stream () = next_cont oz (Ocsigen_stream.get stream) in
  Lwt_log.ign_info ~section "Zlib stream initialized" ;
  if deflate then
    Ocsigen_stream.make ~finalize new_stream
  else
    Ocsigen_stream.make
      ~finalize (fun () -> Ocsigen_stream.cont gzip_header new_stream)


(*****************************************************************************)
(** The filter function *)
(* We implement Content-Encoding, not Transfer-Encoding *)

type encoding = Deflate | Gzip | Id | Star | Not_acceptable

let qvalue = function Some x -> x |None -> 1.0

let enc_compare e e' = match e,e' with
  |(Star,_),(_,_) -> -1 (* star should be at the very end *)
  |(_,_),(Star,_) -> 1
  |(_,v),(_,v') when v<v' -> 1 (* then, sort by qvalue *)
  |(_,v),(_,v') when v>v' -> -1
  |(x,_),(x',_) when x=x' -> 0
  |(Deflate,_),(_,_) -> 1 (* and subsort by encoding *)
  |(_,_),(Deflate,_) -> -1
  |(Gzip,_),(_,_) -> 1
  |(_,_),(Gzip,_) -> -1
  |(Id,_),(_,_) -> 1
  |(_,_),(Id,_) -> -1
  |_ -> assert false

let rec filtermap f = function
  |[] -> []
  |t::q -> match f t with
    |Some s -> s::(filtermap f q)
    |None -> filtermap f q

let convert = function
  |(Some "deflate",v) -> Some (Deflate, qvalue v)
  |(Some "gzip",v)|(Some "x-gzip",v) -> Some (Gzip, qvalue v)
  |(Some "identity",v) -> Some (Id, qvalue v)
  |(None,v) -> Some (Star, qvalue v)
  |_ -> None

(* Follow http's RFC to select the transfert encoding *)
let select_encoding accept_header =
  let h = List.sort enc_compare (filtermap convert accept_header) in
  let (exclude,accept) =
    let (e,a) = List.partition (fun x -> snd x = 0.) h in
    (List.map fst e, List.map fst a) in
  let rec aux = function
    |[] ->
      if ((List.mem Star exclude) || (List.mem Id exclude))
      then Not_acceptable else Id
    |t::q ->
      if (List.mem t exclude)
      then aux q else t
  in
  aux accept

exception No_compress

(* deflate = true -> mode deflate
 * deflate = false -> mode gzip *)
let stream_filter contentencoding url deflate choice res =
  return (Ext_found (fun () ->
      try (
        match Ocsigen_http_frame.Result.content_type res with
        | None -> raise No_compress (* il faudrait défaut ? *)
        | Some contenttype ->
          match Ocsigen_headers.parse_mime_type contenttype with
          | None, _ | _, None -> raise No_compress (* should never happen? *)
          | (Some a, Some b)
            when should_compress (a, b) url choice ->
            return
              (Ocsigen_http_frame.Result.update res
                 ~content_length:None
                 ~etag:
                   (match Ocsigen_http_frame.Result.etag res with
                    | Some e ->
                      Some ((if deflate then "Ddeflatemod" else "Gdeflatemod")^e)
                    | None -> None)
                 ~stream:
                   (compress deflate (fst (Ocsigen_http_frame.Result.stream res)), None)
                 ~headers:
                   (Http_headers.replace
                      Http_headers.content_encoding
                      contentencoding (Ocsigen_http_frame.Result.headers res)) ())
          | _ -> raise No_compress)
      with Not_found | No_compress -> return res))

let filter choice_list = function
  | Req_not_found (code,_) -> return (Ext_next code)
  | Req_found ({ request_info = ri }, res) ->
    match select_encoding (Lazy.force(Ocsigen_request_info.accept_encoding ri)) with
    | Deflate ->
      stream_filter "deflate" (Ocsigen_request_info.sub_path_string ri) true choice_list res
    | Gzip ->
      stream_filter "gzip" (Ocsigen_request_info.sub_path_string ri)  false choice_list res
    | Id | Star -> return (Ext_found (fun () -> return res))
    | Not_acceptable ->
      return (Ext_stop_all (Ocsigen_http_frame.Result.cookies res,406))


(*****************************************************************************)

let rec parse_global_config = function
  | [] -> ()
  | (Element ("compress", [("level", l)], []))::ll ->
    let l = try int_of_string l
      with Failure _ -> raise (Error_in_config_file
                                 "Compress level should be an integer between 0 and 9") in
    compress_level := if (l <= 9 && l >= 0) then l else 6 ;
    parse_global_config ll
  | (Element ("buffer", [("size", s)], []))::ll ->
    let s = (try int_of_string s
             with Failure _ -> raise (Error_in_config_file
                                        "Buffer size should be a positive integer")) in
    buffer_size := if s > 0 then s else 8192 ;
    parse_global_config ll
  (* TODO: Pas de filtre global pour l'instant
   * le nom de balise contenttype est mauvais, au passage
     | (Element ("contenttype", [("compress", b)], choices))::ll ->
       let l = (try parse_filter choices
               with Not_found -> raise (Error_in_config_file
                    "Can't parse mime-type content")) in
       (match b with
       |"only" -> choice_list := Compress_only l
       |"allbut" -> choice_list := All_but l
       | _ ->  raise (Error_in_config_file
       "Attribute \"compress\" should be \"allbut\" or \"only\""));
       parse_global_config ll
  *)
  | _ -> raise (Error_in_config_file
                  "Unexpected content inside deflatemod config")

(*****************************************************************************)

let parse_config config_elem =
  let mode = ref (Compress_only []) in
  let pages = ref [] in
  Ocsigen_extensions.(
    Configuration.process_element
      ~in_tag:"host"
      ~other_elements:(fun t _ _ -> raise (Bad_config_tag_for_extension t))
      ~elements:[
        Configuration.element
          ~name:"deflate"
          ~attributes:[
            Configuration.attribute
              ~name:"compress"
              ~obligatory:true
              (function
                | "only" -> mode := Compress_only []
                | "allbut" -> mode := All_but []
                | _ ->
                  badconfig
                    "Attribute 'compress' should be 'allbut' or 'only'"
              );
          ]
          ~elements:[
            Configuration.element
              ~name:"type"
              ~pcdata:(fun s ->
                let (a, b) = Ocsigen_headers.parse_mime_type s in
                pages := Type (a, b) :: !pages) ();
            Configuration.element
              ~name:"extension"
              ~pcdata:(fun s ->
                pages := Extension s :: !pages) ();

          ]
          ()]
      config_elem
  );
  match !pages with
  | [] ->
    badconfig
      "Unexpected element inside contenttype (should be <type> or <extension>)"
  | l ->
    let mode = match !mode with
      | Compress_only __ -> Compress_only l
      | All_but _ -> All_but l
    in filter mode

(*****************************************************************************)
(** Registration of the extension *)
let () = Ocsigen_extensions.register_extension
    ~name:"deflatemod"
    ~fun_site:(fun _ _ _ _ _ -> parse_config)
    ~init_fun:parse_global_config
    ()
