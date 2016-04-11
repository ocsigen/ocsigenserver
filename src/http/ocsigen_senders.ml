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
(** This module provides predefined "senders" for usual types of pages to be
    sent by the server: xhtml, files, ... *)

open Ocsigen_lib
open Ocsigen_http_frame
open Ocsigen_http_com
open Lwt
open Ocsigen_stream

let section = Lwt_log.Section.make "ocsigen:http:sender"

(*****************************************************************************)
(** this module instantiate the HTTP_CONTENT signature for an Html content*)

module Make_XML_Content(Xml : Xml_sigs.Iterable)
    (Typed_xml : Xml_sigs.Typed_xml with module Xml := Xml)
= struct

  module Htmlprinter =
    Xml_print.Make_typed(Xml)(Typed_xml)(Ocsigen_stream.StringStream)

  type t = Typed_xml.doc
  type options = Http_headers.accept Lazy.t

  let get_etag_aux x = None

  let get_etag ?options c = None

  let choose_content_type accepted alt default =
    match accepted, alt with
    | None, _ | _, [] -> default
    | Some accepted, alt ->
      try
        List.find
          (fun content_type ->
             List.exists
               (function
                 | ((Some a, Some b),_,_) -> a^"/"^b = content_type
                 | _ -> false)
               (Lazy.force accepted))
          (default :: alt)
      with Not_found -> default

  let result_of_content ?options c =
    let content_type =
      choose_content_type options
        Typed_xml.Info.alternative_content_types
        Typed_xml.Info.content_type in
    let encode x = fst (Xml_print.Utf8.normalize_html x) in
    let x = Htmlprinter.print ~encode ~advert c in
    let default_result = Result.default () in
    Lwt.return
      (Result.update default_result
         ~content_length:None
         ~content_type:(Some content_type)
         ~etag:(get_etag c)
         ~charset:(Some "utf-8")
         ~headers:Http_headers.dyn_headers
         ~stream:(x, None) ())
end

module Html5_content = Make_XML_Content(Xml)(Html5.M)


(*****************************************************************************)
module Text_content =
struct
  type t = string (* content *) * string (* content-type *)

  type options = unit

  let get_etag ?options (x, _) = None
  (*      Some (Digest.to_hex (Digest.string x)) *)
  (* We do not add etags here because the content is
     probably generated and there are problemes with etags
     and POST requests:
     when doing POST request with etags, the semantinc
     is not to do side effects on the server when the
     etag match and respond with code 412 ( precondition failed ).
     Since the etag is calculated from the content of the answer,
     we cannot enforce this semantic correctly, so it is better not
     to send etags with POST requests. Here, we cannot know wether
     the request was in POST or GET, so the easiest way to fix that
     is to never send etags *)


  let result_of_content ?(options = ()) ((c, ct) as content) =
    let md5 = get_etag content in
    let default_result = Result.default () in
    Lwt.return
      (Result.update default_result
         ~content_length:(Some (Int64.of_int (String.length c)))
         ~etag:md5
         ~content_type:(Some ct)
         ~headers:Http_headers.dyn_headers
         ~stream:
           (Ocsigen_stream.make
              (fun () ->
                 Ocsigen_stream.cont c (fun () -> Ocsigen_stream.empty None)),
            None) ())
end

(*****************************************************************************)
module Stream_content =
(* Used to send data from a stream *)
struct
  type t = string Ocsigen_stream.t

  type options = unit

  let get_etag ?options c = None

  let result_of_content ?(options = ()) c =
    let default_result = Result.default () in
    Lwt.return
      (Result.update default_result
         ~content_length:None
         ~headers:Http_headers.dyn_headers
         ~stream:(c, None) ())

end

(*****************************************************************************)
module Streamlist_content =
(* Used to send data from streams *)
struct
  type t = (unit -> string Ocsigen_stream.t Lwt.t) list
           * string (* content-type *)

  type options = unit

  let get_etag ?options c = None

    let result_of_content ?(options = ()) (c, ct) =
      let finalizer = ref (fun _ -> Lwt.return ()) in
      let finalize status =
        let f = !finalizer in
        finalizer := (fun _ -> Lwt.return ());
        f status
      in
      let rec next stream l =
        Lwt.try_bind (fun () -> Ocsigen_stream.next stream)
          (fun s ->
             match s with
               Ocsigen_stream.Finished None ->
                 finalize `Success >>= fun () ->
                 next_stream l
             | Ocsigen_stream.Finished (Some stream) ->
                 next stream l
             | Ocsigen_stream.Cont (v, stream) ->
                 Ocsigen_stream.cont v (fun () -> next stream l))
          (function Interrupted e | e ->
(*XXX string_of_exn should know how to print "Interrupted _" exceptions*)
             exnhandler e l)
      and next_stream l =
        match l with
          [] -> Ocsigen_stream.empty None
        | f :: l ->
            Lwt.try_bind f
              (fun stream ->
                 finalizer :=
                   (fun status -> Ocsigen_stream.finalize stream status);
                 next (Ocsigen_stream.get stream) l)
              (fun e -> exnhandler e l)
      and exnhandler e l =
        Lwt_log.ign_warning ~section ~exn:e "Error while reading stream list";
        finalize `Failure >>= fun () ->
        next_stream l
      in
      let default_result = Result.default () in
      Lwt.return
        (Result.update default_result
         ~content_length:None
         ~etag:(get_etag c)
         ~stream:
           (Ocsigen_stream.make ~finalize (fun _ -> next_stream c), None)
         ~headers:(Http_headers.dyn_headers)
         ~content_type:(Some ct) ())
end


(*****************************************************************************)
module Empty_content =
struct
  type t = unit

  type options = unit

  let get_etag ?options c = None

  let result_of_content ?(options = ()) c = Lwt.return (Result.empty ())

end

(*****************************************************************************)
(* Files *)

(** this module instanciate the HTTP_CONTENT signature for files *)
module File_content =
struct
  type t =
    string (* nom du fichier *) *
    Ocsigen_charset_mime.charset_assoc *
    Ocsigen_charset_mime.mime_assoc

  type options = unit

  let read_file ?buffer_size fd =
    let buffer_size = match buffer_size with
      | None -> Ocsigen_config.get_filebuffersize ()
      | Some s -> s
    in
    Lwt_log.ign_info ~section "start reading file (file opened)";
    let buf = Bytes.create buffer_size in
    let rec read_aux () =
      Lwt_unix.read fd buf 0 buffer_size >>= fun read ->
      if read = 0 then
        Ocsigen_stream.empty None
      else begin
        if read = buffer_size
        then Ocsigen_stream.cont buf read_aux
        else Ocsigen_stream.cont (String.sub buf 0 read) read_aux
      end
    in read_aux

  let get_etag_aux st =
    Some (Printf.sprintf "%Lx-%x-%f" st.Unix.LargeFile.st_size
            st.Unix.LargeFile.st_ino st.Unix.LargeFile.st_mtime)

  let get_etag ?options (f, _, _) =
    let st = Unix.LargeFile.stat f in
    get_etag_aux st

  let skip fd stream k =
    try
      ignore
        (Unix.LargeFile.lseek (Lwt_unix.unix_file_descr fd) k Unix.SEEK_CUR);
      Ocsigen_stream.next (Ocsigen_stream.get stream)
    with e -> Lwt.fail e

  let result_of_content ?options (c, charset_assoc, mime_assoc) =
    (* open the file *)
    try
      let fdu = Unix.openfile c [Unix.O_RDONLY;Unix.O_NONBLOCK] 0o666 in
      let fd = Lwt_unix.of_unix_file_descr fdu in
      try
        let st = Unix.LargeFile.fstat fdu in
        let etag = get_etag_aux st in
        let buffer_size =
          if st.Unix.LargeFile.st_size <=
             Int64.of_int (Ocsigen_config.get_filebuffersize ()) then
            Some (Int64.to_int st.Unix.LargeFile.st_size)
          else
            None in
        let stream = read_file ?buffer_size fd in
        let default_result = Result.default () in
        Lwt.return
          (Result.update default_result
             ~content_length:(Some st.Unix.LargeFile.st_size)
             ~content_type:
               (Some (Ocsigen_charset_mime.find_mime c mime_assoc))
             ~charset:
               (Some (Ocsigen_charset_mime.find_charset c charset_assoc))
             ~lastmodified:(Some st.Unix.LargeFile.st_mtime)
             ~etag:etag
             ~stream:
               (Ocsigen_stream.make
                  ~finalize:
                    (fun _ ->
                       Lwt_log.ign_info ~section "closing file";
                       Lwt_unix.close fd)
                  stream,
                Some (skip fd)) ())
        with e -> Lwt_unix.close fd >>= fun () -> raise e
     with e -> Lwt_log.ign_info ~section ~exn:e "Exc";
       fail e

end

(*****************************************************************************)
(* directory listing - by Gabriel Kerneis *)

(** this module instanciate the HTTP_CONTENT signature for directories *)
module Directory_content =
struct
  type t = string (* dir name *) * string list (* corresponding URL path *)

  type options = unit

  let get_etag_aux st =
    Some (Printf.sprintf "%Lx-%x-%f" st.Unix.LargeFile.st_size
            st.Unix.LargeFile.st_ino st.Unix.LargeFile.st_mtime)

  let get_etag ?options (f, _) =
    let st = Unix.LargeFile.stat f in
    get_etag_aux st

  let date fl =
    let t = Unix.gmtime fl in
    Printf.sprintf
      "%02d-%02d-%04d %02d:%02d:%02d"
      t.Unix.tm_mday
      (t.Unix.tm_mon + 1)
      (1900 + t.Unix.tm_year)
      t.Unix.tm_hour
      t.Unix.tm_min
      t.Unix.tm_sec


  let image_found fich =
    if fich="README" || fich="README.Debian"
    then "/ocsigenstuff/readme.png"
    else
      let reg=Netstring_pcre.regexp "([^//.]*)(.*)"
      in match Netstring_pcre.global_replace reg "$2" fich with
      | ".jpeg" | ".jpg" | ".gif" | ".tif"
      | ".png" -> "/ocsigenstuff/image.png"
      | ".ps" -> "/ocsigenstuff/postscript.png"
      | ".pdf" -> "/ocsigenstuff/pdf.png"
      | ".html" | ".htm"
      | ".php" -> "/ocsigenstuff/html.png"
      | ".mp3"
      | ".wma" -> "/ocsigenstuff/sound.png"
      | ".c" -> "/ocsigenstuff/source_c.png"
      | ".java" -> "/ocsigenstuff/source_java.png"
      | ".pl" -> "/ocsigenstuff/source_pl.png"
      | ".py" -> "/ocsigenstuff/source_py.png"
      | ".iso" | ".mds" | ".mdf" | ".cue" | ".nrg"
      | ".cdd" -> "/ocsigenstuff/cdimage.png"
      | ".deb" -> "/ocsigenstuff/deb.png"
      | ".dvi" -> "/ocsigenstuff/dvi.png"
      | ".rpm" -> "/ocsigenstuff/rpm.png"
      | ".tar" | ".rar" -> "/ocsigenstuff/tar.png"
      | ".gz" | ".tar.gz" | ".tgz" | ".zip"
      | ".jar"  -> "/ocsigenstuff/tgz.png"
      | ".tex" -> "/ocsigenstuff/tex.png"
      | ".avi" | ".mov" -> "/ocsigenstuff/video.png"
      | ".txt" -> "/ocsigenstuff/txt.png"
      | _ -> "/ocsigenstuff/unknown.png"


  (* An html row for a file in the directory listing *)
  let file_row name icon stat = Printf.sprintf "
<tr>
  <td class=\"img\"><img src=\"%s\" alt=\"\" /></td>
  <td><a href=\"%s\">%s</a></td>
  <td>%Ld</td>
  <td>%s</td>
</tr>"
      icon (Netencoding.Url.encode ~plus:false name) name
      stat.Unix.LargeFile.st_size (date stat.Unix.LargeFile.st_mtime)


  let directory filename =
    let dir = Unix.opendir filename in
    let rec aux d =
      try
        let f = Unix.readdir dir in
        try
          let stat = Unix.LargeFile.stat (filename^f) in
          if stat.Unix.LargeFile.st_kind = Unix.S_DIR && f <> "." && f <> ".."
          then
            (`Dir, f,
             file_row f "/ocsigenstuff/folder_open.png" stat) :: aux d
          else
          if stat.Unix.LargeFile.st_kind = Unix.S_REG &&
             f.[(String.length f) - 1] <> '~'
          then
            (`Reg, f, file_row f (image_found f) stat) :: aux d
          else aux d
        with _ (* Unix.stat can fail for a lot of reasons *) -> aux d
        with
          End_of_file -> Unix.closedir d;[]

    in
    let trie li =
      List.sort (fun (a1, b1, _) (a2, b2, _) -> match a1, a2 with
          | `Dir, `Dir ->
            if b1<b2
            then 0
            else 1
          | `Dir, _ -> 0
          | _, `Dir -> 1
          | _, _->
            if b1<b2
            then 0
            else 1) li

    in let rec aux2 = function
      | [] -> ""
      | (_, _, i)::l -> i^(aux2 l)
    in aux2 (trie (aux dir))



  let result_of_content ?(options = ()) (filename, path) =
    let stat = Unix.LargeFile.stat filename in
    let rec back = function
      | [] | [""] -> assert false
      | [_] | [_ ; ""] -> []
      | i::j -> i :: (back j)
    in
    let parent =
      if path = [] || path = [""] then
        None
      else
        Some ("/"^Url.string_of_url_path ~encode:true (back path))
    in
    let before =
      let st = Url.string_of_url_path ~encode:false path in
      "<html>\n\
       <head><meta http-equiv=\"Content-Type\" content=\"text/html;\" />\n\
       <link rel=\"stylesheet\" type=\"text/css\" href=\"/ocsigenstuff/style.css\" media=\"screen\" />\n\
       <title>Listing Directory: "^st^"</title>\n</head>\n\
                                       <body><h1>"^st^"</h1>\n\
                       <table summary=\"Contenu du dossier "^st^"\">\n\
                                                 <tr id=\"headers\"><th></th><th>Name</th><th>Size</th>\
                                                 <th>Last modified</th></tr>\n"

    and back = match parent with
      | None -> ""
      | Some parent ->
        "<tr>\n\
         <td class=\"img\"><img src=\"/ocsigenstuff/back.png\" alt=\"\" /></td>\n\
         <td><a href=\""^parent^"\">Parent Directory</a></td>\n\
                                <td>"^(Int64.to_string stat.Unix.LargeFile.st_size)^"</td>\n\
                                                             <td>"^(date stat.Unix.LargeFile.st_mtime)^"</td>\n\
                                                   </tr>\n"

    and after=
      "</table>\
       <p id=\"footer\">Ocsigen Webserver</p>\
       </body></html>"
    in
    let c = before^back^(directory filename)^after in
    let etag = get_etag_aux stat in
    Text_content.result_of_content (c, "text/html") >>= fun r ->
    Lwt.return
      (Result.update r
         ~lastmodified:(Some stat.Unix.LargeFile.st_mtime)
         ~etag:etag
         ~charset:(Some "utf-8") ())
end



(*****************************************************************************)
module Error_content =
(** sends an error page that fit the error number *)
struct
  type t = int option * exn option * Ocsigen_cookies.cookieset

  type options = unit

  let get_etag ?options c = None

  let error_page s msg c =
    Html5.M.html
      (Html5.M.head (Html5.M.title (Html5.M.pcdata s)) [])
      (Html5.M.body
         (Html5.M.h1 [Html5.M.pcdata msg]::
          Html5.M.p [Html5.M.pcdata s]::
          c)
      )

  let result_of_content ?(options = ()) (code, exn, cookies_to_set) =
    let code = match code with
      | None -> 500
      | Some c -> c
    in
    let (error_code, error_msg, headers) =
      match exn with
      | Some (Http_error.Http_exception (errcode, msgs, h) as e) ->
        let msg = Http_error.string_of_http_exception e in
        let headers = match h with
          | Some h -> h
          | None -> Http_headers.dyn_headers
        in (errcode, msg, headers)
      | _ ->
        let error_mes = Http_error.expl_of_code code in
        (code, error_mes, Http_headers.empty)
    in
    let headers =
      (* puts dynamic headers *)
      let (<<) h (n, v) = Http_headers.replace n v h in
      headers
      << (Http_headers.cache_control, "no-cache")
      << (Http_headers.expires, "0")
    in
    let str_code = string_of_int error_code in
    let err_page =
      match exn with
      | Some exn when Ocsigen_config.get_debugmode () ->
        error_page
          ("Error "^str_code)
          error_msg
          [Html5.M.p
             [Html5.M.pcdata (Printexc.to_string exn);
              Html5.M.br ();
              Html5.M.em
                [Html5.M.pcdata "(Ocsigen running in debug mode)"]
             ]]
      | _ ->
        error_page
          ("Error "^str_code)
          error_msg
          []
    in
    Html5_content.result_of_content err_page >>= fun r ->
    Lwt.return
      (Result.update r
         ~cookies:cookies_to_set
         ~code:error_code
         ~charset:(Some "utf-8")
         ~headers:headers ())
end


let send_error
    ?code
    ?exn
    slot
    ~clientproto
    ?mode
    ?proto
    ?(cookies = Ocsigen_cookies.Cookies.empty)
    ~head
    ~sender
    ()
  =
  Error_content.result_of_content (code, exn, cookies) >>= fun r ->
  send
    slot
    ~clientproto
    ?mode
    ?proto
    ~head
    ~sender
    r
