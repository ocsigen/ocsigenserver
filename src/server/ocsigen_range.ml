(* Ocsigen
 * http://www.ocsigen.org
 * ocsigen_range.ml Copyright (C) 2008
 * Vincent Balat
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

(* - We send the range only if we know the content length
   (the header of partial answers must contain the length)
   - We compute range after content-encoding (deflation)
   - We do not support multipart ranges. We send only an interval.
   - The following works with any stream.
   For files, it should be optimized with seek!!!!!
*)

open Ocsigen_lib

exception Range_416

(* We do not support multipart ranges. We send only an interval.
   The following function checks if we support the range requested.
*)
let rec change_range = function
  | Some ([], Some b, ifmatch) -> Some (b, None, ifmatch)
  | Some ([ (b, e) ], None, ifmatch) -> Some (b, Some e, ifmatch)
  | _ -> None

let select_range length beg endopt skipfun stream =
  let rec aux step num () =
    if num = 0L
    then Ocsigen_stream.empty None
    else
      (match step with
       | Ocsigen_stream.Finished _  ->
         Lwt.fail Ocsigen_stream.Stream_too_small
       | Ocsigen_stream.Cont (c, f) -> Lwt.return (c, f))
      >>= fun (buf, nextstream) ->
      let buflen = String.length buf in
      let buflen64 = Int64.of_int buflen in
      if (Int64.compare buflen64 num) <= 0
      then
        Ocsigen_stream.cont buf (fun () ->
            Ocsigen_stream.next nextstream >>= fun next ->
            aux next (Int64.sub num buflen64) ())
      else
        Ocsigen_stream.cont (String.sub buf 0 (Int64.to_int num))
          (fun () -> Ocsigen_stream.empty None)
  in
  Lwt.catch
    (fun () ->
       skipfun stream beg >>= fun new_s ->
       Lwt.return
         (match endopt with
          | None ->
            Ocsigen_stream.make
              ~finalize:
                (fun status -> Ocsigen_stream.finalize stream status)
              (fun () -> Lwt.return new_s)
          | Some endc ->
            Ocsigen_stream.make
              ~finalize:
                (fun status -> Ocsigen_stream.finalize stream status)
              (aux new_s length))
    )
    (function
      | Ocsigen_stream.Stream_too_small -> Lwt.fail Range_416
      (* RFC 2616 A server SHOULD return a response with this status code if a request included a Range request-header field, and none of the range-specifier values in this field overlap the current extent of the selected resource, and the request did not include an If-Range request-header field. (For byte-ranges, this means that the first- byte-pos of all of the byte-range-spec values were greater than the current length of the selected resource.) *)
      | e -> Lwt.fail e)


let compute_range ri res =
  match Ocsigen_http_frame.Result.content_length res with
  (* We support Ranges only if we know the content length, because
     Content-Range always contains the length ... *)
  | None -> Lwt.return res
  | Some cl ->
    (* Send range only if the code is 200!! *)
    if (Ocsigen_http_frame.Result.code res <> 200)
    || (Ocsigen_config.get_disablepartialrequests ())
    then Lwt.return res
    else begin
      let res =
        Ocsigen_http_frame.Result.update res
          ~headers:
            (Http_headers.replace
               Http_headers.accept_ranges "bytes"
               (Ocsigen_http_frame.headers res)) ()
      in
      match change_range (Lazy.force (Ocsigen_request_info.range ri)) with
      | None -> Lwt.return res
      | Some (_, _, Ocsigen_extensions.IR_ifmatch etag)
        when (match Ocsigen_http_frame.Result.etag res with
            | None -> true
            | Some resetag -> String.compare etag resetag <> 0) ->
        Lwt.return res
      | Some (_, _, Ocsigen_extensions.IR_Ifunmodsince date)
        when (match Ocsigen_http_frame.Result.lastmodified res with
            | None -> true
            | Some l -> l > date)
        ->
        Lwt.return res
      | Some (beg, endopt, _) ->

        Lwt.catch
          (fun () ->
             (if Int64.compare cl beg <= 0
              then Lwt.fail Range_416
              else Lwt.return ()) >>= fun () ->

             let endc, length = match endopt with
               | None -> (Int64.sub cl 1L, Int64.sub cl beg)
               | Some e -> (e, Int64.add (Int64.sub e beg) 1L)
             in

             let resstream, skipfun =
               (Ocsigen_http_frame.Result.stream res)
             in
             (* stream transform *)
             let skipfun =
               match skipfun with
               | None ->
                 (fun stream beg ->
                    (Ocsigen_stream.next
                       (Ocsigen_stream.get stream) >>= fun s ->
                     Ocsigen_stream.skip s beg))
               | Some f -> f
             in
             select_range
               length beg endopt skipfun
               resstream
             >>= fun new_s ->
             Lwt.return
               (Ocsigen_http_frame.Result.update res
                  ~stream:(new_s, None)
                  ~code:206
                  ~headers:
                    (Http_headers.replace
                       Http_headers.content_range
                       ("bytes "^Int64.to_string beg^"-"^
                        Int64.to_string endc^"/"^
                        Int64.to_string cl)
                       (Ocsigen_http_frame.Result.headers res))
                  ~content_length:(Some length) ())
          )
          (function
            | Range_416 ->
              (* RFC 2616 When this status code is returned for a byte-range request, the response SHOULD include a Content-Range entity-header field specifying the current length of the selected resource *)
              let dr = Ocsigen_http_frame.Result.default () in
              Lwt.return
                (Ocsigen_http_frame.Result.update dr
                   ~code:416
                   ~headers:
                     (Http_headers.replace
                        Http_headers.content_range
                        ("bytes */"^Int64.to_string cl)
                        (Ocsigen_http_frame.Result.headers dr))
                   ())
            | e -> Lwt.fail e)

    end


let get_range http_frame =
  try
    let rangeheader = Ocsigen_http_frame.Http_header.get_headers_value
        http_frame.Ocsigen_http_frame.frame_header
        Http_headers.range
    in

    let decode_int index d e =
      let a = Int64.of_string d in
      let b = Int64.of_string e in
      assert (Int64.compare index a < 0);
      assert (Int64.compare a b <= 0);
      (a, b)
    in

    let interval, from =
      let a,b = String.sep '=' rangeheader in
      if String.compare a "bytes" <> 0
      then raise Not_found
      else
        let l = String.split ',' b in
        let rec f index = function
          | [] -> [], None
          | [a] ->
            let d, e = String.sep '-' a in
            if e = ""
            then [], Some (Int64.of_string d)
            else [decode_int index d e], None
          | a::l ->
            let d, e = String.sep '-' a in
            let a, b = decode_int index d e in
            let ll, fr = f b l in (* not tail rec *)
            (a, b)::ll, fr
        in
        f (-1L) l
    in

    let ifrange =
      try
        let ifrangeheader = Ocsigen_http_frame.Http_header.get_headers_value
            http_frame.Ocsigen_http_frame.frame_header
            Http_headers.if_range
        in
        try
          Ocsigen_extensions.IR_Ifunmodsince (Netdate.parse_epoch ifrangeheader)
        with _ -> Ocsigen_extensions.IR_ifmatch ifrangeheader
        with Not_found -> Ocsigen_extensions.IR_No
    in

    Some (interval, from, ifrange)

  with _ -> None
