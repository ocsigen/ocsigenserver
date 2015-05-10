(* Ocsigen
 * http://www.ocsigen.org
 * ocsigen_http_frame.ml Copyright (C) 2005
 * Denis Berthod, Vincent Balat, JÃ©rÃ´me Vouillon
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

open Ocsigen_lib

(** this set of modules discribes the http protocol and
    the operation on this protocol*)

(** this signature provides a template to discribe the content of a http
    frame *)

open Ocsigen_stream
open Ocsigen_cookies

let section = Lwt_log.Section.make "ocsigen:http:frame"

type etag = string


(* The following cookie function is not in Ocsigen_cookies
   because ri is not used client side *)


(* [compute_new_ri_cookies now path ri_cookies cookies_to_set]
   adds the cookies from [cookies_to_set]
   to [ri_cookies], as if the cookies
   had been send to the browser and the browser
   was doing a new request to the url [path].
   Only the cookies that match [path] (current path) are added. *)
let compute_new_ri_cookies
    now
    ripath
    ricookies
    cookies_set_by_page =

  let prefix path p =
    Url.is_prefix_skip_end_slash
      (Url.remove_slash_at_beginning path)
      (Url.remove_slash_at_beginning p)
  in
  Cookies.fold
    (fun path ct t ->
       if prefix path ripath then
         String.Table.fold
           (fun n v beg ->
              match v with
              | OSet (Some ti, v, _) when ti>now ->
                String.Table.add n v t
              | OSet (None, v, _) -> String.Table.add n v t
              | OSet (_, _, _)
              | OUnset -> String.Table.remove n t
           )
           ct
           t
       else t
    )
    cookies_set_by_page
    ricookies


module Result = struct
  (** The type of answers to send *)
  type result =
      {cookies: cookieset; (** cookies to set (with optional path) *)
       lastmodified: float option; (** Default: [None] *)
       etag: string option;
       code: int; (** HTTP code, if not 200 *)
       stream: string Ocsigen_stream.t *
         (string Ocsigen_stream.t ->
            int64 ->
              string Ocsigen_stream.step Lwt.t) option
       ; (** Default: empty stream.
             The second field is (optionaly)
             the function used to skip a part of the
             stream, if you do not you want to use
             a basic reading of the stream.
             For example, for static files, you can optimize it by using
             a [seek] function.
         *)
       (* It is not a new field of the record to remember to change it
          if we change the stream. *)
       content_length: int64 option; (** [None] means Transfer-encoding: chunked *)
       content_type: string option;
       headers: Http_headers.t; (** The headers you want to add *)
       charset: string option; (** Default: None *)
       location: string option; (** Default: None *)
     }

  let cookies { cookies; _ } = cookies
  let lastmodified { lastmodified; _ } = lastmodified
  let etag { etag; _ } = etag
  let code { code; _ } = code
  let stream { stream; _ } = stream
  let content_length { content_length; _ } = content_length
  let content_type { content_type; _ } = content_type
  let headers { headers; _ } = headers
  let charset { charset; _ } = charset
  let location { location; _ } = location

  (** Default [result] to use as a base for constructing others. *)
  let default () =
    {
     cookies = Cookies.empty;
     lastmodified = None;
     (* No date => proxies use etag *)
     etag = None;
     code = 200;
     stream = (Ocsigen_stream.make (fun () -> Ocsigen_stream.empty None),
                   None);
     content_length = Some 0L;
     content_type = None;
     headers= Http_headers.empty;
     charset= None;
     location= None;
   }

  let update result
    ?(cookies=result.cookies)
    ?(lastmodified=result.lastmodified)
    ?(etag=result.etag)
    ?(code=result.code)
    ?(stream=result.stream)
    ?(content_length=result.content_length)
    ?(content_type=result.content_type)
    ?(headers=result.headers)
    ?(charset=result.charset)
    ?(location=result.location) () =
      {
        cookies;
        lastmodified;
        etag;
        code;
        stream;
        content_length;
        content_type;
        headers;
        charset;
        location;
      }

  (** [result] for an empty page. *)
  let empty () =
    {
     cookies = Cookies.empty;
     lastmodified = None;
     etag = None;
     code = 204; (* No content *)
     stream = (Ocsigen_stream.make (fun () -> Ocsigen_stream.empty None),
                   None);
     content_length = Some 0L;
     content_type = None;
     headers= Http_headers.empty;
     charset= None;
     location= None;
   }
end

include Result

module type HTTP_CONTENT =
sig
  (** abstract type of the content *)
  type t

  type options

  (** convert a content into a thread returning the default
      [result] for this content *)
  val result_of_content : ?options:options -> t -> Result.result Lwt.t

  (** compute etag for content *)
  val get_etag : ?options:options -> t -> etag option
end


(** this module describes the type of an http header *)
module Http_header =
struct

  (** type of the http_method *)
  type http_method =
    | GET
    | POST
    | HEAD
    | PUT
    | DELETE
    | TRACE
    | OPTIONS
    | CONNECT
    | LINK
    | UNLINK
    | PATCH

  (** type of ocsigen_http_frame mode. The int is the HTTP answer code *)
  type http_mode =
    | Query of (http_method * string)
    | Answer of int
    | Nofirstline

  type proto = HTTP10 | HTTP11

  (** type of the http headers *)
  type http_header =
    {
      (** the mode of the header : Query or Answer *)
      mode:http_mode;
      (** protocol used for the Query or the Answer *)
      proto: proto;
      (** list of the headers options *)
      headers: Http_headers.t;
    }

  (*        (** gets the url raise Not_found if Answer *)
            let get_url header =
            match header.mode with
            | Query (_, s) -> s
            | _ -> raise Not_found *)

  (** gets the firstline of the header *)
  let get_firstline header = header.mode

  (** gets the headers *)
  let get_headers header = header.headers

  (** gets the value of a given header's option *)
  let get_headers_value header key =
    Http_headers.find key header.headers

  (** gets all the values of a given header's option *)
  let get_headers_values header key =
    Http_headers.find_all key header.headers

  (** gets the value of the protocol used *)
  let get_proto header = header.proto

  (*        (** gets the value of the http method used *)
            let get_method header =
            match header.mode with
            | Query (meth, _) -> meth
            | _ -> raise Not_found *)

  (** adds an header option in the header option list*)
  let add_headers header key value =
    { header with
      headers = Http_headers.add key value header.headers }
end

module Http_error =
struct

  (** Exception raised on an http error. It is possible to pass the code of
      the error, some comment, and some headers. *)
  exception Http_exception of int * string option * Http_headers.t option

  (* this fonction provides the translation mecanisme between a code and
   * its explanation *)
  let expl_of_code =
    function
    | 100 -> "Continue"
    | 101 -> "Switching Protocol"
    | 200 -> "OK"
    | 201 -> "Created"
    | 202 -> "Accepted"
    | 203 -> "Non-Authoritative information"
    | 204 -> "No Content"
    | 205 -> "Reset Content"
    | 206 -> "Partial Content"
    | 300 -> "Multiple Choices"
    | 301 -> "Moved Permanently"
    | 302 -> "Found"
    | 303 -> "See Other"
    | 304 -> "Not Modified"
    | 305 -> "Use Proxy"
    | 307 -> "Moved Temporarily"
    | 400 -> "Bad Request"
    | 401 -> "Unauthorized"
    | 402 -> "Payment Required"
    | 403 -> "Forbidden"
    | 404 -> "Not Found"
    | 405 -> "Method Not Allowed"
    | 406 -> "Not Acceptable"
    | 407 -> "Proxy Authentication Required"
    | 408 -> "Request Time-out"
    | 409 -> "Conflict"
    | 410 -> "Gone"
    | 411 -> "Length Required"
    | 412 -> "Precondition Failed"
    | 413 -> "Request Entity Too Large"
    | 414 -> "Request URL Too Long"
    | 415 -> "Unsupported Media type"
    | 416 -> "Request Range Not Satisfiable"
    | 417 -> "Expectation Failed"
    | 500 -> "Internal Server Error"
    | 501 -> "Not Implemented"
    | 502 -> "Bad Gateway"
    | 503 -> "Service Unavailable"
    | 504 -> "Gateway Time-out"
    | 505 -> "Version Not Supported"
    | _   -> "Unknown Error" (*!!!*)

        let display_http_exception e =
          match e with
          | Http_exception (n, Some s, Some _) ->
            Lwt_log.ign_info_f ~section "%s: %s (with headers)" (expl_of_code n) s
          | Http_exception (n, Some s, None) ->
            Lwt_log.ign_info_f ~section "%s: %s" (expl_of_code n) s
          | Http_exception (n, None, _) ->
            Lwt_log.ign_info ~section (expl_of_code n)
          | _ ->
              raise e

  let string_of_http_exception e =
    match e with
    | Http_exception (n, Some s, Some _) ->
      Format.sprintf "Error %d, %s: %s (with headers)" n (expl_of_code n) s
    | Http_exception (n, Some s, None) ->
      Format.sprintf "Error %d, %s: %s" n (expl_of_code n) s
    | Http_exception (n, None, _) ->
      Format.sprintf "Error %d, %s" n (expl_of_code n)
    | _ ->
      raise e

end

(** HTTP messages *)
type t =
  { frame_header : Http_header.http_header;
    frame_content : string Ocsigen_stream.t option;
    frame_abort : unit -> unit Lwt.t
    (*VVV abort looks like a hack.
      It has been added for the reverse proxy, to enable closing the connection
      if the request is cancelled ...
    *)
  }
