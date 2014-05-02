(* Ocsigen
 * http://www.ocsigen.org
 * ocsigen_http_frame.ml Copyright (C) 2005
 * Denis Berthod, Vincent Balat, Jérôme Vouillon
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
open Cohttp

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



(** The type of answers to send *)
type result =
  {res_cookies: cookieset; (** cookies to set (with optional path) *)
   res_lastmodified: float option; (** Default: [None] *)
   res_etag: string option;
   res_code: int; (** HTTP code, if not 200 *)
   res_stream: string Ocsigen_stream.t *
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
   res_content_length: int64 option; (** [None] means Transfer-encoding: chunked *)
   res_content_type: string option;
   res_headers: Http_headers.t; (** The headers you want to add *)
   res_charset: string option; (** Default: None *)
   res_location: string option; (** Default: None *)
  }

(** Default [result] to use as a base for constructing others. *)
let default_result () =
  {
    res_cookies = Cookies.empty;
    res_lastmodified = None;
    (* No date => proxies use etag *)
    res_etag = None;
    res_code = 200;
    res_stream = (Ocsigen_stream.make (fun () -> Ocsigen_stream.empty None), 
                  None);
    res_content_length = Some 0L;
    res_content_type = None;
    res_headers= Http_headers.empty;
    res_charset= None;
    res_location= None;
  }

(** [result] for an empty page. *)
let empty_result () =
  {
    res_cookies = Cookies.empty;
    res_lastmodified = None;
    res_etag = None;
    res_code = 204; (* No content *)
    res_stream = (Ocsigen_stream.make (fun () -> Ocsigen_stream.empty None), 
                  None);
    res_content_length = Some 0L;
    res_content_type = None;
    res_headers= Http_headers.empty;
    res_charset= None;
    res_location= None;
  }

module type HTTP_CONTENT =
sig
  (** abstract type of the content *)
  type t

  type options

  (** convert a content into a thread returning the default
      [result] for this content *)
  val result_of_content : ?options:options -> t -> result Lwt.t

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

  let proto_of_cohttp_version = function
    | `HTTP_1_0 -> HTTP10
    | `HTTP_1_1 -> HTTP11
  let proto_to_cohttp_version = function
    | HTTP10 -> `HTTP_1_0
    | HTTP11 -> `HTTP_1_1
  let meth_of_cohttp_meth = function
    | `GET -> GET
    | `POST -> POST
    | `HEAD -> HEAD
    | `PUT -> PUT
    | `DELETE -> DELETE
    | `TRACE -> TRACE
    | `OPTIONS -> OPTIONS
    | `CONNECT -> CONNECT
    | `LINK -> LINK
    | `UNLINK -> UNLINK
    | `PATCH -> PATCH
  let meth_to_cohttp_meth = function
    | GET -> `GET
    | POST -> `POST
    | HEAD -> `HEAD
    | PUT -> `PUT
    | DELETE -> `DELETE
    | OPTIONS -> `OPTIONS
    | PATCH -> `PATCH
    | _ -> raise
             (Invalid_argument "Ocsigen_http_frame.Http_header.meth_to_cohttp_meth")

  let of_cohttp_header headers =
    Cohttp.Header.fold
      (fun key value acc -> Http_headers.add (Http_headers.name key) value acc)
      headers Http_headers.empty
  let to_cohttp_header headers =
    Http_headers.fold
      (fun key values acc ->
         let key = Http_headers.name_to_string key in
         List.fold_left (fun acc value ->
             Cohttp.Header.add acc key value) acc values)
      headers (Cohttp.Header.init ())

  (** Type conversion between Cohttp.[Response|Request].t to
   * Ocsigen_http_frame.http_header *)
  let of_cohttp_request request =
    {
      mode = Query
          (meth_of_cohttp_meth @@ Request.meth request,
           Uri.to_string @@ Request.uri request);
      proto = proto_of_cohttp_version @@ Request.version request;
      headers = of_cohttp_header @@ Request.headers request;
    }
  let of_cohttp_response response =
    {
      mode = Answer (Cohttp.Code.code_of_status @@ Response.status response);
      proto = proto_of_cohttp_version @@ Response.version response;
      headers = of_cohttp_header @@ Response.headers response;
    }
  let to_cohttp_request ?encoding { mode; proto; headers; } uri =
    match mode with
    | Query (meth, _) ->
      let meth = meth_to_cohttp_meth meth in
      let version = proto_to_cohttp_version proto in
      let headers = to_cohttp_header headers in
      Cohttp.Request.make ~meth ~version ?encoding ~headers uri
    | _ -> raise
             (Invalid_argument "Ocsigen_http_frame.Http_header.to_cohttp_request")
  let to_cohttp_response ?encoding ?flush { mode; proto; headers; } =
    match mode with
    | Answer code ->
      let version = proto_to_cohttp_version proto in
      let status = Cohttp.Code.status_of_code code in
      let headers = to_cohttp_header headers in
      Cohttp.Response.make ~version ~status ?flush ?encoding ~headers ()
    | _ -> raise
             (Invalid_argument "Ocsigen_http_frame.Http_header.to_cohttp_response")
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
      Ocsigen_messages.debug
        (fun () -> Format.sprintf "%s: %s (with headers)" (expl_of_code n) s)
    | Http_exception (n, Some s, None) ->
      Ocsigen_messages.debug
        (fun () -> Format.sprintf "%s: %s" (expl_of_code n) s)
    | Http_exception (n, None, _) ->
      Ocsigen_messages.debug
        (fun () -> Format.sprintf "%s" (expl_of_code n))
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
      à revoir...
    *)
  }

let of_cohttp_request request body = {
  frame_header = Http_header.of_cohttp_request request;
  frame_content = Some
      (Ocsigen_stream.of_lwt_stream
         (fun x -> x)
         (Cohttp_lwt_body.to_stream body));
  frame_abort = (fun () -> Lwt.return ());
  (*XXX: It is complex and related to the management of the pipeline by Ocsigen.
   * Consideration should be given to another layer over cohttp for pipeline
   * management as proxy. *)
}

let to_date date =
  let x = Netdate.mk_mail_date ~zone:0 date in
  try
    let ind_plus = String.index x '+' in
    String.set x ind_plus 'G';
    String.set x (ind_plus + 1) 'M';
    String.set x (ind_plus + 2) 'T';
    String.sub x 0 (ind_plus + 3)
  with Invalid_argument _ | Not_found -> (); x

let to_type ty charset =
  if String.length ty >= 4 then
    match String.sub ty 0 4, charset with
    | "text", Some "" -> ty
    | "text", Some ch -> Format.sprintf "%s; charset=%s" ty ch
    | _ ->
      begin match String.sub ty (String.length ty - 4) 4, charset with
        | ("+xml"|"/xml"), Some "" -> ty
        | ("+xml"|"/xml"), Some ch -> Format.sprintf "%s; charset=%s" ty ch
        | _ -> ty
      end
  else ty

let result_to_cohttp_response {
    res_cookies; (* OK *)
    res_lastmodified; (* OK *)
    res_etag; (* OK *)
    res_code; (* OK *)
    res_stream;
    res_content_length; (* OK *)
    res_content_type; (* OK *)
    res_headers; (* OK *)
    res_charset; (* OK *)
    res_location; (* OK *)
  } =
  let headers =
    Ocsigen_cookies_server.to_cohttp_header res_cookies
      (Http_header.to_cohttp_header res_headers) in
  let headers = match res_lastmodified with
    | Some date -> Cohttp.Header.add headers "Last-Modified" (to_date date)
    | None -> headers
  in
  let headers = match res_etag with
    | Some etag -> Cohttp.Header.add headers "ETag" (Format.sprintf "\"%s\"" etag)
    | None -> headers
  in
  let encoding = match res_content_length with
    | Some length when length <= Int64.of_int max_int ->
      Cohttp.Transfer.Fixed (Int64.to_int length)
    | _ -> Cohttp.Transfer.Chunked
  in
  let headers = match res_content_type with
    | Some ty -> Cohttp.Header.add headers "Content-Type" (to_type ty res_charset)
    | None -> headers
  in
  let headers = match res_location with
    | Some location -> Cohttp.Header.add headers "Location" location
    | None -> headers
  in
  (Cohttp.Response.make
     ~status:(Cohttp.Code.status_of_code res_code)
     ~encoding
     ~headers
     (),
   `Stream (Ocsigen_stream.to_lwt_stream
              ~is_empty:(fun x -> String.length x = 0)
              (fst res_stream)))
