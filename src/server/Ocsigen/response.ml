open Cohttp
open Lwt.Syntax

module Body = struct
  (* TODO: Avoid copies by passing buffers directly. This API was choosen
     because it is closer to [Lwt_stream] which was used before. This type
     forces data to be copied from buffers (usually [bytes]) to immutable
     strings, which is unecessary. *)
  type t = ((string -> unit Lwt.t) -> unit Lwt.t) * Transfer.encoding

  let make encoding writer : t = writer, encoding
  let empty = make (Fixed 0L) (fun _write -> Lwt.return_unit)

  let of_string s =
    make
      (Transfer.Fixed (Int64.of_int (String.length s)))
      (fun write -> write s)

  let of_cohttp ~encoding body =
    (fun write -> Cohttp_lwt.Body.write_body write body), encoding

  let write (w, _) = w
  let transfer_encoding = snd
end

type t =
  {a_response : Response.t; a_body : Body.t; a_cookies : Ocsigen_cookie_map.t}

let remove_header_if_equal_to (resp : Response.t) header equals_to =
  match Header.get resp.headers header with
  | Some v when String.equal v equals_to ->
      {resp with headers = Header.remove resp.headers header}
  | _ -> resp

let make ?(body = Body.empty) ?(cookies = Ocsigen_cookie_map.empty) a_response =
  (* Remove the erroneous [transfer-encoding] set by default. *)
  (* TODO: Deprecate usages of [Cohttp.Response.t] exposed by this API. *)
  let a_response =
    remove_header_if_equal_to a_response "transfer-encoding" "chunked"
  in
  {a_response; a_body = body; a_cookies = cookies}

let respond ?headers ~status ?(body = Body.empty) () =
  let response = Response.make ~status ?headers () in
  make ~body response

let respond_string ?headers ~status ~body () =
  let response = Response.make ~status ?headers () in
  let body = Body.of_string body in
  make ~body response

let respond_error ?headers ?(status = `Internal_server_error) ~body () =
  respond_string ?headers ~status ~body:("Error: " ^ body) ()

let respond_not_found ?uri () =
  let body =
    match uri with
    | None -> "Not found"
    | Some uri -> "Not found: " ^ Uri.to_string uri
  in
  respond_string ~status:`Not_found ~body ()

(* Weak entity-tag built from the file's modification time and size. *)
let weak_etag ~mtime ~size = Printf.sprintf "W/\"%x-%x\"" mtime size

(* Does the [If-None-Match] header value match [etag]? Handles ["*"] and a
   comma-separated list of entity-tags. *)
let if_none_match_matches if_none_match etag =
  let if_none_match = String.trim if_none_match in
  String.equal if_none_match "*"
  || List.exists
       (fun t -> String.equal (String.trim t) etag)
       (String.split_on_char ',' if_none_match)

(* Parse a single byte-range request against the total [size] (RFC 7233).
   Returns the inclusive bounds [`Range (first, last)], [`Full] when there is no
   usable single range (absent, malformed or multi-range), or [`Unsatisfiable]. *)
let parse_range size value =
  match String.split_on_char '=' (String.trim value) with
  | ["bytes"; spec] when not (String.contains spec ',') -> (
    match String.split_on_char '-' (String.trim spec) with
    | [first; last] -> (
        let parse s =
          let s = String.trim s in
          if String.equal s "" then None else int_of_string_opt s
        in
        match parse first, parse last with
        | None, None -> `Full
        | Some first, None ->
            if first < size then `Range (first, size - 1) else `Unsatisfiable
        | None, Some suffix ->
            if suffix <= 0 || size = 0
            then `Unsatisfiable
            else `Range (max 0 (size - suffix), size - 1)
        | Some first, Some last ->
            let last = min last (size - 1) in
            if first <= last then `Range (first, last) else `Unsatisfiable)
    | _ -> `Full)
  | _ -> `Full

let respond_file
      ?headers
      ?(status = `OK)
      ?if_none_match
      ?if_modified_since
      ?range
      ?if_range
      fname
  =
  let exception Isnt_a_file in
  (* Copied from [cohttp-lwt-unix] and adapted to [Body]. *)
  Lwt.catch
    (fun () ->
       (* Check this isn't a directory first *)
       let* s = Lwt_unix.stat fname in
       if Unix.(s.st_kind <> S_REG)
       then raise Isnt_a_file
       else
         let size = s.Unix.st_size in
         let last_modified = Ocsigen_base.Lib.Date.to_string s.Unix.st_mtime in
         let etag = weak_etag ~mtime:(int_of_float s.Unix.st_mtime) ~size in
         (* Validators sent with [200], [206] and [304] responses (RFC 7232). *)
         let add_validators h =
           Http.Header.add
             (Http.Header.add h "etag" etag)
             "last-modified" last_modified
         in
         let headers = Option.value headers ~default:(Http.Header.init ()) in
         (* [If-None-Match] takes precedence over [If-Modified-Since]
            (RFC 7232). For the latter we only return [304] on an exact match of
            our [Last-Modified] value, so we never answer [304] wrongly. *)
         let not_modified =
           match if_none_match with
           | Some inm -> if_none_match_matches inm etag
           | None -> (
             match if_modified_since with
             | Some ims -> String.equal ims last_modified
             | None -> false)
         in
         if not_modified
         then
           Lwt.return
             (respond ~headers:(add_validators headers) ~status:`Not_modified ())
         else
           (* A [Range] request is honoured only if [If-Range] (when present)
              matches the current validators (RFC 7233). *)
           let if_range_ok =
             match if_range with
             | None -> true
             | Some v ->
                 let v = String.trim v in
                 String.equal v etag || String.equal v last_modified
           in
           let range =
             match range with
             | Some r when if_range_ok -> parse_range size r
             | _ -> `Full
           in
           let add_common h =
             add_validators (Http.Header.add h "accept-ranges" "bytes")
           in
           match range with
           | `Unsatisfiable ->
               let headers =
                 Http.Header.add (add_common headers) "content-range"
                   (Printf.sprintf "bytes */%d" size)
               in
               Lwt.return
                 (respond ~headers ~status:`Requested_range_not_satisfiable ())
           | (`Full | `Range _) as range ->
               let first, length, status, add_content_range =
                 match range with
                 | `Full -> 0, size, status, fun h -> h
                 | `Range (first, last) ->
                     ( first
                     , last - first + 1
                     , `Partial_content
                     , fun h ->
                         Http.Header.add h "content-range"
                           (Printf.sprintf "bytes %d-%d/%d" first last size) )
               in
               let count = 16384 in
               let* ic =
                 Lwt_io.open_file ~buffer:(Lwt_bytes.create count)
                   ~mode:Lwt_io.input fname
               in
               let encoding = Http.Transfer.Fixed (Int64.of_int length) in
               let stream write =
                 let send () =
                   let* () =
                     if first = 0
                     then Lwt.return_unit
                     else Lwt_io.set_position ic (Int64.of_int first)
                   in
                   let remaining = ref length in
                   let rec cat_loop () =
                     if !remaining <= 0
                     then Lwt.return_unit
                     else
                       let* buf =
                         Lwt_io.read ~count:(min count !remaining) ic
                       in
                       if String.equal buf ""
                       then Lwt.return_unit
                       else (
                         remaining := !remaining - String.length buf;
                         let* () = write buf in
                         cat_loop ())
                   in
                   cat_loop ()
                 in
                 let* () =
                   Lwt.catch send (fun exn ->
                     Logs.warn (fun m ->
                       m "Error resolving file %s (%s)" fname
                         (Printexc.to_string exn));
                     Lwt.return_unit)
                 in
                 Lwt.catch
                   (fun () -> Lwt_io.close ic)
                   (fun e ->
                      Logs.warn (fun f ->
                        f "Closing channel failed: %s" (Printexc.to_string e));
                      Lwt.return_unit)
               in
               let body = Body.make encoding stream in
               let mime_type = Magic_mime.lookup fname in
               let headers =
                 add_content_range
                   (add_common
                      (Http.Header.add_unless_exists headers "content-type"
                         mime_type))
               in
               Lwt.return (respond ~headers ~status ~body ()))
    (function
      | Unix.Unix_error (Unix.ENOENT, _, _) | Isnt_a_file ->
          Lwt.return (respond_not_found ())
      | exn -> Lwt.reraise exn)

let update ?response ?body ?cookies {a_response; a_body; a_cookies} =
  let a_response =
    match response with Some response -> response | None -> a_response
  in
  let a_body = match body with Some body -> body | None -> a_body
  and a_cookies =
    match cookies with Some cookies -> cookies | None -> a_cookies
  in
  {a_response; a_body; a_cookies}

let of_cohttp ?(cookies = Ocsigen_cookie_map.empty) (a_response, body) =
  let encoding = Response.encoding a_response in
  let a_body = Body.of_cohttp ~encoding body in
  {a_response; a_body; a_cookies = cookies}

(* FIXME: secure *)
let make_cookies_header path exp name c _secure =
  Format.sprintf "%s=%s%s%s" name c
    (*VVV encode = true? *)
    ("; path=/" ^ Ocsigen_base.Lib.Url.string_of_url_path ~encode:true path)
    (* (if secure && slot.sl_ssl then "; secure" else "")^ *)
    ""
  ^
  match exp with
  | Some s -> "; expires=" ^ Ocsigen_base.Lib.Date.to_string s
  | None -> ""

let make_cookies_headers path t hds =
  Ocsigen_cookie_map.Map_inner.fold
    (fun name c h ->
       let open Ocsigen_cookie_map in
       let exp, v, secure =
         match c with
         | OUnset -> Some 0., "", false
         | OSet (t, v, secure) -> t, v, secure
       in
       Cohttp.Header.add h
         Ocsigen_http.Header.Name.(to_string set_cookie)
         (make_cookies_header path exp name v secure))
    t hds

let to_cohttp_response {a_response; a_cookies; a_body = _, body_encoding} =
  let headers =
    let add name value headers = Header.add_unless_exists headers name value in
    let add_transfer_encoding h =
      Header.add_transfer_encoding h body_encoding
    in
    Response.headers a_response
    |> Ocsigen_cookie_map.Map_path.fold make_cookies_headers a_cookies
    |> add "server" Config.server_name
    |> add "date" (Ocsigen_base.Lib.Date.to_string (Unix.time ()))
    |> add_transfer_encoding
  in
  {a_response with Response.headers}

let to_response_expert t =
  let module R = Cohttp_lwt_unix.Response in
  let write_footer encoding oc =
    (* Copied from [cohttp/response.ml]. *)
    match encoding with
    | Transfer.Chunked -> Lwt_io.write oc "0\r\n\r\n"
    | Transfer.Fixed _ | Transfer.Unknown -> Lwt.return_unit
  in
  let res = to_cohttp_response t in
  ( res
  , fun _ic oc ->
      let writer = R.make_body_writer ~flush:false res oc in
      let body, encoding = t.a_body in
      let* () = body (R.write_body writer) in
      write_footer encoding oc )

let response t = t.a_response
let body t = t.a_body

let status {a_response = {Cohttp.Response.status; _}; _} =
  match status with
  | `Code _ -> failwith "FIXME: Cohttp.Code.status_code -> status"
  | #Cohttp.Code.status as a -> a

let set_status ({a_response; _} as a) status =
  { a with
    a_response =
      {a_response with Cohttp.Response.status :> Cohttp.Code.status_code} }

let cookies {a_cookies; _} = a_cookies

let add_cookies ({a_cookies; _} as a) cookies =
  if cookies = Ocsigen_cookie_map.empty
  then a
  else {a with a_cookies = Ocsigen_cookie_map.add_multi a_cookies cookies}

let header {a_response; _} id =
  let h = Cohttp.Response.headers a_response in
  Cohttp.Header.get h (Ocsigen_http.Header.Name.to_string id)

let header_multi {a_response; _} id =
  let h = Cohttp.Response.headers a_response in
  Cohttp.Header.get_multi h (Ocsigen_http.Header.Name.to_string id)

let add_header
      ({a_response = {Cohttp.Response.headers; _} as a_response; _} as a)
      id
      v
  =
  { a with
    a_response =
      { a_response with
        Cohttp.Response.headers =
          Cohttp.Header.add headers (Ocsigen_http.Header.Name.to_string id) v }
  }

let add_header_multi
      ({a_response = {Cohttp.Response.headers; _} as a_response; _} as a)
      id
      l
  =
  let id = Ocsigen_http.Header.Name.to_string id in
  let headers =
    List.fold_left (fun headers -> Cohttp.Header.add headers id) headers l
  in
  {a with a_response = {a_response with Cohttp.Response.headers}}

let replace_header
      ({a_response = {Cohttp.Response.headers; _} as a_response; _} as a)
      id
      v
  =
  { a with
    a_response =
      { a_response with
        Cohttp.Response.headers =
          Cohttp.Header.replace headers
            (Ocsigen_http.Header.Name.to_string id)
            v } }

let replace_headers ({a_response; _} as a) l =
  let headers =
    List.fold_left
      (fun headers (id, content) ->
         Cohttp.Header.replace headers
           (Ocsigen_http.Header.Name.to_string id)
           content)
      (Cohttp.Response.headers a_response)
      l
  in
  {a with a_response = {a_response with Cohttp.Response.headers}}

let remove_header ({a_response; _} as a) id =
  let headers = Cohttp.Response.headers a_response
  and id = Ocsigen_http.Header.Name.to_string id in
  let headers = Cohttp.Header.remove headers id in
  {a with a_response = {a_response with Cohttp.Response.headers}}
