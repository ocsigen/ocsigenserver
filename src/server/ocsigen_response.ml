open Eio.Std
open Cohttp

module Body = struct
  (* TODO: Avoid copies by passing buffers directly. This API was choosen
     because it is closer to [Lwt_stream] which was used before. This type
     forces data to be copied from buffers (usually [bytes]) to immutable
     strings, which is unecessary. *)
  type t = ((string -> unit) -> unit) * Transfer.encoding

  let make encoding writer : t = writer, encoding
  let empty = make (Fixed 0L) (fun _write -> ())

  let of_string s =
    make
      (Transfer.Fixed (Int64.of_int (String.length s)))
      (fun write -> write s)

  let copy_body_to_t body write =
    let buf = Cstruct.create 4096 in
    let rec aux () =
      match Eio.Flow.single_read body buf with
      | len ->
          write (Cstruct.to_string ~len buf);
          aux ()
      | exception End_of_file -> ()
    in
    aux ()

  let of_cohttp ~encoding body = copy_body_to_t body, encoding
  let write (w, _) = w
  let transfer_encoding = snd
end

type t =
  {a_response : Response.t; a_body : Body.t; a_cookies : Ocsigen_cookie_map.t}

let make ?(body = Body.empty) ?(cookies = Ocsigen_cookie_map.empty) a_response =
  {a_response; a_body = body; a_cookies = cookies}

let respond ?headers ~status ~encoding ?(body = Body.empty) () =
  let encoding =
    match headers with
    | None -> encoding
    | Some headers -> (
      match Cohttp.Header.get_transfer_encoding headers with
      | Cohttp.Transfer.Unknown -> encoding
      | t -> t)
  in
  let response = Response.make ~status ~encoding ?headers () in
  make ~body response

let respond_string ?headers ~status ~body () =
  let encoding = Transfer.Fixed (Int64.of_int (String.length body)) in
  let response = Response.make ~status ~encoding ?headers () in
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

let respond_file ?headers ?(status = `OK) fname =
  let exception Isnt_a_file in
  try
    let sw = Stdlib.Option.get (Fiber.get Ocsigen_lib.current_switch) in
    let env = Stdlib.Option.get (Fiber.get Ocsigen_lib.env) in
    (* Copied from [cohttp-lwt-unix] and adapted to [Body]. *)
    let file_path = Eio.Path.( / ) env#fs fname in
    let file_size =
      (* Check this isn't a directory first *)
      let s = Eio.Path.stat ~follow:true file_path in
      if s.Eio.File.Stat.kind <> `Regular_file
      then raise Isnt_a_file
      else s.size
    in
    let ic = Eio.Path.open_in ~sw file_path in
    let encoding = Http.Transfer.Fixed (Optint.Int63.to_int64 file_size) in
    let stream write =
      let buf = Cstruct.create 16384 in
      let rec cat_loop () =
        match Eio.Flow.single_read ic buf with
        | len ->
            write (Cstruct.to_string ~len buf);
            cat_loop ()
        | exception End_of_file -> ()
      in
      let () =
        try cat_loop ()
        with exn ->
          Logs.warn (fun m ->
            m "Error resolving file %s@\n%a" fname Eio.Exn.pp exn)
      in
      try Eio.Resource.close ic
      with e ->
        Logs.warn (fun f -> f "Closing channel failed:@\n%a" Eio.Exn.pp e)
    in
    let body = Body.make encoding stream in
    let mime_type = Magic_mime.lookup fname in
    let headers =
      Http.Header.add_opt_unless_exists headers "content-type" mime_type
    in
    respond ~headers ~status ~encoding ~body ()
  with Unix.Unix_error (Unix.ENOENT, _, _) | Isnt_a_file ->
    respond_not_found ()

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
    ("; path=/" ^ Ocsigen_lib.Url.string_of_url_path ~encode:true path)
    (* (if secure && slot.sl_ssl then "; secure" else "")^ *)
    ""
  ^
  match exp with
  | Some s -> "; expires=" ^ Ocsigen_lib.Date.to_string s
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
         Ocsigen_header.Name.(to_string set_cookie)
         (make_cookies_header path exp name v secure))
    t hds

let to_cohttp_response {a_response; a_cookies; a_body = _, encoding} =
  let headers =
    let add name value headers = Header.add_unless_exists headers name value in
    let add_transfer_encoding h =
      match encoding with
      | Transfer.Chunked -> add "transfer-encoding" "chunked" h
      | _ -> h
    in
    Ocsigen_cookie_map.Map_path.fold make_cookies_headers a_cookies
      (Response.headers a_response)
    |> add "server" Ocsigen_config.server_name
    |> add "date" (Ocsigen_lib.Date.to_string (Unix.time ()))
    |> add_transfer_encoding
  in
  {a_response with Response.headers}

module Response_io = Cohttp.Response.Make (Cohttp_eio.Server.IO)
[@alert "-deprecated"]

let to_response_expert t =
  let module R = Response_io in
  let res = to_cohttp_response t in
  (* Use the response's encoding (what we tell the client), not the body's encoding *)
  let response_encoding = Response.encoding res in
  let write_footer oc =
    match response_encoding with
    (* Copied from [cohttp/response.ml]. *)
    | Transfer.Chunked -> Eio.Buf_write.string oc "0\r\n\r\n"
    | Transfer.Fixed _ | Transfer.Unknown -> ()
  in
  ( res
  , fun _ic oc ->
      (* TODO: Use [Cohttp_eio.Server.respond] instead of internal APIs. This requires turning [Body.t] into a [Eio.Flow]. *)
      let writer = R.make_body_writer ~flush:false res oc in
      let body, _ = t.a_body in
      let () = body (R.write_body writer) in
      write_footer oc; Eio.Buf_write.flush oc )

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
  Cohttp.Header.get h (Ocsigen_header.Name.to_string id)

let header_multi {a_response; _} id =
  let h = Cohttp.Response.headers a_response in
  Cohttp.Header.get_multi h (Ocsigen_header.Name.to_string id)

let add_header
      ({a_response = {Cohttp.Response.headers; _} as a_response; _} as a)
      id
      v
  =
  { a with
    a_response =
      { a_response with
        Cohttp.Response.headers =
          Cohttp.Header.add headers (Ocsigen_header.Name.to_string id) v } }

let add_header_multi
      ({a_response = {Cohttp.Response.headers; _} as a_response; _} as a)
      id
      l
  =
  let id = Ocsigen_header.Name.to_string id in
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
          Cohttp.Header.replace headers (Ocsigen_header.Name.to_string id) v }
  }

let replace_headers ({a_response; _} as a) l =
  let headers =
    List.fold_left
      (fun headers (id, content) ->
         Cohttp.Header.replace headers
           (Ocsigen_header.Name.to_string id)
           content)
      (Cohttp.Response.headers a_response)
      l
  in
  {a with a_response = {a_response with Cohttp.Response.headers}}

let remove_header ({a_response; _} as a) id =
  let headers = Cohttp.Response.headers a_response
  and id = Ocsigen_header.Name.to_string id in
  let headers = Cohttp.Header.remove headers id in
  {a with a_response = {a_response with Cohttp.Response.headers}}
