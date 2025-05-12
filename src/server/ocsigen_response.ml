module Body = struct
  type t = ((string -> unit Lwt.t) -> unit Lwt.t) * Cohttp.Transfer.encoding

  let make encoding writer : t = writer, encoding
  let empty = make (Fixed 0L) (fun _write -> Lwt.return_unit)

  let of_string s =
    make
      (Cohttp.Transfer.Fixed (Int64.of_int (String.length s)))
      (fun write -> write s)

  let of_cohttp body =
    ( (fun write -> Cohttp_lwt.Body.write_body write body)
    , Cohttp_lwt.Body.transfer_encoding body )

  let write (w, _) = w
  let transfer_encoding = snd
end

type t =
  { a_response : Cohttp.Response.t
  ; a_body : Body.t
  ; a_cookies : Ocsigen_cookie_map.t }

let make ?(body = Body.empty) ?(cookies = Ocsigen_cookie_map.empty) a_response =
  {a_response; a_body = body; a_cookies = cookies}

let respond ?headers ~status ?(body = Body.empty) () =
  let encoding =
    match headers with
    | None -> Body.transfer_encoding body
    | Some headers -> (
      match Cohttp.Header.get_transfer_encoding headers with
      | Cohttp.Transfer.Unknown -> Body.transfer_encoding body
      | t -> t)
  in
  let response = Cohttp.Response.make ~status ~encoding ?headers () in
  make ~body response

let update ?response ?body ?cookies {a_response; a_body; a_cookies} =
  let a_response =
    match response with Some response -> response | None -> a_response
  and a_body = match body with Some body -> body | None -> a_body
  and a_cookies =
    match cookies with Some cookies -> cookies | None -> a_cookies
  in
  {a_response; a_body; a_cookies}

let of_cohttp ?(cookies = Ocsigen_cookie_map.empty) (a_response, body) =
  let a_body = Body.of_cohttp body in
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

let to_cohttp_response {a_response; a_cookies; _} =
  let headers =
    Cohttp.Header.add_unless_exists
      (Cohttp.Header.add_unless_exists
         (Ocsigen_cookie_map.Map_path.fold make_cookies_headers a_cookies
            (Cohttp.Response.headers a_response))
         "server" Ocsigen_config.server_name)
      "date"
      (Ocsigen_lib.Date.to_string (Unix.time ()))
  in
  {a_response with Cohttp.Response.headers}

let to_response_expert t =
  to_cohttp_response t, fun _ic oc -> fst t.a_body (Lwt_io.write oc)

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
