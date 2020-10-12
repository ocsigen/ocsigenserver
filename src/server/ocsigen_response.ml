type t = {
  a_response : Cohttp.Response.t ;
  a_body     : Cohttp_lwt.Body.t ;
  a_cookies  : Ocsigen_cookie_map.t
}

let make
    ?(body = Cohttp_lwt.Body.empty)
    ?(cookies = Ocsigen_cookie_map.empty)
    a_response =
  { a_response ; a_body = body ; a_cookies = cookies }

let update
    ?response
    ?body
    ?cookies
    { a_response ; a_body ; a_cookies } =
  let a_response =
    match response with
    | Some response -> response
    | None          -> a_response
  and a_body =
    match body with
    | Some body -> body
    | None      -> a_body
  and a_cookies =
    match cookies with
    | Some cookies -> cookies
    | None         -> a_cookies
  in
  { a_response ; a_body ; a_cookies }

let of_cohttp
    ?(cookies = Ocsigen_cookie_map.empty)
    (a_response, a_body) =
  { a_response ; a_body ; a_cookies = cookies }

let to_cohttp { a_response ; a_body ; _ } = a_response, a_body

let status { a_response = { Cohttp.Response.status; _ } ; _} =
  match status with
  | `Code _ ->
    failwith "FIXME: Cohttp.Code.status_code -> status"
  | #Cohttp.Code.status as a ->
    a

let set_status ({ a_response ; _} as a) status =
  { a with
    a_response = {
      a_response with
      Cohttp.Response.status = (status :> Cohttp.Code.status_code)
    }
  }

let cookies {a_cookies; _} = a_cookies

let add_cookies ({ a_cookies; _ } as a) cookies =
  if cookies = Ocsigen_cookie_map.empty then
    a
  else {
    a with
    a_cookies = Ocsigen_cookie_map.add_multi a_cookies cookies
  }

let header {a_response; _} id =
  let h = Cohttp.Response.headers a_response in
  Cohttp.Header.get h (Ocsigen_header.Name.to_string id)

let header_multi {a_response; _} id =
  let h = Cohttp.Response.headers a_response in
  Cohttp.Header.get_multi h (Ocsigen_header.Name.to_string id)

let add_header
    ({a_response = ({Cohttp.Response.headers; _} as a_response); _} as a)
    id v = {
  a with
  a_response = {
    a_response with
    Cohttp.Response.headers =
      Cohttp.Header.add headers (Ocsigen_header.Name.to_string id) v
  }
}

let add_header_multi
    ({a_response = ({Cohttp.Response.headers; _} as a_response); _} as a)
    id l =
  let id = Ocsigen_header.Name.to_string id in
  let headers =
    List.fold_left
      (fun headers -> Cohttp.Header.add headers id)
      headers
      l
  in
  { a with a_response = { a_response with Cohttp.Response.headers } }

let replace_header
    ({a_response = ({Cohttp.Response.headers; _} as a_response); _} as a)
    id v = {
  a with
  a_response = {
    a_response with
    Cohttp.Response.headers =
      Cohttp.Header.replace headers (Ocsigen_header.Name.to_string id) v
  }
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
  { a with a_response = { a_response with Cohttp.Response.headers } }

let remove_header ({a_response; _} as a) id =
  let headers = Cohttp.Response.headers a_response
  and id = Ocsigen_header.Name.to_string id in
  let headers = Cohttp.Header.remove headers id in
  { a with a_response = { a_response with Cohttp.Response.headers } }
