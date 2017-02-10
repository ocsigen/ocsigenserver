type t = {
  a_response : Cohttp.Response.t ;
  a_body     : Cohttp_lwt_body.t ;
  a_cookies  : Ocsigen_cookies.cookieset
}

let make
    ?(body = Cohttp_lwt_body.empty)
    ?(cookies = Ocsigen_cookies.empty_cookieset)
    ~response () =
  { a_response = response ; a_body = body ; a_cookies = cookies }

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
    ?(cookies = Ocsigen_cookies.empty_cookieset)
    (a_response, a_body) =
  { a_response ; a_body ; a_cookies = cookies }

let to_cohttp { a_response ; a_body } = a_response, a_body

let cookies {a_cookies} = a_cookies

let set_status ({ a_response } as a) status =
  { a with
    a_response = {
      a_response with status = (status :> Cohttp.Code.status_code)
    }
  }

let add_cookies ({ a_cookies } as a) cookies =
  if cookies = Ocsigen_cookies.Cookies.empty then
    a
  else {
    a with
    a_cookies = Ocsigen_cookies.add_cookies a_cookies cookies
  }

let header {a_response} id =
  let h = Cohttp.Response.headers a_response in
  Cohttp.Header.get h (Http_headers.name_to_string id)

let header_multi {a_response} id =
  let h = Cohttp.Response.headers a_response in
  Cohttp.Header.get_multi h (Http_headers.name_to_string id)

let add_header
    ({a_response = ({headers} as a_response)} as a)
    id v = {
  a with
  a_response = {
    a_response with
    headers =
      Cohttp.Header.add headers (Http_headers.name_to_string id) v
  }
}

let add_header_multi
    ({a_response = ({headers} as a_response)} as a)
    id l =
  let id = Http_headers.name_to_string id in
  let headers =
    List.fold_left
      (fun headers -> Cohttp.Header.add headers id)
      headers
      l
  in
  { a with a_response = { a_response with headers } }

let remove_header ({a_response} as a) id =
  let headers = Cohttp.Response.headers a_response
  and id = Http_headers.name_to_string id in
  let headers = Cohttp.Header.remove headers id in
  { a with a_response = { a_response with headers } }

let replace_header
    ({a_response = ({headers} as a_response)} as a)
    id v = {
  a with
  a_response = {
    a_response with
    headers =
      Cohttp.Header.replace headers (Http_headers.name_to_string id) v
  }
}

let replace_headers ({a_response} as a) l =
  let headers =
    List.fold_left
      (fun headers (id, content) ->
         Cohttp.Header.replace headers
           (Http_headers.name_to_string id)
           content)
      (Cohttp.Response.headers a_response)
      l
  in
  { a with a_response = { a_response with headers } }

let status { a_response = { Cohttp.Response.status } } =
  match status with
  | `Code _ ->
    failwith "FIXME: Cohttp.Code.status_code -> status"
  | #Cohttp.Code.status as a ->
    a
