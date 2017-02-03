open Lwt.Infix

let target https host ?port uri =
  let scheme = if https then "https" else "http" in
  Uri.resolve scheme (Uri.make ~scheme ~host ?port ()) (Uri.of_string uri)

let post_string ?(https = false) ?port ?(headers = Cohttp.Header.init ())
    ~host ~uri ~content ~content_type () =
  let content_type =
    String.concat "/" [
      fst content_type;
      snd content_type
    ]
  in
  let headers =
    let add n v m =
      Cohttp.Header.add m
        (Http_headers.name_to_string n)
        v
    in
    headers
    |> add Http_headers.content_type content_type
    |> add Http_headers.content_length (string_of_int (String.length content))
  in
  Cohttp_lwt_unix.Client.post
    ~body:(Cohttp_lwt_body.of_string content)
    ~headers
    (target https host ?port uri)

let get ?(https = false) ?port ?headers ~host ~uri () =
  Cohttp_lwt_unix.Client.get ?headers (target https host ?port uri)

let post_urlencoded ?https ?port ?headers ~host ~uri ~content () =
  post_string ?https ?port ?headers
    ~host ~uri
    ~content:(Netencoding.Url.mk_url_encoded_parameters content)
    ~content_type:("application", "x-www-form-urlencoded")
    ()

let basic_raw_request
    ?(headers = Http_headers.empty) ?(https = false) ?port
    ~content ?content_length
    ~meth ~host ~inet_addr ~uri () =
  ignore inet_addr;
  let headers =
    match content_length with
    | Some len ->
      Cohttp.Header.add headers
        Http_headers.(name_to_string content_length)
        (Int64.to_string len)
    | None ->
      headers
  in
  let body =
    match content with
    | Some c ->
      Some (Cohttp_lwt_body.of_stream (Ocsigen_stream.to_lwt_stream c))
    | None ->
      None
  in
  Cohttp_lwt_unix.Client.call ~headers ?body meth
    (target https host ?port uri)

let raw_request
    ?keep_alive ?headers ?https ?port
    ~content ?content_length ~meth ~host ~inet_addr ~uri () () =
  ignore keep_alive;
  basic_raw_request
    ?headers ?https ?port ~content ?content_length
    ~meth ~host ~inet_addr ~uri ()
