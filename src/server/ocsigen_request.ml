open Lwt.Infix

let post_data_of_body ~content_type b =
  Cohttp_lwt_body.to_stream b
  |> Ocsigen_stream.of_lwt_stream
  |> Ocsigen_multipart.post_params ~content_type

type content_type = Ocsigen_multipart.content_type

type file_info = Ocsigen_multipart.file_info = {
  tmp_filename : string ;
  filesize : int64 ;
  raw_original_filename : string ;
  file_content_type : content_type option
}

type post_data = Ocsigen_multipart.post_data

type t = {
  r_address : Unix.inet_addr ;
  r_port : int ;
  r_filenames : string list ref ;
  r_sockaddr : Lwt_unix.sockaddr ;
  r_remote_ip : string Lazy.t ;
  r_remote_ip_parsed : Ipaddr.t Lazy.t ;
  r_forward_ip : string list ;
  r_request : Cohttp.Request.t ;
  r_body : Cohttp_lwt_body.t ;
  r_get_params_override : (string * string list) list option ;
  mutable r_post_data_override : post_data Lwt.t option option ;
  r_original_full_path : string option ;
  r_sub_path : string option ;
  r_cookies_override : string Ocsigen_cookies.CookiesTable.t option ;
  mutable r_request_cache : Polytables.t ;
  mutable r_tries : int ;
  r_connection_closed : unit Lwt.t * unit Lwt.u
}

let make
    ?(forward_ip = []) ?sub_path ?original_full_path
    ?(request_cache = Polytables.create ())
    ?cookies_override
    ~address ~port ~filenames ~sockaddr ~request ~body ~waiter () =
  let r_remote_ip =
    lazy
      (Unix.string_of_inet_addr
         (Ocsigen_socket.ip_of_sockaddr sockaddr))
  in
  let r_remote_ip_parsed =
    lazy (Ipaddr.of_string_exn (Lazy.force r_remote_ip))
  and r_connection_closed = Lwt.wait () in
  {
    r_address = address ;
    r_port = port ;
    r_filenames = filenames ;
    r_sockaddr = sockaddr ;
    r_remote_ip ;
    r_remote_ip_parsed ;
    r_forward_ip = forward_ip ;
    r_request = request ;
    r_body = body ;
    r_get_params_override = None ;
    r_post_data_override = None ;
    r_sub_path = sub_path ;
    r_original_full_path = original_full_path ;
    r_cookies_override = cookies_override ;
    r_request_cache = request_cache ;
    r_tries = 0 ;
    r_connection_closed
  }

let update_uri_components ~full_rewrite ~request uri =
  let request =
    let meth = Cohttp.Request.meth request
    and version = Cohttp.Request.version request
    and encoding = Cohttp.Request.encoding request
    and headers = Cohttp.Request.headers request in
    Cohttp.Request.make ~meth ~version ~encoding ~headers uri
  in
  let original_full_path =
    if full_rewrite then
      Some (Uri.path (Cohttp.Request.uri request))
    else
      None
  in
  request, original_full_path

let update
    ?forward_ip ?remote_ip ?ssl ?request
    ?get_params_override ?post_data_override ?cookies_override
    ?(full_rewrite = false) ?uri
    ({
      r_request ;
      r_forward_ip ;
      r_remote_ip ;
      r_remote_ip_parsed ;
      r_get_params_override ;
      r_cookies_override ;
      r_post_data_override ;
      r_sub_path ;
      r_original_full_path
    } as r) =
  (* FIXME : ssl *)
  let r_request =
    match request with
    | Some request ->
      request
    | None ->
      r_request
  and r_forward_ip =
    match forward_ip with
    | Some forward_ip ->
      forward_ip
    | None ->
      r_forward_ip
  and r_remote_ip, r_remote_ip_parsed =
    match remote_ip with
    | Some remote_ip ->
      lazy remote_ip, lazy (Ipaddr.of_string_exn remote_ip)
    | None ->
      r_remote_ip, r_remote_ip_parsed
  and r_post_data_override =
    match post_data_override with
    | Some (Some post_data_override) ->
      Some (Some (Lwt.return post_data_override))
    | Some None ->
      Some None
    | None ->
      r_post_data_override
  and r_cookies_override =
    match cookies_override with
    | Some _ ->
      cookies_override
    | None ->
      r_cookies_override
  and r_get_params_override =
    match get_params_override with
    | Some _ ->
      get_params_override
    | None ->
      r_get_params_override
  in
  let r_request, r_original_full_path =
    match uri with
    | Some uri ->
      update_uri_components ~full_rewrite ~request:r_request uri
    | None ->
      r_request, r_original_full_path
  in
  {
    r with
    r_request ;
    r_forward_ip ;
    r_remote_ip ;
    r_remote_ip_parsed ;
    r_get_params_override ;
    r_post_data_override ;
    r_cookies_override ;
    r_sub_path ;
    r_original_full_path
  }

let uri {r_request} = Cohttp.Request.uri r_request

let request {r_request} =
  r_request


let body {r_body} =
  r_body

let map_cohttp_request ~f ({r_request} as r) =
  {r with r_request = f r_request}

let address {r_address} =
  r_address

let host {r_request} =
  Uri.host (Cohttp.Request.uri r_request)

let meth {r_request} =
  Cohttp.Request.meth r_request

let port {r_port} =
  r_port

let ssl _ =
  (* FIXME *)
  false

let version {r_request} =
  Cohttp.Request.version r_request

let query {r_request} =
  Uri.verbatim_query (Cohttp.Request.uri r_request)

let get_params { r_request ; r_get_params_override } =
  match r_get_params_override with
  | Some r_get_params_override ->
    r_get_params_override
  | None ->
    Uri.query (Cohttp.Request.uri r_request)

let path_string {r_request} =
  Uri.path (Cohttp.Request.uri r_request)

let path r =
  (* CHECKME *)
  match Ocsigen_lib.Url.split_path (path_string r) with
  | "" :: path ->
    path
  | path ->
    path

let sub_path_string = function
  | {r_sub_path = Some r_sub_path} ->
    r_sub_path
  | r ->
    path_string r

let sub_path r =
  match Ocsigen_lib.Url.split_path (sub_path_string r) with
  | "" :: path ->
    path
  | path ->
    path

let original_full_path_string = function
  | {r_original_full_path = Some r_original_full_path} ->
    r_original_full_path
  | r ->
    path_string r

let original_full_path r =
  Ocsigen_lib.Url.split_path (original_full_path_string r)

let header {r_request} id =
  let h = Cohttp.Request.headers r_request in
  Cohttp.Header.get h (Http_headers.name_to_string id)

let header_multi {r_request} id =
  let h = Cohttp.Request.headers r_request in
  Cohttp.Header.get_multi h (Http_headers.name_to_string id)

let add_header r id v =
  let f ({Cohttp.Request.headers} as r) =
    let headers =
      Cohttp.Header.add headers
        (Http_headers.name_to_string id)
        v
    in
    { r with Cohttp.Request.headers }
  in
  map_cohttp_request r ~f

let parse_cookies s =
  let splitted = Ocsigen_lib.String.split ';' s in
  try
    List.fold_left
      (fun beg a ->
         let (n, v) = Ocsigen_lib.String.sep '=' a in
         Ocsigen_cookies.CookiesTable.add n v beg)
      Ocsigen_cookies.CookiesTable.empty
      splitted
  with _ ->
    Ocsigen_cookies.CookiesTable.empty

let cookies = function
  | {r_cookies_override = Some cookies} ->
    cookies
  | r ->
    match header r Http_headers.cookie with
    | Some cookies ->
      parse_cookies cookies
    | None ->
      Ocsigen_cookies.CookiesTable.empty

let content_type r =
  match header r Http_headers.content_type with
  | Some content_type ->
    Ocsigen_multipart.parse_content_type content_type
  | None ->
    None

let force_post_data ({r_post_data_override ; r_body} as r) s i =
  match r_post_data_override with
  | Some r_post_data_override ->
    r_post_data_override
  | None ->
    let v =
      match content_type r with
      | Some content_type ->
        (match
           post_data_of_body ~content_type r_body
         with
         | Some f ->
           Some (f s i)
         | None ->
           None)
      | None ->
        None
    in
    r.r_post_data_override <- Some v;
    v

let post_params r s i =
  match force_post_data r s i with
  | Some v ->
    Some (v >|= fst)
  | None ->
    None

let files r s i =
  match force_post_data r s i with
  | Some v ->
    Some (v >|= snd)
  | None ->
    None

let remote_ip {r_remote_ip} = Lazy.force r_remote_ip

let remote_ip_parsed {r_remote_ip_parsed} = Lazy.force r_remote_ip_parsed

let forward_ip {r_forward_ip} = r_forward_ip

let request_cache {r_request_cache} = r_request_cache

let tries {r_tries} = r_tries

let incr_tries r = r.r_tries <- r.r_tries + 1

let connection_closed {r_connection_closed = (wait, _)} = wait

let wakeup {r_connection_closed = (_, wakeup)} = Lwt.wakeup wakeup ()
