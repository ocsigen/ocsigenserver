open Lwt.Infix

let post_data_of_body ~content_type b =
  Ocsigen_multipart.post_params ~content_type b

type content_type = Ocsigen_multipart.content_type

type file_info = Ocsigen_multipart.file_info = {
  tmp_filename : string ;
  filesize : int64 ;
  raw_original_filename : string ;
  file_content_type : content_type option
}

type post_data = Ocsigen_multipart.post_data

type body = [
  | `Unparsed of Cohttp_lwt.Body.t
  | `Parsed of post_data Lwt.t
]

(* Wrapper around Uri providing our derived fields.

   Is the laziness too fine-grained? *)
type uri = {
  u_uri : Uri.t Lazy.t ;
  u_get_params : (string * string list) list Lazy.t ;
  u_get_params_flat : (string * string) list Lazy.t ;
  u_path_string : string Lazy.t ;
  u_path : string list Lazy.t ;
}

let unflatten_get_params l =
  let module M = Ocsigen_lib.String.Table in
  M.bindings
    (List.fold_left
       (fun acc (id, v) ->
          M.add id (try v :: M.find id acc with Not_found -> [v]) acc)
       M.empty
       l)

let flatten_get_params l =
  List.concat (List.map (fun (s, l) -> List.map (fun v -> s, v) l) l)

let remove_trailing_slash_string s =
  if String.length s > 0 && String.get s 0 = '/' then
    String.sub s 1 (String.length s - 1)
  else
    s

let make_uri u =
  let u_uri = lazy u
  and u_get_params = lazy (Uri.query u)
  and u_path_string = lazy (remove_trailing_slash_string (Uri.path u)) in
  let u_path = lazy (Ocsigen_lib.Url.split_path (Lazy.force u_path_string))
  and u_get_params_flat = lazy (
    flatten_get_params (Lazy.force u_get_params)
  ) in
  { u_uri ; u_get_params ; u_get_params_flat ; u_path ; u_path_string }

type t = {
  r_address : Unix.inet_addr ;
  r_port : int ;
  r_ssl : bool ;
  r_filenames : string list ref ;
  r_sockaddr : Lwt_unix.sockaddr ;
  r_remote_ip : string Lazy.t ;
  r_remote_ip_parsed : Ipaddr.t Lazy.t ;
  r_forward_ip : string list ;
  r_uri : uri ;
  r_meth : Cohttp.Code.meth ;
  r_encoding : Cohttp.Transfer.encoding ;
  r_version : Cohttp.Code.version ;
  r_headers : Cohttp.Header.t ;
  r_body : body ref ;
  r_original_full_path : string option ;
  r_sub_path : string option ;
  r_cookies_override : string Ocsigen_cookie_map.Map_inner.t option ;
  mutable r_request_cache : Polytables.t ;
  mutable r_tries : int ;
  r_connection_closed : unit Lwt.t;
  r_timeofday : float
}

let make
    ?(forward_ip = []) ?sub_path ?original_full_path
    ?(request_cache = Polytables.create ())
    ?cookies_override
    ~address ~port ~ssl ~filenames ~sockaddr ~body ~connection_closed
    request =
  let r_remote_ip =
    lazy
      (Unix.string_of_inet_addr
         (Ocsigen_lib.Ip_address.of_sockaddr sockaddr))
  in
  let r_remote_ip_parsed =
    lazy (Ipaddr.of_string_exn (Lazy.force r_remote_ip))
  in
  {
    r_address = address ;
    r_port = port ;
    r_ssl = ssl ;
    r_filenames = filenames ;
    r_sockaddr = sockaddr ;
    r_remote_ip ;
    r_remote_ip_parsed ;
    r_forward_ip = forward_ip ;
    r_uri = make_uri (Cohttp.Request.uri request) ;
    r_encoding = Cohttp.Request.encoding request ;
    r_meth = Cohttp.Request.meth request ;
    r_version = Cohttp.Request.version request ;
    r_headers = Cohttp.Request.headers request ;
    r_body = ref (`Unparsed body);
    r_sub_path = sub_path ;
    r_original_full_path = original_full_path ;
    r_cookies_override = cookies_override ;
    r_request_cache = request_cache ;
    r_tries = 0 ;
    r_connection_closed = connection_closed ;
    r_timeofday = Unix.gettimeofday ()
  }

let path_string {r_uri = {u_path_string;_};_} =
  Lazy.force u_path_string

let path {r_uri = {u_path;_};_} =
  Lazy.force u_path

let update
    ?ssl ?forward_ip ?remote_ip ?sub_path
    ?meth
    ?get_params_flat
    ?post_data
    ?cookies_override
    ?(full_rewrite = false) ?uri
    ({
      r_ssl ;
      r_uri = {u_uri;_} as r_uri;
      r_meth ;
      r_forward_ip ;
      r_remote_ip ;
      r_remote_ip_parsed ;
      r_cookies_override ;
      r_body ;
      r_sub_path ;
      r_original_full_path
      ;_
    } as r) =
  let r_ssl =
    match ssl with
    | Some ssl ->
      ssl
    | None ->
      r_ssl
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
  and r_sub_path =
    match sub_path with
    | Some _ ->
      sub_path
    | None ->
      r_sub_path
  and r_body =
    match post_data with
    | Some (Some post_data) ->
      ref (`Parsed (Lwt.return post_data))
    | None | Some None ->
      r_body
  and r_cookies_override =
    match cookies_override with
    | Some _ ->
      cookies_override
    | None ->
      r_cookies_override
  and r_meth =
    match meth with
    | Some meth ->
      meth
    | None ->
      r_meth
  and r_original_full_path, r_uri =
    match uri with
    | Some uri ->
      (match full_rewrite, r_original_full_path with
       | true, _ ->
         None
       | false, Some _ ->
         r_original_full_path
       | false, _ ->
         Some (Uri.path (Lazy.force u_uri))),
      make_uri uri
    | None ->
      r_original_full_path, r_uri
  in
  let r_uri =
    match get_params_flat with
    | Some l ->
      let u_get_params = lazy (unflatten_get_params l) in
      let u_uri = lazy (
        Uri.with_query
          (Lazy.force r_uri.u_uri)
          (Lazy.force u_get_params)
      ) in
      { r_uri with
        u_uri ;
        u_get_params ;
        u_get_params_flat = lazy l
      }
    | None ->
      r_uri
  in {
    r with
    r_ssl ;
    r_uri ;
    r_meth ;
    r_forward_ip ;
    r_remote_ip ;
    r_remote_ip_parsed ;
    r_body ;
    r_cookies_override ;
    r_sub_path ;
    r_original_full_path ;
  }

let uri {r_uri = {u_uri;_};_} =
  Lazy.force u_uri

let to_cohttp ({ r_meth ; r_encoding ; r_version ; r_headers ;_} as r) =
  Cohttp.Request.make
    ~meth:r_meth ~encoding:r_encoding ~version:r_version ~headers:r_headers
    (uri r)

let body = function
  | {r_body = {contents = `Unparsed body;_};_} ->
    body
  | _ ->
    failwith "Ocsigen_request.body: body has already been parsed"

let address {r_address;_} = r_address

let host {r_uri = {u_uri;_};_} =
  Uri.host (Lazy.force u_uri)

let meth {r_meth;_} = r_meth

let port {r_port;_} = r_port

let ssl {r_ssl;_} = r_ssl

let version {r_version;_} = r_version

let query {r_uri = {u_uri;_};_} =
  Uri.verbatim_query (Lazy.force u_uri)

let get_params {r_uri = { u_get_params;_};_} =
  Lazy.force u_get_params

let get_params_flat {r_uri = { u_get_params_flat;_};_} =
  Lazy.force u_get_params_flat

let sub_path_string req =
  remove_trailing_slash_string
    (match req with
     | {r_sub_path = Some r_sub_path;_} ->
       r_sub_path
     | r ->
       path_string r)

let sub_path r =
  Ocsigen_lib.Url.split_path (sub_path_string r)

let original_full_path_string = function
  | {r_original_full_path = Some r_original_full_path;_} ->
    r_original_full_path
  | r ->
    path_string r

let original_full_path r =
  Ocsigen_lib.Url.split_path (original_full_path_string r)

let header {r_headers;_} id =
  Cohttp.Header.get r_headers (Ocsigen_header.Name.to_string id)

let header_multi {r_headers;_} id =
  Cohttp.Header.get_multi r_headers (Ocsigen_header.Name.to_string id)

let add_header ({r_headers;_} as r) id v =
  { r with
    r_headers =
      Cohttp.Header.add r_headers
        (Ocsigen_header.Name.to_string id)
        v
  }

let parse_cookies s =
  let splitted = Ocsigen_lib.String.split ';' s in
  try
    List.fold_left
      (fun beg a ->
         try
           let (n, v) = Ocsigen_lib.String.sep '=' a in
           Ocsigen_cookie_map.Map_inner.add n v beg
         with Not_found ->
           beg)
      Ocsigen_cookie_map.Map_inner.empty
      splitted
  with _ ->
    Ocsigen_cookie_map.Map_inner.empty

let cookies = function
  | {r_cookies_override = Some cookies;_} ->
    cookies
  | r ->
    match header r Ocsigen_header.Name.cookie with
    | Some cookies ->
      parse_cookies cookies
    | None ->
      Ocsigen_cookie_map.Map_inner.empty

let content_type r =
  match header r Ocsigen_header.Name.content_type with
  | Some content_type ->
    Ocsigen_multipart.parse_content_type content_type
  | None ->
    None

let force_post_data ({r_body;_} as r) s i =
  match !r_body with
  | `Parsed post_data ->
    Some post_data
  | `Unparsed body ->
    match content_type r with
    | Some content_type ->
      (match
         post_data_of_body ~content_type body
       with
       | Some f ->
         let v = f s i in
         r.r_body := `Parsed v;
         Some v
       | None ->
         None)
    | None ->
      None

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

let remote_ip {r_remote_ip;_} = Lazy.force r_remote_ip

let remote_ip_parsed {r_remote_ip_parsed;_} = Lazy.force r_remote_ip_parsed

let forward_ip {r_forward_ip;_} = r_forward_ip

let request_cache {r_request_cache;_} = r_request_cache

let tries {r_tries;_} = r_tries

let incr_tries r = r.r_tries <- r.r_tries + 1

let connection_closed {r_connection_closed;_} = r_connection_closed

let timeofday {r_timeofday;_} = r_timeofday
