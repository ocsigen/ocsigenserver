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
  r_sub_path : string option ;
  r_waiter : unit Lwt.t ;
  mutable r_tries : int
}

(* FIXME: old ocsigenserver used to store original_full_path. Do we
   really need that? *)

let make
    ?(forward_ip = []) ?sub_path
    ~address ~port ~filenames ~sockaddr ~request ~body ~waiter () =
  let r_remote_ip =
    lazy
      (Unix.string_of_inet_addr
         (Ocsigen_socket.ip_of_sockaddr sockaddr))
  in
  let r_remote_ip_parsed =
    lazy (Ipaddr.of_string_exn (Lazy.force r_remote_ip))
  in
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
    r_sub_path = sub_path ;
    r_waiter = waiter ;
    r_tries = 0
  }

let update
    ?forward_ip ?remote_ip ?ssl ?request
    ({
      r_request ;
      r_forward_ip ;
      r_remote_ip ;
      r_remote_ip_parsed
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
  in
  { r with r_request ; r_forward_ip ; r_remote_ip ; r_remote_ip_parsed }

let update_url ?(full_rewrite = false) url ({r_request} as r) =
  let r_request =
    let meth = Cohttp.Request.meth r_request
    and version = Cohttp.Request.version r_request
    and encoding = Cohttp.Request.encoding r_request
    and headers = Cohttp.Request.headers r_request in
    Cohttp.Request.make ~meth ~version ~encoding ~headers url
  and r_sub_path = None in
  { r with r_request ; r_sub_path }

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

let path_string {r_request} =
  Uri.path (Cohttp.Request.uri r_request)

let path r =
  Ocsigen_lib.Url.split_path (path_string r)

let sub_path_string = function
  | {r_sub_path = Some r_sub_path} ->
    r_sub_path
  | r ->
    path_string r

let sub_path r =
  Ocsigen_lib.Url.split_path (sub_path_string r)

let header {r_request} id =
  let h = Cohttp.Request.headers r_request in
  Cohttp.Header.get h (Http_headers.name_to_string id)

let header_multi {r_request} id =
  let h = Cohttp.Request.headers r_request in
  Cohttp.Header.get_multi h (Http_headers.name_to_string id)

let remote_ip {r_remote_ip} = Lazy.force r_remote_ip

let remote_ip_parsed {r_remote_ip_parsed} = Lazy.force r_remote_ip_parsed

let forward_ip {r_forward_ip} = r_forward_ip

let tries {r_tries} = r_tries

let incr_tries r = r.r_tries <- r.r_tries + 1
