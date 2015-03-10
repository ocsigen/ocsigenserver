open Ocsigen_cookies
open Ocsigen_lib

type ifrange = IR_No | IR_Ifunmodsince of float | IR_ifmatch of string

type file_info = {
  tmp_filename: string;
  filesize: int64;
  raw_original_filename: string;
  original_basename: string ;
  file_content_type: ((string * string) * (string * string) list) option;
}

(** The request *)
type request_info =
  {url_string: string; (** full URL *)
   meth: Ocsigen_http_frame.Http_header.http_method; (** GET, POST, HEAD... *)
   protocol: Ocsigen_http_frame.Http_header.proto; (** HTTP protocol used by client *)
   ssl: bool; (** true if HTTPS, false if HTTP *)
   full_path_string: string; (** full path of the URL *)
   full_path: string list;   (** full path of the URL *)
   original_full_path_string: string;   (** full path of the URL, as first sent by the client. Should not be changed by extensions, even rewritemod. It is used to create relative links. *)
   original_full_path: string list;   (** full path of the URL, as first sent by the client. See below. *)
   sub_path: string list;   (** path of the URL (only part concerning the site) *)
   sub_path_string: string;   (** path of the URL (only part concerning the site) *)
   get_params_string: string option; (** string containing GET parameters *)
   host: string option; (** Host field of the request (if any), without port *)
   port_from_host_field: int option; (** Port in the host field of the request (if any) *)
   get_params: (string * string) list Lazy.t;  (** Association list of get parameters *)
   initial_get_params: (string * string) list Lazy.t;  (** Association list of get parameters, as sent by the browser (must not be modified by extensions) *)
   post_params: ((string option * Int64.t option) -> (string * string) list Lwt.t) option; (** Association list of post parameters, if urlencoded form parameters or multipart data. None if other content type or no content. *)
   files: ((string option * Int64.t option) -> (string * file_info) list Lwt.t) option; (** Files sent in the request (multipart data). None if other content type or no content. *)
   remote_inet_addr: Unix.inet_addr; (** IP of the client *)
   remote_ip: string;            (** IP of the client *)
   remote_ip_parsed: Ipaddr.t Lazy.t;    (** IP of the client, parsed *)
   remote_port: int;      (** Port used by the client *)
   forward_ip: string list; (** IPs of gateways the request went throught *)
   server_port: int;      (** Port of the request (server) *)
   user_agent: string;    (** User_agent of the browser *)
   cookies_string: string option Lazy.t; (** Cookies sent by the browser *)
   cookies: string CookiesTable.t Lazy.t;  (** Cookies sent by the browser *)
   ifmodifiedsince: float option;   (** if-modified-since field *)
   ifunmodifiedsince: float option;   (** if-unmodified-since field *)
   ifnonematch: string list option;   (** if-none-match field ( * and weak entity tags not implemented) *)
   ifmatch: string list option;   (** if-match field ( * not implemented) *)
   content_type: ((string * string) * (string * string) list) option; (** Content-Type HTTP header *)
   content_type_string: string option; (** Content-Type HTTP header *)
   content_length: int64 option; (** Content-Length HTTP header *)
   referer: string option Lazy.t; (** Referer HTTP header *)

   origin: string option Lazy.t;
   (** Where the cross-origin request or preflight request originates from.
       http://www.w3.org/TR/cors/#origin-request-header *)
   access_control_request_method : string option Lazy.t;
   (** which method will be used in the actual request as part of
       the preflight request.
       http://www.w3.org/TR/cors/#access-control-request-method-request-he*)
   access_control_request_headers : string list option Lazy.t;
   (** Which headers will be used in the actual request as part of
       the preflight request.
       http://www.w3.org/TR/cors/#access-control-request-headers-request-h *)

   accept: ((string option * string option) * float option * (string * string) list) list Lazy.t; (** Accept HTTP header. For example [(Some "text", None)] means ["text/*"]. The float is the "quality" value, if any. The last association list is for other extensions. *)
   accept_charset: (string option * float option) list Lazy.t; (** Accept-Charset HTTP header. [None] for the first value means "*". The float is the "quality" value, if any. *)
   accept_encoding: (string option * float option) list Lazy.t; (** Accept-Encoding HTTP header. [None] for the first value means "*". The float is the "quality" value, if any. *)
   accept_language: (string * float option) list Lazy.t; (** Accept-Language HTTP header. The float is the "quality" value, if any. *)

   http_frame: Ocsigen_http_frame.t; (** The full http_frame *)
   mutable request_cache: Polytables.t;
   (** Use this to put anything you want,
       for example, information for subsequent
       extensions
   *)
   client: Ocsigen_http_com.connection; (** The request connection *)
   range: ((int64 * int64) list * int64 option * ifrange) option Lazy.t;
   (** Range HTTP header. [None] means all the document.
       List of intervals + possibly from an index to the end of the document.
   *)
   timeofday: float; (** An Unix timestamp computed at the beginning of the request *)
   mutable nb_tries: int; (** For internal use:
                                 used to prevent loops of requests *)

   connection_closed: unit Lwt.t; (** a thread waking up when the connection is closed *)
  }
(** If you force [ri_files] or [ri_post_params], the request is fully read,
    so it is not possible any more to read it from [ri_http_frame]
    (and vice versa).
*)

(* used to modify the url in ri (for example for retrying after rewrite) *)
let ri_of_url ?(full_rewrite = false) url ri =
  let (_, host, _, url, path, params, get_params) = Url.parse url in
  let host = match host with
    | Some h -> host
    | None -> ri.host
  in
  let path_string = Url.string_of_url_path ~encode:true path in
  let original_full_path, original_full_path_string =
    if full_rewrite
    then (path, path_string)
    else (ri.original_full_path, ri.original_full_path_string)
  in
  {ri with
   url_string = url;
   host ;
   full_path_string = path_string;
   full_path = path;
   original_full_path_string ;
   original_full_path ;
   sub_path = path;
   sub_path_string = path_string;
   get_params_string = params;
   get_params ;
   }

let make
    ~url_string
    ~meth
    ~protocol
    ?(ssl=false)
    ~full_path_string
    ~full_path
    ?(original_full_path_string=full_path_string)
    ?(original_full_path=full_path)
    ?(sub_path=full_path)
    ?(sub_path_string=Url.string_of_url_path ~encode:true full_path)
    ~get_params_string
    ~host
    ~port_from_host_field
    ~get_params
    ?(initial_get_params=get_params)
    ~post_params
    ~files
    ~remote_inet_addr
    ~remote_ip
    ?(remote_ip_parsed=lazy (Ipaddr.of_string_exn remote_ip))
    ~remote_port
    ?(forward_ip=[])
    ~server_port
    ~user_agent
    ~cookies_string
    ~cookies
    ~ifmodifiedsince
    ~ifunmodifiedsince
    ~ifnonematch
    ~ifmatch
    ~content_type
    ~content_type_string
    ~content_length
    ~referer
    ~origin
    ~access_control_request_method
    ~access_control_request_headers
    ~accept
    ~accept_charset
    ~accept_encoding
    ~accept_language
    ~http_frame
    ?(request_cache=Polytables.create ())
    ~client
    ~range
    (* XXX: We should have this line but it would produce a circular dependency
     * between the two modules
     *
     * ?(range=lazy (Ocsigen_range.get_range http_frame)) *)
    ?(timeofday=Unix.gettimeofday ())
    ?(nb_tries=0)
    ?(connection_closed=Ocsigen_http_com.closed client)
    () =
  {
    url_string;
    meth;
    protocol;
    ssl;
    full_path_string;
    full_path;
    original_full_path_string;
    original_full_path;
    sub_path;
    sub_path_string;
    get_params_string;
    host;
    port_from_host_field;
    get_params;
    initial_get_params;
    post_params;
    files;
    remote_inet_addr;
    remote_ip;
    remote_ip_parsed;
    remote_port;
    forward_ip;
    server_port;
    user_agent;
    cookies_string;
    cookies;
    ifmodifiedsince;
    ifunmodifiedsince;
    ifnonematch;
    ifmatch;
    content_type;
    content_type_string;
    content_length;
    referer;
    origin;
    access_control_request_method;
    access_control_request_headers;
    accept;
    accept_charset;
    accept_encoding;
    accept_language;
    http_frame;
    request_cache;
    client;
    range;
    timeofday;
    nb_tries;
    connection_closed;
  }

let update ri
    ?(url_string=ri.url_string)
    ?(meth=ri.meth)
    ?(protocol=ri.protocol)
    ?(ssl=ri.ssl)
    ?(full_path_string=ri.full_path_string)
    ?(full_path=ri.full_path)
    ?(original_full_path_string=ri.original_full_path_string)
    ?(original_full_path=ri.original_full_path)
    ?(sub_path=ri.sub_path)
    ?(sub_path_string=ri.sub_path_string)
    ?(get_params_string=ri.get_params_string)
    ?(host=ri.host)
    ?(port_from_host_field=ri.port_from_host_field)
    ?(get_params=ri.get_params)
    ?(initial_get_params=ri.initial_get_params)
    ?(post_params=ri.post_params)
    ?(files=ri.files)
    ?(remote_inet_addr=ri.remote_inet_addr)
    ?(remote_ip=ri.remote_ip)
    ?(remote_ip_parsed=ri.remote_ip_parsed)
    ?(remote_port=ri.remote_port)
    ?(forward_ip=ri.forward_ip)
    ?(server_port=ri.server_port)
    ?(user_agent=ri.user_agent)
    ?(cookies_string=ri.cookies_string)
    ?(cookies=ri.cookies)
    ?(ifmodifiedsince=ri.ifmodifiedsince)
    ?(ifunmodifiedsince=ri.ifunmodifiedsince)
    ?(ifnonematch=ri.ifnonematch)
    ?(ifmatch=ri.ifmatch)
    ?(content_type=ri.content_type)
    ?(content_type_string=ri.content_type_string)
    ?(content_length=ri.content_length)
    ?(referer=ri.referer)
    ?(origin=ri.origin)
    ?(access_control_request_method=ri.access_control_request_method)
    ?(access_control_request_headers=ri.access_control_request_headers)
    ?(accept=ri.accept)
    ?(accept_charset=ri.accept_charset)
    ?(accept_encoding=ri.accept_encoding)
    ?(accept_language=ri.accept_language)
    ?(http_frame=ri.http_frame)
    ?(request_cache=ri.request_cache)
    ?(client=ri.client)
    ?(range=ri.range)
    ?(timeofday=ri.timeofday)
    ?(nb_tries=ri.nb_tries)
    ?(connection_closed=ri.connection_closed)
    () =
  {
    url_string;
    meth;
    protocol;
    ssl;
    full_path_string;
    full_path;
    original_full_path_string;
    original_full_path;
    sub_path;
    sub_path_string;
    get_params_string;
    host;
    port_from_host_field;
    get_params;
    initial_get_params;
    post_params;
    files;
    remote_inet_addr;
    remote_ip;
    remote_ip_parsed;
    remote_port;
    forward_ip;
    server_port;
    user_agent;
    cookies_string;
    cookies;
    ifmodifiedsince;
    ifunmodifiedsince;
    ifnonematch;
    ifmatch;
    content_type;
    content_type_string;
    content_length;
    referer;
    origin;
    access_control_request_method;
    access_control_request_headers;
    accept;
    accept_charset;
    accept_encoding;
    accept_language;
    http_frame;
    request_cache;
    client;
    range;
    timeofday;
    nb_tries;
    connection_closed;
  }

let update_nb_tries ri value = ri.nb_tries <- value
let update_request_cache ri value = ri.request_cache <- value

let range { range; _ } = range
let url_string { url_string; _ } = url_string
let protocol { protocol; _ } = protocol
let http_frame { http_frame; _ } = http_frame
let meth { meth; _ } = meth
let ifmatch { ifmatch; _ } = ifmatch
let ifunmodifiedsince { ifunmodifiedsince; _ } = ifunmodifiedsince
let ifnonematch { ifnonematch; _ } = ifnonematch
let ifmodifiedsince { ifmodifiedsince; _ } = ifmodifiedsince
let remote_ip { remote_ip; _ } = remote_ip
let user_agent { user_agent; } = user_agent
let host { host; _ } = host
let ssl { ssl; _ } = ssl
let port_from_host_field { port_from_host_field; _ } =
  port_from_host_field
let server_port { server_port; _ } = server_port
let full_path { full_path; _ } = full_path
let get_params_string { get_params_string; _ } = get_params_string
let client { client; _ } = client
let nb_tries { nb_tries; _ } = nb_tries
let sub_path { sub_path; _ } = sub_path
let content_length { content_length; _ } = content_length
let content_type_string { content_type_string; _ } = content_type_string
let remote_port { remote_port; _ } = remote_port
let sub_path_string { sub_path_string; _ } = sub_path_string
let full_path_string { full_path_string; _ } = full_path_string
let remote_inet_addr { remote_inet_addr; _ } = remote_inet_addr
let forward_ip { forward_ip; _ } = forward_ip
let remote_ip_parsed { remote_ip_parsed; _ } = remote_ip_parsed
let content_type { content_type; _ } = content_type
let origin { origin; _ } = origin
let access_control_request_method { access_control_request_method; _ } =
  access_control_request_method
let access_control_request_headers { access_control_request_headers; _ } =
  access_control_request_headers
let request_cache { request_cache; _ } = request_cache
let files { files; _ } = files
let original_full_path { original_full_path; _ } = original_full_path
let cookies { cookies; _ } = cookies
let post_params { post_params; _ } = post_params
let get_params { get_params; _ } = get_params
let initial_get_params { initial_get_params; _ } = initial_get_params
let original_full_path_string { original_full_path_string; _ } =
  original_full_path_string
let timeofday { timeofday; _ } = timeofday
let accept_language { accept_language; _ } = accept_language
let accept_encoding { accept_encoding; _ } = accept_encoding
let accept { accept; _ } = accept
let connection_closed { connection_closed; _ } = connection_closed
