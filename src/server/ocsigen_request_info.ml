open Ocsigen_lib
open Ocsigen_cookies

(* Requests *)
type ifrange = IR_No | IR_Ifunmodsince of float | IR_ifmatch of string

type file_info = {
  tmp_filename: string;
  filesize: int64;
  raw_original_filename: string;
  original_basename: string ;
  file_content_type: ((string * string) * (string * string) list) option;
}

type request_info =
  {ri_url_string: string; (** full URL *)
   ri_method: Ocsigen_http_frame.Http_header.http_method; (** GET, POST, HEAD... *)
   ri_protocol: Ocsigen_http_frame.Http_header.proto; (** HTTP protocol used by client *)
   ri_ssl: bool; (** true if HTTPS, false if HTTP *)
   ri_full_path_string: string; (** full path of the URL *)
   ri_full_path: string list;   (** full path of the URL *)
   ri_original_full_path_string: string;   (** full path of the URL, as first sent by the client. Should not be changed by extensions, even rewritemod. It is used to create relative links. *)
   ri_original_full_path: string list;   (** full path of the URL, as first sent by the client. See below. *)
   ri_sub_path: string list;   (** path of the URL (only part concerning the site) *)
   ri_sub_path_string: string;   (** path of the URL (only part concerning the site) *)
   ri_get_params_string: string option; (** string containing GET parameters *)
   ri_host: string option; (** Host field of the request (if any), without port *)
   ri_port_from_host_field: int option; (** Port in the host field of the request (if any) *)
   ri_get_params: (string * string) list Lazy.t;  (** Association list of get parameters *)
   ri_initial_get_params: (string * string) list Lazy.t;  (** Association list of get parameters, as sent by the browser (must not be modified by extensions) *)
   ri_post_params: ((string option * Int64.t option) -> (string * string) list Lwt.t) option; (** Association list of post parameters, if urlencoded form parameters or multipart data. None if other content type or no content. *)
   ri_files: ((string option * Int64.t option) -> (string * file_info) list Lwt.t) option; (** Files sent in the request (multipart data). None if other content type or no content. *)
   ri_remote_inet_addr: Unix.inet_addr; (** IP of the client *)
   ri_remote_ip: string;            (** IP of the client *)
   ri_remote_ip_parsed: Ipaddr.t Lazy.t;    (** IP of the client, parsed *)
   ri_remote_port: int;      (** Port used by the client *)
   ri_forward_ip: string list; (** IPs of gateways the request went throught *)
   ri_server_port: int;      (** Port of the request (server) *)
   ri_user_agent: string;    (** User_agent of the browser *)
   ri_cookies_string: string option Lazy.t; (** Cookies sent by the browser *)
   ri_cookies: string CookiesTable.t Lazy.t;  (** Cookies sent by the browser *)
   ri_ifmodifiedsince: float option;   (** if-modified-since field *)
   ri_ifunmodifiedsince: float option;   (** if-unmodified-since field *)
   ri_ifnonematch: string list option;   (** if-none-match field ( * and weak entity tags not implemented) *)
   ri_ifmatch: string list option;   (** if-match field ( * not implemented) *)
   ri_content_type: ((string * string) * (string * string) list) option; (** Content-Type HTTP header *)
   ri_content_type_string: string option; (** Content-Type HTTP header *)
   ri_content_length: int64 option; (** Content-Length HTTP header *)
   ri_referer: string option Lazy.t; (** Referer HTTP header *)

   ri_origin: string option Lazy.t;
   (** Where the cross-origin request or preflight request originates from.
       http://www.w3.org/TR/cors/#origin-request-header *)
   ri_access_control_request_method : string option Lazy.t;
   (** which method will be used in the actual request as part of
       the preflight request.
       http://www.w3.org/TR/cors/#access-control-request-method-request-he*)
   ri_access_control_request_headers : string list option Lazy.t;
   (** Which headers will be used in the actual request as part of
       the preflight request.
       http://www.w3.org/TR/cors/#access-control-request-headers-request-h *)

   ri_accept: Http_headers.accept Lazy.t; (** Accept HTTP header. For example [(Some "text", None)] means ["text/*"]. The float is the "quality" value, if any. The last association list is for other extensions. *)
   ri_accept_charset: (string option * float option) list Lazy.t; (** Accept-Charset HTTP header. [None] for the first value means "*". The float is the "quality" value, if any. *)
   ri_accept_encoding: (string option * float option) list Lazy.t; (** Accept-Encoding HTTP header. [None] for the first value means "*". The float is the "quality" value, if any. *)
   ri_accept_language: (string * float option) list Lazy.t; (** Accept-Language HTTP header. The float is the "quality" value, if any. *)

   ri_http_frame: Ocsigen_http_frame.t; (** The full http_frame *)
   mutable ri_request_cache: Polytables.t;
   (** Use this to put anything you want,
       for example, information for subsequent
       extensions
   *)
   ri_client: Ocsigen_http_com.connection; (** The request connection *)
   ri_range: ((int64 * int64) list * int64 option * ifrange) option Lazy.t;
   (** Range HTTP header. [None] means all the document.
       List of intervals + possibly from an index to the end of the document.
   *)
   ri_timeofday: float; (** An Unix timestamp computed at the beginning of the request *)
   mutable ri_nb_tries: int; (** For internal use:
                                 used to prevent loops of requests *)

   ri_connection_closed: unit Lwt.t; (** a thread waking up when the connection is closed *)
  }

(* used to modify the url in ri (for example for retrying after rewrite) *)
let ri_of_url ?(full_rewrite = false) url ri =
  let (_, host, _, url, path, params, get_params) = Url.parse url in
  let host = match host with
    | Some h -> host
    | None -> ri.ri_host
  in
  let path_string = Url.string_of_url_path ~encode:true path in
  let original_fullpath, original_fullpath_string =
    if full_rewrite
    then (path, path_string)
    else (ri.ri_original_full_path, ri.ri_original_full_path_string)
  in
  (* ri_original_full_path is not changed *)
  {ri with
   ri_url_string = url;
   ri_host = host;
   ri_full_path_string = path_string;
   ri_full_path = path;
   ri_original_full_path_string = original_fullpath_string;
   ri_original_full_path = original_fullpath;
   ri_sub_path = path;
   ri_sub_path_string = path_string;
   ri_get_params_string = params;
   ri_get_params = get_params;
  }

let get_server_address ri =
  let socket = Ocsigen_http_com.connection_fd ri.ri_client in
  match Lwt_ssl.getsockname socket with
  | Unix.ADDR_UNIX _ -> failwith "unix domain socket have no ip"
  | Unix.ADDR_INET (addr,port) -> addr,port

let make
    ~ri_url_string
    ~ri_method
    ~ri_protocol
    ?(ri_ssl=false)
    ~ri_full_path_string
    ~ri_full_path
    ?(ri_original_full_path_string=ri_full_path_string)
    ?(ri_original_full_path=ri_full_path)
    ?(ri_sub_path=ri_full_path)
    ?(ri_sub_path_string=Url.string_of_url_path ~encode:true ri_full_path)
    ~ri_get_params_string
    ~ri_host
    ~ri_port_from_host_field
    ~ri_get_params
    ?(ri_initial_get_params=ri_get_params)
    ~ri_post_params
    ~ri_files
    ~ri_remote_inet_addr
    ~ri_remote_ip
    ?(ri_remote_ip_parsed=lazy (Ipaddr.of_string_exn ri_remote_ip))
    ~ri_remote_port
    ?(ri_forward_ip=[])
    ~ri_server_port
    ~ri_user_agent
    ~ri_cookies_string
    ~ri_cookies
    ~ri_ifmodifiedsince
    ~ri_ifunmodifiedsince
    ~ri_ifnonematch
    ~ri_ifmatch
    ~ri_content_type
    ~ri_content_type_string
    ~ri_content_length
    ~ri_referer
    ~ri_origin
    ~ri_access_control_request_method
    ~ri_access_control_request_headers
    ~ri_accept
    ~ri_accept_charset
    ~ri_accept_encoding
    ~ri_accept_language
    ~ri_http_frame
    ?(ri_request_cache=Polytables.create ())
    ~ri_client
    ~ri_range
    (* XXX: We should have this line but it would produce a circular dependency
     * between the two modules
     *
     * ?(ri_range=lazy (Ocsigen_range.get_range ri_http_frame)) *)
    ?(ri_timeofday=Unix.gettimeofday ())
    ?(ri_nb_tries=0)
    ?(ri_connection_closed=Ocsigen_http_com.closed ri_client)
    () =
  {
    ri_url_string;
    ri_method;
    ri_protocol;
    ri_ssl;
    ri_full_path_string;
    ri_full_path;
    ri_original_full_path_string;
    ri_original_full_path;
    ri_sub_path;
    ri_sub_path_string;
    ri_get_params_string;
    ri_host;
    ri_port_from_host_field;
    ri_get_params;
    ri_initial_get_params;
    ri_post_params;
    ri_files;
    ri_remote_inet_addr;
    ri_remote_ip;
    ri_remote_ip_parsed;
    ri_remote_port;
    ri_forward_ip;
    ri_server_port;
    ri_user_agent;
    ri_cookies_string;
    ri_cookies;
    ri_ifmodifiedsince;
    ri_ifunmodifiedsince;
    ri_ifnonematch;
    ri_ifmatch;
    ri_content_type;
    ri_content_type_string;
    ri_content_length;
    ri_referer;
    ri_origin;
    ri_access_control_request_method;
    ri_access_control_request_headers;
    ri_accept;
    ri_accept_charset;
    ri_accept_encoding;
    ri_accept_language;
    ri_http_frame;
    ri_request_cache;
    ri_client;
    ri_range;
    ri_timeofday;
    ri_nb_tries;
    ri_connection_closed;
  }

let update ri
    ?(ri_url_string=ri.ri_url_string)
    ?(ri_method=ri.ri_method)
    ?(ri_protocol=ri.ri_protocol)
    ?(ri_ssl=ri.ri_ssl)
    ?(ri_full_path_string=ri.ri_full_path_string)
    ?(ri_full_path=ri.ri_full_path)
    ?(ri_original_full_path_string=ri.ri_original_full_path_string)
    ?(ri_original_full_path=ri.ri_original_full_path)
    ?(ri_sub_path=ri.ri_sub_path)
    ?(ri_sub_path_string=ri.ri_sub_path_string)
    ?(ri_get_params_string=ri.ri_get_params_string)
    ?(ri_host=ri.ri_host)
    ?(ri_port_from_host_field=ri.ri_port_from_host_field)
    ?(ri_get_params=ri.ri_get_params)
    ?(ri_initial_get_params=ri.ri_initial_get_params)
    ?(ri_post_params=ri.ri_post_params)
    ?(ri_files=ri.ri_files)
    ?(ri_remote_inet_addr=ri.ri_remote_inet_addr)
    ?(ri_remote_ip=ri.ri_remote_ip)
    ?(ri_remote_ip_parsed=ri.ri_remote_ip_parsed)
    ?(ri_remote_port=ri.ri_remote_port)
    ?(ri_forward_ip=ri.ri_forward_ip)
    ?(ri_server_port=ri.ri_server_port)
    ?(ri_user_agent=ri.ri_user_agent)
    ?(ri_cookies_string=ri.ri_cookies_string)
    ?(ri_cookies=ri.ri_cookies)
    ?(ri_ifmodifiedsince=ri.ri_ifmodifiedsince)
    ?(ri_ifunmodifiedsince=ri.ri_ifunmodifiedsince)
    ?(ri_ifnonematch=ri.ri_ifnonematch)
    ?(ri_ifmatch=ri.ri_ifmatch)
    ?(ri_content_type=ri.ri_content_type)
    ?(ri_content_type_string=ri.ri_content_type_string)
    ?(ri_content_length=ri.ri_content_length)
    ?(ri_referer=ri.ri_referer)
    ?(ri_origin=ri.ri_origin)
    ?(ri_access_control_request_method=ri.ri_access_control_request_method)
    ?(ri_access_control_request_headers=ri.ri_access_control_request_headers)
    ?(ri_accept=ri.ri_accept)
    ?(ri_accept_charset=ri.ri_accept_charset)
    ?(ri_accept_encoding=ri.ri_accept_encoding)
    ?(ri_accept_language=ri.ri_accept_language)
    ?(ri_http_frame=ri.ri_http_frame)
    ?(ri_request_cache=ri.ri_request_cache)
    ?(ri_client=ri.ri_client)
    ?(ri_range=ri.ri_range)
    ?(ri_timeofday=ri.ri_timeofday)
    ?(ri_nb_tries=ri.ri_nb_tries)
    ?(ri_connection_closed=ri.ri_connection_closed)
    () =
  {
    ri_url_string;
    ri_method;
    ri_protocol;
    ri_ssl;
    ri_full_path_string;
    ri_full_path;
    ri_original_full_path_string;
    ri_original_full_path;
    ri_sub_path;
    ri_sub_path_string;
    ri_get_params_string;
    ri_host;
    ri_port_from_host_field;
    ri_get_params;
    ri_initial_get_params;
    ri_post_params;
    ri_files;
    ri_remote_inet_addr;
    ri_remote_ip;
    ri_remote_ip_parsed;
    ri_remote_port;
    ri_forward_ip;
    ri_server_port;
    ri_user_agent;
    ri_cookies_string;
    ri_cookies;
    ri_ifmodifiedsince;
    ri_ifunmodifiedsince;
    ri_ifnonematch;
    ri_ifmatch;
    ri_content_type;
    ri_content_type_string;
    ri_content_length;
    ri_referer;
    ri_origin;
    ri_access_control_request_method;
    ri_access_control_request_headers;
    ri_accept;
    ri_accept_charset;
    ri_accept_encoding;
    ri_accept_language;
    ri_http_frame;
    ri_request_cache;
    ri_client;
    ri_range;
    ri_timeofday;
    ri_nb_tries;
    ri_connection_closed;
  }

let update_nb_tries ri value = ri.ri_nb_tries <- value

let range { ri_range; _ } = ri_range
let url_string { ri_url_string; _ } = ri_url_string
let protocol { ri_protocol; _ } = ri_protocol
let http_frame { ri_http_frame; _ } = ri_http_frame
let meth { ri_method; _ } = ri_method
let ifmatch { ri_ifmatch; _ } = ri_ifmatch
let ifunmodifiedsince { ri_ifunmodifiedsince; _ } = ri_ifunmodifiedsince
let ifnonematch { ri_ifnonematch; _ } = ri_ifnonematch
let ifmodifiedsince { ri_ifmodifiedsince; _ } = ri_ifmodifiedsince
let remote_ip { ri_remote_ip; _ } = ri_remote_ip
let user_agent { ri_user_agent; } = ri_user_agent
let host { ri_host; _ } = ri_host
let ssl { ri_ssl; _ } = ri_ssl
let port_from_host_field { ri_port_from_host_field; _ } =
  ri_port_from_host_field
let server_port { ri_server_port; _ } = ri_server_port
let full_path { ri_full_path; _ } = ri_full_path
let get_params_string { ri_get_params_string; _ } = ri_get_params_string
let client { ri_client; _ } = ri_client
let nb_tries { ri_nb_tries; _ } = ri_nb_tries
let sub_path { ri_sub_path; _ } = ri_sub_path
let content_length { ri_content_length; _ } = ri_content_length
let content_type_string { ri_content_type_string; _ } = ri_content_type_string
let remote_port { ri_remote_port; _ } = ri_remote_port
let sub_path_string { ri_sub_path_string; _ } = ri_sub_path_string
let full_path_string { ri_full_path_string; _ } = ri_full_path_string
let remote_inet_addr { ri_remote_inet_addr; _ } = ri_remote_inet_addr
let forward_ip { ri_forward_ip; _ } = ri_forward_ip
let remote_ip_parsed { ri_remote_ip_parsed; _ } = ri_remote_ip_parsed
let content_type { ri_content_type; _ } = ri_content_type
let origin { ri_origin; _ } = ri_origin
let access_control_request_method { ri_access_control_request_method; _ } =
  ri_access_control_request_method
let access_control_request_headers { ri_access_control_request_headers; _ } =
  ri_access_control_request_headers
