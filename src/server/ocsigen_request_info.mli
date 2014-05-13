open Ocsigen_cookies

type ifrange = IR_No | IR_Ifunmodsince of float | IR_ifmatch of string

type file_info = {
    tmp_filename: string;
    filesize: int64;
    raw_original_filename: string;
    original_basename: string ;
    file_content_type: ((string * string) * (string * string) list) option;
  }

(** The request info *)
type request_info

(** Parsing URLs.
   This allows to modify the URL in the request_info.
   (to be used for example with Ext_retry_with or Ext_continue_with)
 *)
val ri_of_url : ?full_rewrite:bool -> string -> request_info -> request_info

val get_server_address : request_info -> Unix.inet_addr * int

(** Make a request_info *)
val make :
  ri_url_string:string ->
  ri_method:Ocsigen_http_frame.Http_header.http_method ->
  ri_protocol:Ocsigen_http_frame.Http_header.proto ->
  ?ri_ssl:bool ->
  ri_full_path_string:string ->
  ri_full_path:Ocsigen_lib.Url.path ->
  ?ri_original_full_path_string:string ->
  ?ri_original_full_path:Ocsigen_lib.Url.path ->
  ?ri_sub_path:Ocsigen_lib.Url.path ->
  ?ri_sub_path_string:Ocsigen_lib.Url.uri ->
  ri_get_params_string:string option ->
  ri_host:string option ->
  ri_port_from_host_field:int option ->
  ri_get_params:(string * string) list Lazy.t ->
  ?ri_initial_get_params:(string * string) list Lazy.t ->
  ri_post_params:(string option * Int64.t option ->
                  (string * string) list Lwt.t)
                 option ->
  ri_files:(string option * Int64.t option ->
            (string * file_info) list Lwt.t)
           option ->
  ri_remote_inet_addr:Unix.inet_addr ->
  ri_remote_ip:string ->
  ?ri_remote_ip_parsed:Ipaddr.t Lazy.t ->
  ri_remote_port:int ->
  ?ri_forward_ip:string list ->
  ri_server_port:int ->
  ri_user_agent:string ->
  ri_cookies_string:string option Lazy.t ->
  ri_cookies:string Ocsigen_cookies.CookiesTable.t Lazy.t ->
  ri_ifmodifiedsince:float option ->
  ri_ifunmodifiedsince:float option ->
  ri_ifnonematch:string list option ->
  ri_ifmatch:string list option ->
  ri_content_type:((string * string) * (string * string) list)
                  option ->
  ri_content_type_string:string option ->
  ri_content_length:int64 option ->
  ri_referer:string option Lazy.t ->
  ri_origin:string option Lazy.t ->
  ri_access_control_request_method:string option Lazy.t ->
  ri_access_control_request_headers:string list option Lazy.t ->
  ri_accept:Http_headers.accept Lazy.t ->
  ri_accept_charset:(string option * float option) list Lazy.t ->
  ri_accept_encoding:(string option * float option) list Lazy.t ->
  ri_accept_language:(string * float option) list Lazy.t ->
  ri_http_frame:Ocsigen_http_frame.t ->
  ?ri_request_cache:Polytables.t ->
  ri_client:Ocsigen_http_com.connection ->
  ri_range:((int64 * int64) list * int64 option * ifrange) option
           Lazy.t ->
  ?ri_timeofday:float ->
  ?ri_nb_tries:int ->
  ?ri_connection_closed:unit Lwt.t -> unit -> request_info

val update :
  request_info ->
  ?ri_url_string:string ->
  ?ri_method:Ocsigen_http_frame.Http_header.http_method ->
  ?ri_protocol:Ocsigen_http_frame.Http_header.proto ->
  ?ri_ssl:bool ->
  ?ri_full_path_string:string ->
  ?ri_full_path:string list ->
  ?ri_original_full_path_string:string ->
  ?ri_original_full_path:string list ->
  ?ri_sub_path:string list ->
  ?ri_sub_path_string:string ->
  ?ri_get_params_string:string option ->
  ?ri_host:string option ->
  ?ri_port_from_host_field:int option ->
  ?ri_get_params:(string * string) list Lazy.t ->
  ?ri_initial_get_params:(string * string) list Lazy.t ->
  ?ri_post_params:(string option * Int64.t option ->
                   (string * string) list Lwt.t)
                  option ->
  ?ri_files:(string option * Int64.t option ->
             (string * file_info) list Lwt.t)
            option ->
  ?ri_remote_inet_addr:Unix.inet_addr ->
  ?ri_remote_ip:string ->
  ?ri_remote_ip_parsed:Ipaddr.t Lazy.t ->
  ?ri_remote_port:int ->
  ?ri_forward_ip:string list ->
  ?ri_server_port:int ->
  ?ri_user_agent:string ->
  ?ri_cookies_string:string option Lazy.t ->
  ?ri_cookies:string Ocsigen_cookies.CookiesTable.t Lazy.t ->
  ?ri_ifmodifiedsince:float option ->
  ?ri_ifunmodifiedsince:float option ->
  ?ri_ifnonematch:string list option ->
  ?ri_ifmatch:string list option ->
  ?ri_content_type:((string * string) * (string * string) list)
                   option ->
  ?ri_content_type_string:string option ->
  ?ri_content_length:int64 option ->
  ?ri_referer:string option Lazy.t ->
  ?ri_origin:string option Lazy.t ->
  ?ri_access_control_request_method:string option Lazy.t ->
  ?ri_access_control_request_headers:string list option Lazy.t ->
  ?ri_accept:Http_headers.accept Lazy.t ->
  ?ri_accept_charset:(string option * float option) list Lazy.t ->
  ?ri_accept_encoding:(string option * float option) list Lazy.t ->
  ?ri_accept_language:(string * float option) list Lazy.t ->
  ?ri_http_frame:Ocsigen_http_frame.t ->
  ?ri_request_cache:Polytables.t ->
  ?ri_client:Ocsigen_http_com.connection ->
  ?ri_range:((int64 * int64) list * int64 option * ifrange) option
            Lazy.t ->
  ?ri_timeofday:float ->
  ?ri_nb_tries:int ->
  ?ri_connection_closed:unit Lwt.t -> unit -> request_info

val update_nb_tries : request_info -> int -> unit

(** Accessor for range of request_info *)
val range : request_info -> ((int64 * int64) list * int64 option * ifrange) option Lazy.t

(** Accessor for url of request_info *)
val url_string : request_info -> string

(** Accessor for protocol of request_info *)
val protocol : request_info -> Ocsigen_http_frame.Http_header.proto

(** Accessor for http_frame of request_info *)
val http_frame : request_info -> Ocsigen_http_frame.t

(** Accessor for method of request_info *)
val meth : request_info -> Ocsigen_http_frame.Http_header.http_method

(** Accessor for ifmatch of request_info *)
val ifmatch : request_info -> string list option

(** Accessor for ifunmodifiedsince of request_info *)
val ifunmodifiedsince : request_info -> float option

(** Accessor for ifnonematch of request_info *)
val ifnonematch : request_info -> string list option

(** Accessor for ifmodifiedsince of request_info *)
val ifmodifiedsince : request_info -> float option

(** Accessor for remote_ip of request_info *)
val remote_ip : request_info -> string

(** Accessor for user_agent of request_info *)
val user_agent : request_info -> string

(** Accessor for host of request_info *)
val host : request_info -> string option

(** Accessor for ssl of request_info *)
val ssl : request_info -> bool

(** Accessor for port_from_host_field of request_info *)
val port_from_host_field : request_info -> int option

(** Accessor for server_port of request_info *)
val server_port : request_info -> int

(** Accessor for full_path of request_info *)
val full_path : request_info -> string list

(** Accessor for get_params_string of request_info *)
val get_params_string : request_info -> string option

(** Accessor for client of request_info *)
val client : request_info -> Ocsigen_http_com.connection

(** Accessor for nb_tries of request_info *)
val nb_tries : request_info -> int

(** Accessor for sub_path of request_info *)
val sub_path : request_info -> string list

(** Accessor for content_length of request_info *)
val content_length : request_info -> int64 option

(** Accessor for content_type_string of request_info *)
val content_type_string : request_info -> string option

(** Accessor for remote_port of request_info *)
val remote_port : request_info -> int

(** Accessor for sub_path_string of request_info *)
val sub_path_string : request_info -> string

(** Accessor for full_path_string of request_info *)
val full_path_string : request_info -> string

(** Accessor for remote_inet_addr of request_info *)
val remote_inet_addr : request_info -> Unix.inet_addr

(** Accessor for forward_ip of request_info *)
val forward_ip : request_info -> string list

(** Accessor for remote_ip_parsed of request_info *)
val remote_ip_parsed : request_info -> Ipaddr.t Lazy.t

(** Accessor for content_type of request_info *)
val content_type : request_info -> ((string * string) * (string * string) list) option

(** Accessor for origin of request_info *)
val origin : request_info -> string option Lazy.t

(** Accessor for access_control_request_method of request_info *)
val access_control_request_method : request_info -> string option Lazy.t

(** Accessor for access_control_request_headers of request_info *)
val access_control_request_headers : request_info -> string list option Lazy.t
