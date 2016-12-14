type t

type content_type = (string * string) * (string * string) list

type file_info = Ocsigen_multipart.file_info = {
  tmp_filename : string ;
  filesize : int64 ;
  raw_original_filename : string ;
  file_content_type : content_type option
}

type post_data = (string * string) list * (string * file_info) list

val make :
  ?forward_ip : string list ->
  ?sub_path : string ->
  ?original_full_path : string ->
  ?request_cache : Polytables.t ->
  ?cookies_override : string Ocsigen_cookie_map.Map_inner.t ->
  address : Unix.inet_addr ->
  port : int ->
  ssl : bool ->
  filenames : string list ref ->
  sockaddr : Lwt_unix.sockaddr ->
  body : Cohttp_lwt.Body.t ->
  connection_closed : unit Lwt.t ->
  Cohttp.Request.t ->
  t

val update :
  ?ssl : bool ->
  ?forward_ip : string list ->
  ?remote_ip : string ->
  ?sub_path : string ->
  ?meth : Cohttp.Code.meth ->
  ?get_params_flat : (string * string) list ->
  ?post_data : post_data option ->
  ?cookies_override : string Ocsigen_cookie_map.Map_inner.t ->
  ?full_rewrite : bool ->
  ?uri : Uri.t ->
  t ->
  t

val to_cohttp : t -> Cohttp.Request.t

val uri : t -> Uri.t

val body : t -> Cohttp_lwt.Body.t

val address : t -> Unix.inet_addr

val host : t -> string option

val meth : t -> Cohttp.Code.meth

val port : t -> int

val ssl : t -> bool

val version : t -> Cohttp.Code.version

val query : t -> string option

val get_params : t -> (string * string list) list

val get_params_flat : t -> (string * string) list

val path : t -> string list

val path_string : t -> string

val sub_path : t -> string list

val sub_path_string : t -> string

val original_full_path : t -> string list

val original_full_path_string : t -> string

val header : t -> Ocsigen_header.Name.t -> string option

val header_multi : t -> Ocsigen_header.Name.t -> string list

val add_header : t -> Ocsigen_header.Name.t -> string -> t

val cookies : t -> string Ocsigen_cookie_map.Map_inner.t

val files :
  t ->
  string option ->
  Int64.t option ->
  (string * file_info) list Lwt.t option

val post_params :
  t ->
  string option ->
  Int64.t option ->
  (string * string) list Lwt.t option

val remote_ip : t -> string

val remote_ip_parsed : t -> Ipaddr.t

val forward_ip : t -> string list

val content_type : t -> content_type option

val request_cache : t -> Polytables.t

val tries : t -> int

val incr_tries : t -> unit

val connection_closed : t -> unit Lwt.t

val timeofday : t -> float
