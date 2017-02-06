type t

val make :
  ?forward_ip : string list ->
  ?sub_path : string ->
  address : Unix.inet_addr ->
  port : int ->
  filenames : string list ref ->
  sockaddr : Lwt_unix.sockaddr ->
  request : Cohttp.Request.t ->
  body : Cohttp_lwt_body.t ->
  waiter : unit Lwt.t ->
  unit ->
  t

val update :
  ?forward_ip : string list ->
  ?remote_ip : string ->
  ?ssl : bool ->
  ?request : Cohttp.Request.t ->
  t ->
  t

val update_url :
  ?full_rewrite : bool ->
  Uri.t ->
  t ->
  t

val request : t -> Cohttp.Request.t

val body : t -> Cohttp_lwt_body.t

val map_cohttp_request :
  f : (Cohttp.Request.t -> Cohttp.Request.t) ->
  t ->
  t

val address : t -> Unix.inet_addr

val host : t -> string option

val meth : t -> Cohttp.Code.meth

val port : t -> int

val ssl : t -> bool

val version : t -> Cohttp.Code.version

val query : t -> string option

val path : t -> string list

val path_string : t -> string

val sub_path : t -> string list

val sub_path_string : t -> string

val header : t -> Http_headers.name -> string option

val header_multi : t -> Http_headers.name -> string list

val remote_ip : t -> string

val remote_ip_parsed : t -> Ipaddr.t

val forward_ip : t -> string list

val tries : t -> int

val incr_tries : t -> unit
