exception Ocsigen_unsupported_media
exception Ocsigen_http_error of
    Ocsigen_cookies.cookieset * Cohttp.Code.status

module Connection : sig
  exception Lost_connection of exn
  exception Aborted
  exception Timeout
  exception Keepalive_timeout
  exception Connection_closed
end

module Request : sig

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
    r_waiter : unit Lwt.t ;
    mutable r_tries : int
  }

  val make :
    ?forward_ip : string list ->
    address : Unix.inet_addr ->
    port : int ->
    filenames : string list ref ->
    sockaddr : Lwt_unix.sockaddr ->
    request : Cohttp.Request.t ->
    body : Cohttp_lwt_body.t ->
    waiter : unit Lwt.t ->
    unit ->
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

  val tries : t -> int

  val incr_tries : t -> unit

  val set_ssl : t -> bool -> t

end

module Answer : sig

  type t = {
    a_response : Cohttp.Response.t ;
    a_body     : Cohttp_lwt_body.t ;
    a_cookies  : Ocsigen_cookies.cookieset
  }

  val make :
    ?cookies : Ocsigen_cookies.cookieset ->
    ?body : Cohttp_lwt_body.t ->
    response : Cohttp.Response.t ->
    unit ->
    t

  val of_cohttp :
    ?cookies : Ocsigen_cookies.cookieset ->
    (Cohttp.Response.t * Cohttp_lwt_body.t) ->
    t

  val to_cohttp : t -> Cohttp.Response.t * Cohttp_lwt_body.t

  val status : t -> Cohttp.Code.status

  val set_status : t -> Cohttp.Code.status -> t

  val add_cookies : t -> Ocsigen_cookies.cookieset -> t

  val header : t -> Http_headers.name -> string option

  val header_multi : t -> Http_headers.name -> string list

  val add_header : t -> Http_headers.name -> string -> t

  val add_header_multi : t -> Http_headers.name -> string list -> t

  val replace_header : t -> Http_headers.name -> string -> t

  val replace_headers : t -> (Http_headers.name * string) list -> t

  val remove_header : t -> Http_headers.name -> t

end

(** compute a redirection if path links to a directory *)
exception Ocsigen_Is_a_directory of (Request.t -> Neturl.url)

(** accessor to get number of client (used by eliom monitoring) *)
val number_of_client : unit -> int

(** alias of [number_of_client] *)
val get_number_of_connected : unit -> int

(** shutdown main loop of server *)
val shutdown_server : float option -> unit

(** initialize a main loop of http server *)
val service :
  ?ssl:string * string * (bool -> string) option ->
  address:Ocsigen_socket.socket_type ->
  port:int ->
  connector:(Request.t -> Answer.t Lwt.t) ->
  unit ->
  unit Lwt.t
