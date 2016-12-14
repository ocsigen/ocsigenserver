exception Ocsigen_unsupported_media
exception Ocsigen_http_error of (Ocsigen_cookies.cookieset * int)

module Connection : sig
  exception Lost_connection of exn
  exception Aborted
  exception Timeout
  exception Keepalive_timeout
  exception Connection_closed
end

type request = {
  r_address       : Unix.inet_addr ;
  r_port          : int ;
  r_filenames     : string list ref;
  r_sockaddr      : Lwt_unix.sockaddr ;
  r_request       : Cohttp.Request.t ;
  r_body          : Cohttp_lwt_body.t ;
  r_waiter        : unit Lwt.t ;
  mutable r_tries : int
}

type result = {
  r_response : Cohttp.Response.t ;
  r_body     : Cohttp_lwt_body.t ;
  r_cookies  : Ocsigen_cookies.cookieset
}

val result_of_cohttp :
  ?cookies : Ocsigen_cookies.cookieset ->
  (Cohttp.Response.t * Cohttp_lwt_body.t) ->
  result

val incr_tries : request -> unit

val tries : request -> int

val path_of_request : request -> string list

(** compute a redirection if path links to a directory *)
exception Ocsigen_Is_a_directory of (request -> Neturl.url)

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
  connector:(request -> result Lwt.t) ->
  unit -> unit Lwt.t
