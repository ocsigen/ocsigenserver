module type S = sig

  (** compute a redirection if path links to a directory *)
  exception Ocsigen_Is_a_directory of (Ocsigen_request_info.request_info -> Neturl.url)

  exception Ocsigen_unsupported_media
  exception Ocsigen_http_error of (Ocsigen_cookies.cookieset * int)

  module Connection : sig
    exception Lost_connection of exn
    exception Aborted
    exception Timeout
    exception Keepalive_timeout
    exception Connection_closed
  end

  (** accessor to get number of client (used by eliom monitoring) *)
  val number_of_client : unit -> int

  (** alias of [number_of_client] *)
  val get_number_of_connected : unit -> int

  (** shutdown main loop of server *)
  val shutdown_server : float option -> unit

  (** initialize a main loop of http server *)
  val service :
    ?ssl:string * string * (bool -> string) option ->
    address:string ->
    port:int ->
    connector:(Ocsigen_request_info.request_info -> unit -> Ocsigen_http_frame.result Lwt.t) ->
    unit -> unit Lwt.t
end
