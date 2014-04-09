(* This module is temporary to avoid cyclical inclusion between
 * Ocsigen_extensions and Ocsigen_server *)

exception Ocsigen_Is_a_directory of (Ocsigen_request_info.request_info -> Neturl.url)
exception Ocsigen_unsupported_media
exception Ocsigen_upload_forbidden

val sockets : Lwt_unix.file_descr list ref
val sslsockets : Lwt_unix.file_descr list ref
val sslctx : Ssl.context ref

val stop : string -> int -> 'a

val get_number_of_connected : unit -> int

val shutdown_server : string -> string list -> unit

val listen :
  bool ->
  Ocsigen_socket.socket_type * int ->
  unit Lwt.t ->
  (Ocsigen_request_info.request_info -> unit -> Ocsigen_http_frame.result Lwt.t) ->
  Lwt_unix.file_descr list
