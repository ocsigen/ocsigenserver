module type S = sig
  exception Ocsigen_Is_a_directory of (Ocsigen_request_info.request_info -> Neturl.url)
  exception Ocsigen_unsupported_media

  val ssl_context : Ssl.context ref

  val stop : string -> int -> unit
  val number_of_client : unit -> int

  val service :
    address:string ->
    port:int ->
    connector:(Ocsigen_request_info.request_info -> unit -> Ocsigen_http_frame.result Lwt.t) ->
    unit -> unit Lwt.t
end
