exception Ocsigen_upload_forbidden

(** Create a Ocsigen_request_info.request_info data with informations given by
    Cohttp_lwt.Server

    @param address address of server
    @param port port of server
    @param receiver connection of client (but it's obsolete !)
    @param files list that will contain the files of request
    @param sockaddr endpoit of client (ip:port)
    @param request cohttp request
    @param body body of request
*)
val of_cohttp_request :
  address:Unix.inet_addr ->
  port:int ->
  string list ref ->
  Lwt_unix.sockaddr ->
  Cohttp.Request.t ->
  Cohttp_lwt_body.t ->
  unit Lwt.t -> Ocsigen_request_info.request_info Lwt.t

(** Create a Cohttp Request (for Cohttp_lwt.Client) according to
    Ocsigen_request_info.request_info

    @param request information of request to send
*)
val to_cohttp_request :
  Ocsigen_request_info.request_info ->
  (Cohttp.Code.meth * Cohttp.Code.version * Cohttp.Header.t * Uri.t * Cohttp_lwt_body.t)
