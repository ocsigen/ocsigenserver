exception Ocsigen_upload_forbidden

(** Create a Ocsigen_request_info.request_info data with informations given by
 * Cohttp_lwt.Server
 *
 * @param address address of server
 * @param port port of server
 * @param receiver connection of client (but it's obsolete !)
 * @param files list that will contain the files of request
 * @param sockaddr endpoit of client (ip:port)
 * @param conn_id id of connection (according to cohttp)
 * @param request cohttp request
 * @param body body of request *)
val of_cohttp_request :
  address:string ->
  port:int ->
  ?receiver:Ocsigen_http_com.connection ->
  string list ref ->
  Lwt_unix.sockaddr ->
  Cohttp.Connection.t ->
  Cohttp.Request.t ->
  Cohttp_lwt_body.t -> Ocsigen_request_info.request_info Lwt.t

(** Create a Cohttp Request (for Cohttp_lwt.Client) according to
 * Ocsigen_request_info.request_info
 *
 * @param request information of request to send *)
val to_cohttp_request :
  Ocsigen_request_info.request_info ->
  (Cohttp.Code.meth * Cohttp.Code.version * Cohttp.Header.t * Uri.t * Cohttp_lwt_body.t)

(** Create a Ocsigen_request_info.request_info data with informations given by
 * old server support of ocsigen
 *
 * @param meth method of request
 * @prama proto version of request
 * @param url url of request
 * @param frame HTTP header of request
 * @param files list that will contain the files of request
 * @param sockaddr endpoint of client (ip:port)
 * @param port port of server
 * @param receiver connection of client (in according with handle of pipeline)
 * @param slot channel of body of request *)
val of_ocsigen_request :
  Ocsigen_http_frame.Http_header.http_method ->
  Ocsigen_http_frame.Http_header.proto ->
  Ocsigen_lib.Url.t ->
  Ocsigen_http_frame.t ->
  string list ref ->
  Unix.sockaddr ->
  int ->
  Ocsigen_http_com.connection ->
  Ocsigen_http_com.slot -> Ocsigen_request_info.request_info Lwt.t
