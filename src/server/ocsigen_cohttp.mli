exception Ocsigen_http_error of
    Ocsigen_cookies.cookieset * Cohttp.Code.status

(** Exception raised by exceptions to describe an HTTP error. It is
    possible to pass the code of the error, an optional comment, and
    optionally some headers. *)
exception Ext_http_error of
    Cohttp.Code.status * string option * Cohttp.Header.t option

(** compute a redirection if path links to a directory *)
exception Ocsigen_Is_a_directory of (Ocsigen_request.t -> Neturl.url)

(** accessor to get number of client (used by eliom monitoring) *)
val number_of_client : unit -> int

(** alias of [number_of_client] *)
val get_number_of_connected : unit -> int

(** Shutdown main loop of server *)
val shutdown : float option -> unit

(** initialize a main loop of http server *)
val service :
  ?ssl:string * string * (bool -> string) option ->
  address:Ocsigen_socket.socket_type ->
  port:int ->
  connector:(Ocsigen_request.t -> Ocsigen_response.t Lwt.t) ->
  unit ->
  unit Lwt.t