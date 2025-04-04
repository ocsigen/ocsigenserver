val section : Logs.src
(** use Lwt_log.Section.set_level in order to debug *)

exception Ocsigen_http_error of Ocsigen_cookie_map.t * Cohttp.Code.status

exception
  Ext_http_error of Cohttp.Code.status * string option * Cohttp.Header.t option
(** Exception raised by exceptions to describe an HTTP error. It is
    possible to pass the code of the error, an optional comment, and
    optionally some headers. *)

exception Ocsigen_is_dir of (Ocsigen_request.t -> Uri.t)
(** compute a redirection if path links to a directory *)

val get_number_of_connected : unit -> int
(** accessor to get number of client (used by eliom monitoring) *)

val shutdown : float option -> unit
(** Shutdown main loop of server *)

val service :
   ?ssl:string * string * (bool -> string) option
  -> address:Ocsigen_config.socket_type
  -> port:int
  -> connector:(Ocsigen_request.t -> Ocsigen_response.t Lwt.t)
  -> unit
  -> unit Lwt.t
(** initialize a main loop of http server *)
