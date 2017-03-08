type result = [
  | `Response of Ocsigen_response.t
  | `Fail     of Cohttp.Code.status
  | `Continue
]

type t =
  string list ->
  (string * string list) list ->
  Cohttp.Header.t ->
  Cohttp_lwt_body.t ->
  result Lwt.t

val register : t -> unit
