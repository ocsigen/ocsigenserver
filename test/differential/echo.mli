(** The requests that echo servers send back.

    An echo server answers each request it accepts with a 200 response whose
    body is the request as it understood it, as a JSON object in the format of
    HTTP Garden (https://github.com/narfindustries/http-garden):
    [{"method": M, "uri": U, "version": V, "headers": [[N, V], ...],
    "body": B}], every string being base64-encoded. The echo server of Ocsigen
    Server is test/http-echo/echo.ml. *)

type request =
  { meth : string
  ; target : string  (** The request target, the "uri" of the JSON object. *)
  ; version : string  (** Without a leading ["HTTP/"], such as ["1.1"]. *)
  ; headers : (string * string) list  (** In the order the server gives. *)
  ; body : string
  }

val of_json : string -> (request, string) result
(** [of_json s] is the request described by the JSON object [s], or an error
    message saying why [s] is not such an object. *)

val to_string : request -> string
(** [to_string r] is a printable rendering of [r] on one line, with OCaml
    escapes for the bytes that are not printable. *)
