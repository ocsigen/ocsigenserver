(** Requests to other HTTP servers, from the server or its applications. *)

val call :
     ?ctx:Cohttp_lwt_unix.Net.ctx
  -> ?interrupted:unit Lwt.t
  -> ?headers:Cohttp.Header.t
  -> ?body:Cohttp_lwt.Body.t
  -> ?absolute_form:bool
  -> Cohttp.Code.meth
  -> Uri.t
  -> (Cohttp.Response.t * Cohttp_lwt.Body.t * (unit -> unit Lwt.t)) Lwt.t
(** [call ?ctx ?interrupted ?headers ?body ?absolute_form meth uri] sends a
    request to [uri] on a connection of its own, as
    [Cohttp_lwt_unix.Client.call] does, with [ctx] ([Net.default_ctx] by
    default). The connection is closed when the call fails or, once the
    response has arrived, when its body is read or [interrupted] resolves. The
    result also holds a function that closes the connection at once, and ends
    the body of the response with what was already received, for a body that
    will not be read to its end.

    Unlike [Cohttp_lwt_unix.Client.call] in cohttp-lwt 6, a failure of the call
    only rejects the promise: cohttp raises it again outside of any promise,
    which stops the program
    ({{:https://github.com/mirage/ocaml-cohttp/issues/1153}cohttp#1153}). Use
    this function rather than [Cohttp_lwt_unix.Client.call] to give a [ctx]:
    with one, cohttp does not use the function set by {!install}. *)

val install : unit -> unit
(** [install ()] makes the functions of [Cohttp_lwt_unix.Client] ([call],
    [get], [post]...), when they are called without a [ctx] argument, send
    their requests with {!call}, so that a failed request does not stop the
    program. {!Server} does it when it is loaded, before the modules of the
    applications: one that sets its own function with
    [Cohttp_lwt_unix.Client.set_cache] replaces it. *)
