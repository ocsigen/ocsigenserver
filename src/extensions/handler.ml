open Lwt.Infix

let name = "handler"

type id = string list * (string * string list) list

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

let handlers : t list ref = ref []

let register f = handlers := f :: !handlers

let rec fold_handlers ~request l =
  match l with
  | f :: l ->
    (f (Ocsigen_request.path request)
       (Ocsigen_request.get_params request)
       (Cohttp.Request.headers (Ocsigen_request.request request))
       (Ocsigen_request.body request) >>= function
     | `Response r ->
       Lwt.return (Ocsigen_extensions.Ext_found (fun () -> Lwt.return r))
     | `Fail s ->
       Lwt.return (Ocsigen_extensions.Ext_next s)
     | `Continue ->
       fold_handlers ~request l)
  | [] ->
    Lwt.return (Ocsigen_extensions.Ext_next `Not_found)

let fun_site _ _ _ _ _ _ = function
  | Ocsigen_extensions.Req_found _ ->
    Lwt.return Ocsigen_extensions.Ext_do_nothing
  | Ocsigen_extensions.Req_not_found
      (_, {Ocsigen_extensions.request_info}) ->
    fold_handlers ~request:request_info !handlers

let () = Ocsigen_extensions.register_extension ~name ~fun_site ()
