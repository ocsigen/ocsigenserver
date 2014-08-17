open Lwt
open Ocsigen_lib
open Cohttp
open Cohttp_lwt_unix

let post_string ?v6 ?(https = false) ?port ?(headers = Http_headers.empty)
    ~host ~uri ~content ~content_type () =
  Ip_address.get_inet_addr ?v6 host >>= fun inet_addr ->
  let host = Unix.string_of_inet_addr inet_addr in
  let content_type = String.concat "/" [fst content_type; snd content_type] in
  let ( |> ) a f = f a in
  let headers =
    headers
    |> Http_headers.add Http_headers.content_type content_type
    |> Http_headers.add Http_headers.content_length
      (string_of_int (String.length content))
    |> To_cohttp.to_headers in
  let scheme = if https then "https" else "http" in
  let target = Uri.make ~scheme ~host ?port ~path:uri () in
  Cohttp_lwt_unix.Client.post
    ~body:(Cohttp_lwt_body.of_string content)
    ~headers
    target
  >|= Of_cohttp.of_response_and_body'

(* XXX: headers is unused at top of ocsigenserver,
 * but cast with cohttp headers is necessary *)

let get ?v6 ?(https = false) ?port ?headers ~host ~uri () =
  Ip_address.get_inet_addr ?v6 host >>= fun inet_addr ->
  let host = Unix.string_of_inet_addr inet_addr in
  let scheme = if https then "https" else "http" in
  let target = Uri.make ~scheme ~host ?port ~path:uri () in
  Cohttp_lwt_unix.Client.get target
  >|= Of_cohttp.of_response_and_body'

let post_urlencoded ?v6 ?https ?port ?headers ~host ~uri ~content () =
  post_string ?v6 ?https ?port ?headers
    ~host ~uri
    ~content:(Netencoding.Url.mk_url_encoded_parameters content)
    ~content_type:("application","x-www-form-urlencoded")
    ()
