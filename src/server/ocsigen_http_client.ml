open Lwt
open Ocsigen_lib
open Cohttp
open Cohttp_lwt_unix

let post_string ?v6 ?https ?port ?(headers = Http_headers.empty)
    ~host ~uri ~content ~content_type () =
  let content_type = String.concat "/" [fst content_type; snd content_type] in
  let ( |> ) a f = f a in
  let headers =
    headers
    |> Http_headers.add Http_headers.content_type content_type
    |> Http_headers.add Http_headers.content_length
      (string_of_int (String.length content))
    |> To_cohttp.to_headers in
  Cohttp_lwt_unix.Client.post
    ~body:(Cohttp_lwt_body.of_string content)
    ~headers
    (Uri.make ~host ?port ~path:uri ())
  >|= Of_cohttp.of_response_and_body'

(* XXX: headers is unused at top of ocsigenserver,
 * but cast with cohttp headers is necessary *)

let get ?v6 ?https ?port ?headers ~host ~uri () =
  Cohttp_lwt_unix.Client.get (Uri.make ~host ?port ~path:uri ())
  >|= Of_cohttp.of_response_and_body'

let post_urlencoded ?v6 ?https ?port ?headers ~host ~uri ~content () =
  post_string ?v6 ?https ?port ?headers
    ~host ~uri
    ~content:(Netencoding.Url.mk_url_encoded_parameters content)
    ~content_type:("application","x-www-form-urlencoded")
    ()
