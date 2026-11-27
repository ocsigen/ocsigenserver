(* An Ocsigen Server that answers every request with the request as it
   understood it, for differential tests of HTTP parsers (see
   ../differential/). The answer is a 200 whose body is a JSON object, in the
   format of the echo servers of HTTP Garden
   (https://github.com/narfindustries/http-garden):

   {"method": M, "uri": U, "version": V, "headers": [[N, V], ...], "body": B}

   where every string is base64-encoded, "uri" being the request target as
   received. The server answers some requests itself, before any extension
   sees them: it rejects invalid ones with its own error response, and
   answers OPTIONS * with an empty 200, which is not an echo. A response to
   HEAD has no body, hence no echo either.

   Usage: echo.exe PORT *)

open Lwt.Syntax

let base64 s =
  Cryptokit.transform_string (Cryptokit.Base64.encode_compact_pad ()) s

(* Base64 strings need no JSON escaping. *)
let json_string s = "\"" ^ base64 s ^ "\""

let json request body =
  let version =
    Cohttp.Code.string_of_version (Ocsigen.Request.version request)
  in
  let headers =
    Cohttp.Header.to_list (Ocsigen.Request.headers request)
    |> List.map (fun (name, value) ->
        Printf.sprintf "[%s,%s]" (json_string name) (json_string value)
    )
    |> String.concat ","
  in
  Printf.sprintf
    {|{"method":%s,"uri":%s,"version":%s,"headers":[%s],"body":%s}|}
    (json_string (Cohttp.Code.string_of_method (Ocsigen.Request.meth request)))
    (json_string (Ocsigen.Request.target request))
    (json_string version) headers (json_string body)

let echo _vh _config_info _path = function
  | Ocsigen.Extensions.Req_not_found (_, {Ocsigen.Extensions.request_info; _})
    ->
      Lwt.return
        (Ocsigen.Extensions.Ext_found
           (fun () ->
             let* body =
               Cohttp_lwt.Body.to_string (Ocsigen.Request.body request_info)
             in
             Lwt.return
               (Ocsigen.Response.respond_string
                  ~headers:
                    (Cohttp.Header.init_with "content-type" "application/json")
                  ~status:`OK ~body:(json request_info body) ()
               )
           )
        )
  | Ocsigen.Extensions.Req_found _ ->
      Lwt.return (Ocsigen.Extensions.Ext_next `Not_found)

let () =
  match Sys.argv with
  | [|_; port|] -> (
    match int_of_string_opt port with
    | Some port ->
        Ocsigen.Server.start
          ~ports:[`IPv4 Unix.inet_addr_loopback, port]
          ~logdir:"log" ~uploaddir:None ~usedefaulthostname:true
          ~command_pipe:"local.cmd"
          [Ocsigen.Server.host [echo]]
    | None ->
        prerr_endline "echo.exe: PORT must be a number";
        exit 2
  )
  | _ ->
      prerr_endline "usage: echo.exe PORT";
      exit 2
