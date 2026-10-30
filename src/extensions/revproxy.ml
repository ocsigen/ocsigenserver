(* Ocsigen
 * http://www.ocsigen.org
 * Module revproxy.ml
 * Copyright (C) 2007 Vincent Balat
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(** Reverse proxy for Ocsigen

    The reverse proxy is still experimental. *)

open Lwt.Infix
module Pcre = Re.Pcre

let section = Logs.Src.create "ocsigen:ext:revproxy"

type redirection =
  { regexp : Pcre.regexp
  ; full_url : bool
  ; dest : string
  ; pipeline : bool
  ; keephost : bool }
(** The table of redirections for each virtual server *)

let create_redirection
      ?(full_url = true)
      ?(pipeline = true)
      ?(keephost = false)
      ~regexp
      dest
  =
  let regexp = Pcre.regexp ("^" ^ regexp ^ "$") in
  {regexp; dest; full_url; pipeline; keephost}

(* The address of the client, as X-Forwarded-For holds it: an IP address, or
   "unknown" for a client without one (Unix socket). *)
let client_address request_info =
  match Ocsigen.Request.client_conn request_info with
  | `Inet (ip, _) -> Ipaddr.to_string ip
  | `Forwarded_for ip -> ip
  | `Unix _ | `Unknown -> "unknown"

(* [headers] without those meant for a single connection, which a proxy does
   not forward (RFC 9110, section 7.6.1): Connection, the headers it names, and
   those always meant for a single connection. Content-Length,
   Transfer-Encoding and Host are kept, even when Connection names them: the
   body is forwarded framed according to the first two, and keephost forwards
   Host. *)
let remove_hop_by_hop headers =
  Ocsigen_http.Header.connection_options headers
  @ ["connection"; "keep-alive"; "proxy-connection"; "te"; "upgrade"]
  |> List.filter (fun name ->
    not (List.mem name ["content-length"; "transfer-encoding"; "host"]))
  |> List.fold_left Cohttp.Header.remove headers

(* Whether the body of [response], to a request with [meth], is delimited by
   the end of the connection to the target, having neither a length nor
   chunks. *)
let delimited_by_close meth response =
  match Cohttp.Response.encoding response, meth with
  | Cohttp.Transfer.Unknown, `HEAD -> false
  | Cohttp.Transfer.Unknown, _ -> (
    match Cohttp.Code.code_of_status (Cohttp.Response.status response) with
    | 204 | 304 -> false
    | code -> code >= 200)
  | (Cohttp.Transfer.Chunked | Cohttp.Transfer.Fixed _), _ -> false

(* The response of the target, to a request with [meth], as the server sends
   it to the client: in its own version of HTTP, without the headers meant for
   the connection to the target. A body delimited by the end of that
   connection is delimited by the end of the connection to the client. *)
let forwarded_response meth response =
  let headers = remove_hop_by_hop (Cohttp.Response.headers response) in
  let headers =
    if delimited_by_close meth response
    then Cohttp.Header.add headers "connection" "close"
    else headers
  in
  {response with Cohttp.Response.headers; version = `HTTP_1_1}

(* [tracked body] is [body], read in the same way, with a function that gives
   the exception raised while reading it, if any. *)
let tracked body =
  let failure = ref None in
  let body =
    match body with
    | `Stream chunks ->
        `Stream
          (Lwt_stream.from (fun () ->
             Lwt.catch
               (fun () -> Lwt_stream.get chunks)
               (fun exn ->
                  failure := Some exn;
                  Lwt.reraise exn)))
    | (`Empty | `String _ | `Strings _) as body -> body
  in
  body, fun () -> !failure

(* [call ~client_closed ~headers ~body meth uri] sends the request to the
   target on a connection of its own, closed once the body of the response is
   read, the call fails, or [client_closed] resolves: a client that leaves
   before the end of the response does not read it to the end. This is what
   [Cohttp_lwt_unix.Client.call] does, except that cohttp-lwt 6 raises again,
   outside of any promise, any failure of the call: an error while reading
   the body of the request (one too large, for instance) or writing it to the
   target stopped the server. *)
let call ~client_closed ~headers ~body meth uri =
  let module Net = Cohttp_lwt_unix.Net in
  let module Connection = Cohttp_lwt_unix.Connection in
  Net.resolve ~ctx:(Lazy.force Net.default_ctx) uri
  (* Persistent, as cohttp does, so that a chunked body is sent chunked. *)
  >>= Connection.connect ~persistent:true
  >>= fun connection ->
  Lwt.catch
    (fun () -> Connection.call connection ~headers ~body meth uri)
    (fun exn ->
       Connection.close connection;
       Lwt.reraise exn)
  >|= fun ((_, response_body) as response) ->
  (match response_body with
  | `Stream stream ->
      (* [Lwt.choose] removes its callback from the promise that does not
         resolve, so that they do not pile up on [client_closed] during a
         long-lived connection of the client. *)
      Lwt.on_termination
        (Lwt.choose [Lwt_stream.closed stream; client_closed])
        (fun () -> Connection.close connection)
  | `Empty | `String _ | `Strings _ -> Connection.close connection);
  response

(** Generate the pages from the request *)
let gen dir = function
  | Ocsigen.Extensions.Req_found _ ->
      Lwt.return Ocsigen.Extensions.Ext_do_nothing
  | Ocsigen.Extensions.Req_not_found (err, {Ocsigen.Extensions.request_info; _})
    ->
      Lwt.catch
        (* Is it a redirection? *)
        (fun () ->
           Logs.info ~src:section (fun fmt -> fmt "Is it a redirection?");
           let dest =
             Ocsigen.Extensions.find_redirection dir.regexp dir.full_url
               dir.dest request_info
           in
           (* [dest] is a full URL. Its path is sent as it is, but Uri decodes
              and encodes again its query, whose encoding may change: "+"
              becomes "%20", which also means a space in a form, and ";"
              becomes "%3B". *)
           let target = Uri.of_string dest in
           (match Uri.scheme target, Uri.userinfo target, Uri.host target with
           | Some ("http" | "https"), None, Some host when host <> "" -> ()
           | _ ->
               raise
                 (Ocsigen.Extensions.Error_in_config_file
                    ("Revproxy : error in destination URL " ^ dest)));
           Logs.info ~src:section (fun fmt ->
             (* Not the query, which may hold secrets. *)
             fmt "YES! Redirection to %a" Uri.pp (Uri.with_query target []));
           (* It is now safe to start processing next request.

            We are sure that the request won't be taken in disorder,
            so we return. *)
           let do_request () =
             let headers =
               (* Removed first, so that Connection cannot name the headers
                  set below. *)
               let h =
                 remove_hop_by_hop
                   (Cohttp.Request.headers
                      (Ocsigen.Request.to_cohttp request_info))
               in
               let h =
                 Cohttp.Header.replace h
                   Ocsigen_http.Header.Name.(to_string x_forwarded_proto)
                   (if Ocsigen.Request.ssl request_info then "https" else "http")
               in
               let h =
                 (* The client alone, without the proxies before this
                    server, so that the target needs to trust only this
                    server. *)
                 Cohttp.Header.replace h
                   Ocsigen_http.Header.Name.(to_string x_forwarded_for)
                   (client_address request_info)
               in
               (* Without keephost, cohttp sets the Host header from [target]. *)
               if dir.keephost
               then h
               else
                 Cohttp.Header.remove h
                   Ocsigen_http.Header.Name.(to_string host)
             and body, body_failure =
               tracked (Ocsigen.Request.body request_info)
             and meth = Ocsigen.Request.meth request_info in
             (* An error while reading the body of the request comes from the
                client (body too large, cut short, too slow), and the server
                answers it as such. Whatever else goes wrong with the target
                (no route, refused connection, unknown name, TLS failure,
                connection closed before a response...), the client gets a
                502, as from other reverse proxies. The exceptions vary with
                the step that fails and the library that does it. *)
             Lwt.catch
               (fun () ->
                  call
                    ~client_closed:
                      (Ocsigen.Request.connection_closed request_info)
                    ~headers ~body meth target)
               (fun exn ->
                  match body_failure (), exn with
                  | Some exn, _ | None, (Lwt.Canceled as exn) -> Lwt.reraise exn
                  | None, exn ->
                      (* Not the path and query, which may carry data that
                         do not belong in the logs. *)
                      let server =
                        Uri.make ?scheme:(Uri.scheme target)
                          ?host:(Uri.host target) ?port:(Uri.port target) ()
                      in
                      Logs.err ~src:section (fun fmt ->
                        fmt "Cannot get a response from %s: %s"
                          (Uri.to_string server) (Printexc.to_string exn));
                      Lwt.fail
                        (Ocsigen.Extensions.Ocsigen_http_error
                           (Ocsigen_cookie_map.empty, `Bad_gateway)))
             >|= fun (response, body) -> forwarded_response meth response, body
           in
           Lwt.return
           @@ Ocsigen.Extensions.Ext_found
                (fun () -> do_request () >|= Ocsigen.Response.of_cohttp))
        (function
          | Ocsigen.Extensions.Not_concerned ->
              Lwt.return (Ocsigen.Extensions.Ext_next err)
          | e -> Lwt.fail e)

let parse_config config_elem =
  let regexp = ref None in
  let full_url = ref true in
  let dest = ref None in
  let pipeline = ref true in
  let keephost = ref false in
  Ocsigen.Extensions.(
    Configuration.process_element ~in_tag:"host"
      ~other_elements:(fun t _ _ -> raise (Bad_config_tag_for_extension t))
      ~elements:
        [ Configuration.element ~name:"revproxy"
            ~attributes:
              [ Configuration.attribute ~name:"fullurl" (fun s ->
                  regexp := Some s;
                  full_url := true)
              ; Configuration.attribute ~name:"suburl" (fun s ->
                  regexp := Some s;
                  full_url := false)
              ; Configuration.attribute ~name:"dest" (fun s -> dest := Some s)
              ; Configuration.attribute ~name:"keephost" (function
                  | "keephost" -> keephost := true
                  | _ -> ())
              ; Configuration.attribute ~name:"nopipeline" (function
                  | "nopipeline" -> pipeline := false
                  | _ -> ()) ]
            () ]
      config_elem);
  match !regexp, !full_url, !dest, !pipeline, !keephost with
  | None, _, _, _, _ ->
      Ocsigen.Extensions.badconfig "Missing attribute 'regexp' for <revproxy>"
  | _, _, None, _, _ ->
      Ocsigen.Extensions.badconfig "Missing attribute 'dest' for <revproxy>"
  | Some regexp, full_url, Some dest, pipeline, keephost ->
      gen
        { regexp = Ocsigen_base.Lib.Netstring_pcre.regexp ("^" ^ regexp ^ "$")
        ; full_url
        ; dest
        ; pipeline
        ; keephost }

let () =
  Ocsigen.Extensions.register ~name:"revproxy"
    ~fun_site:(fun userconf _ _ _ _ _ -> function
       (* From a user configuration file, it would reach the services that
         only the server can reach. *)
       | Xml.Element ("revproxy", _, _) when Option.is_some userconf ->
           raise
             (Ocsigen.Extensions.Error_in_user_config_file
                "<revproxy> is not allowed in user configuration files")
       | element -> parse_config element)
    ~respect_pipeline:true
    (* We ask ocsigen to respect pipeline order
                              when sending to extensions! *)
    ()

let run ~redirection () _ _ _ = gen redirection

(* Publish a "forward everything to TARGET" function so that the one-command
   reverse-proxy mode can use it after loading this extension dynamically,
   without a static dependency. *)
let () =
  Ocsigen.Server.register_reverse_proxy_server (fun ~target ->
    let target = Ocsigen_base.Lib.Url.remove_end_slash target in
    run
      ~redirection:
        (create_redirection ~full_url:false ~regexp:"(.*)" (target ^ "/\\1"))
      ())
