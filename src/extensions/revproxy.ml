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
           let host, port =
             match Uri.scheme target, Uri.userinfo target, Uri.host target with
             | Some (("http" | "https") as scheme), None, Some host
               when host <> "" ->
                 ( host
                 , Option.value (Uri.port target)
                     ~default:(if scheme = "https" then 443 else 80) )
             | _ ->
                 raise
                   (Ocsigen.Extensions.Error_in_config_file
                      ("Revproxy : error in destination URL " ^ dest))
           in
           Logs.info ~src:section (fun fmt ->
             (* Not the query, which may hold secrets. *)
             fmt "YES! Redirection to %a" Uri.pp (Uri.with_query target []));
           Ocsigen_base.Lib.Ip_address.get_inet_addr host >>= fun _inet_addr ->
           (* It is now safe to start processing next request.

            We are sure that the request won't be taken in disorder,
            so we return. *)
           let do_request () =
             let headers =
               let h =
                 Cohttp.Request.headers (Ocsigen.Request.to_cohttp request_info)
               in
               let h =
                 Ocsigen.Request.version request_info
                 |> Cohttp.Code.string_of_version
                 |> Cohttp.Header.replace h
                      Ocsigen_http.Header.Name.(to_string x_forwarded_proto)
               in
               let h =
                 let forward =
                   let address =
                     Ocsigen.Config.Socket_type.to_string
                       (Ocsigen.Request.address request_info)
                   in
                   String.concat ", "
                     (Ocsigen.Request.client_conn_to_string request_info
                      :: Ocsigen.Request.forward_ip request_info
                     @ [address])
                 in
                 Cohttp.Header.replace h
                   Ocsigen_http.Header.Name.(to_string x_forwarded_for)
                   forward
               in
               Cohttp.Header.remove h Ocsigen_http.Header.Name.(to_string host)
             and uri =
               let host =
                 match
                   if dir.keephost
                   then Ocsigen.Request.host request_info
                   else None
                 with
                 | Some host -> host
                 | None -> host
               in
               let uri = Uri.with_host target (Some host) in
               Uri.with_port uri (Some port)
             and body = Ocsigen.Request.body request_info
             and meth = Ocsigen.Request.meth request_info in
             Cohttp_lwt_unix.Client.call ~headers ~body meth uri
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
    ~fun_site:(fun _ _ _ _ _ _ -> parse_config)
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
