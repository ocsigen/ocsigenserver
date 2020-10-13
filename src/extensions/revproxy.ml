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

let section = Lwt_log.Section.make "ocsigen:ext:revproxy"

exception Bad_answer_from_http_server

(** The table of redirections for each virtual server *)
type redir = {
  regexp : Pcre.regexp ;
  full_url : Ocsigen_lib.yesnomaybe ;
  dest : string ;
  pipeline : bool ;
  keephost : bool
}

(** Generate the pages from the request *)
let gen dir = function
  | Ocsigen_extensions.Req_found _ ->
    Lwt.return Ocsigen_extensions.Ext_do_nothing
  | Ocsigen_extensions.Req_not_found
      (err, {Ocsigen_extensions.request_info; _}) ->
    Lwt.catch
      (* Is it a redirection? *)
      (fun () ->
         Lwt_log.ign_info ~section "Is it a redirection?";
         let dest =
           let fi full =
             Ocsigen_extensions.find_redirection
               dir.regexp
               full
               dir.dest
               request_info
           in
           match dir.full_url with
           | Ocsigen_lib.Yes -> fi true
           | Ocsigen_lib.No -> fi false
           | Ocsigen_lib.Maybe ->
             try fi false
             with Ocsigen_extensions.Not_concerned -> fi true
         in
         let (https, host, port, path) =
           try
             (* FIXME: we do not seem to handle GET
                parameters. Why? *)
             match Ocsigen_lib.Url.parse dest with
             | (Some https, Some host, port, path, _, _, _) ->
               let port = match port with
                 | None -> if https then 443 else 80
                 | Some p -> p
               in
               (https, host, port, path)
             | _ ->
               raise (Ocsigen_extensions.Error_in_config_file
                        ("Revproxy : error in destination URL "^dest))
           (*VVV catch only URL-related exceptions? *)
           with e ->
             raise (Ocsigen_extensions.Error_in_config_file
                      ("Revproxy : error in destination URL "^dest^" - "^
                       Printexc.to_string e))
         in

         Lwt_log.ign_info_f ~section
           "YES! Redirection to http%s://%s:%d/%s"
           (if https then "s" else "") host port path;

         Ocsigen_lib.Ip_address.get_inet_addr host >>= fun _inet_addr ->

         (* It is now safe to start processing next request.

            We are sure that the request won't be taken in disorder,
            so we return. *)

         let do_request () =
           let headers =
             let h =
               Cohttp.Request.headers
                 (Ocsigen_request.to_cohttp request_info)
             in
             let h =
               Ocsigen_request.version request_info
               |> Cohttp.Code.string_of_version
               |> Cohttp.Header.replace h
                    Ocsigen_header.Name.(to_string x_forwarded_proto)
             in
             let h =
               let forward =
                 let address =
                   Unix.string_of_inet_addr
                     (Ocsigen_request.address request_info)
                 in
                 String.concat ", "
                   (Ocsigen_request.remote_ip request_info
                    :: Ocsigen_request.forward_ip request_info
                    @  [address])
               in
               Cohttp.Header.replace h
                 Ocsigen_header.Name.(to_string x_forwarded_for)
                 forward
             in
             Cohttp.Header.remove h Ocsigen_header.Name.(to_string host)
           and uri =
             let scheme =
               if Ocsigen_request.ssl request_info then
                 "https"
               else
                 "http"
             and host =
               match
                 if dir.keephost then
                   Ocsigen_request.host request_info
                 else
                   None
               with
               | Some host -> host
               | None -> host
             in
             Uri.make ~scheme ~host ~port ~path ()
           and body = Ocsigen_request.body request_info
           and meth = Ocsigen_request.meth request_info in
           Cohttp_lwt_unix.Client.call ~headers ~body meth uri
         in
         Lwt.return @@
         Ocsigen_extensions.Ext_found
           (fun () -> do_request () >|= Ocsigen_response.of_cohttp))
      (function
        | Ocsigen_extensions.Not_concerned ->
          Lwt.return (Ocsigen_extensions.Ext_next err)
        | e -> Lwt.fail e)

let parse_config config_elem =
  let regexp = ref None in
  let full_url = ref Ocsigen_lib.Yes in
  let dest = ref None in
  let pipeline = ref true in
  let keephost = ref false in
  Ocsigen_extensions.(
    Configuration.process_element
      ~in_tag:"host"
      ~other_elements:(fun t _ _ -> raise (Bad_config_tag_for_extension t))
      ~elements:[
        Configuration.element
          ~name:"revproxy"
          ~attributes:[
            Configuration.attribute
              ~name:"regexp"
              (fun s ->
                 regexp := Some s;
                 full_url := Ocsigen_lib.Yes);
            Configuration.attribute
              ~name:"fullurl"
              (fun s ->
                 regexp := Some s;
                 full_url := Ocsigen_lib.Yes);
            Configuration.attribute
              ~name:"suburl"
              (fun s ->
                 regexp := Some s;
                 full_url := Ocsigen_lib.No);
            Configuration.attribute
              ~name:"dest"
              (fun s -> dest := Some s);
            Configuration.attribute
              ~name:"keephost"
              (function "keephost" -> keephost := true
                      | _ -> ());
            Configuration.attribute
              ~name:"nopipeline"
              (function "nopipeline" -> pipeline := false
                      | _ -> ());
          ]
          ()]
      config_elem
  );
  match !regexp, !full_url, !dest, !pipeline, !keephost with
  | (None, _, _, _, _) ->
    Ocsigen_extensions.badconfig
      "Missing attribute 'regexp' for <revproxy>"
  | (_, _, None, _, _) ->
    Ocsigen_extensions.badconfig
      "Missing attribute 'dest' for <revproxy>"
  | (Some regexp, full_url, Some dest, pipeline, keephost) ->
    gen {
      regexp = Ocsigen_lib.Netstring_pcre.regexp ("^" ^ regexp  ^ "$");
      full_url;
      dest;
      pipeline;
      keephost;
    }

let () =
  Ocsigen_extensions.register
    ~name:"revproxy"
    ~fun_site:(fun _ _ _ _ _ _ -> parse_config)
    ~respect_pipeline:true (* We ask ocsigen to respect pipeline order
                              when sending to extensions! *)
    ()
