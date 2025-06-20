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

open Eio.Std
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
  | Ocsigen_extensions.Req_found _ -> Ocsigen_extensions.Ext_do_nothing
  | Ocsigen_extensions.Req_not_found (err, {Ocsigen_extensions.request_info; _})
    -> (
    try
      (* Is it a redirection? *)
      Logs.info ~src:section (fun fmt -> fmt "Is it a redirection?");
      let dest =
        Ocsigen_extensions.find_redirection dir.regexp dir.full_url dir.dest
          request_info
      in
      let https, host, port, path =
        try
          (* FIXME: we do not seem to handle GET
                parameters. Why? *)
          match Ocsigen_lib.Url.parse dest with
          | Some https, Some host, port, path, _, _, _ ->
              let port =
                match port with
                | None -> if https then 443 else 80
                | Some p -> p
              in
              https, host, port, path
          | _ ->
              raise
                (Ocsigen_extensions.Error_in_config_file
                   ("Revproxy : error in destination URL " ^ dest))
          (*VVV catch only URL-related exceptions? *)
        with e ->
          raise
            (Ocsigen_extensions.Error_in_config_file
               ("Revproxy : error in destination URL " ^ dest ^ " - "
              ^ Printexc.to_string e))
      in
      Logs.info ~src:section (fun fmt ->
        fmt "YES! Redirection to http%s://%s:%d/%s"
          (if https then "s" else "")
          host port path);
      let _inet_addr = Ocsigen_lib.Ip_address.get_inet_addr host in
      (* It is now safe to start processing next request.

            We are sure that the request won't be taken in disorder,
            so we return. *)
      let do_request () =
        let headers =
          let h =
            Cohttp.Request.headers (Ocsigen_request.to_cohttp request_info)
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
                Ocsigen_config.Socket_type.to_string
                  (Ocsigen_request.address request_info)
              in
              String.concat ", "
                (Ocsigen_request.remote_ip request_info
                 :: Ocsigen_request.forward_ip request_info
                @ [address])
            in
            Cohttp.Header.replace h
              Ocsigen_header.Name.(to_string x_forwarded_for)
              forward
          in
          Cohttp.Header.remove h Ocsigen_header.Name.(to_string host)
        and uri =
          let scheme =
            if Ocsigen_request.ssl request_info then "https" else "http"
          and host =
            match
              if dir.keephost then Ocsigen_request.host request_info else None
            with
            | Some host -> host
            | None -> host
          in
          Uri.make ~scheme ~host ~port ~path ()
        and body = Ocsigen_request.body request_info
        and meth = Ocsigen_request.meth request_info in
        let sw = Stdlib.Option.get (Fiber.get Ocsigen_lib.current_switch) in
        let env = Stdlib.Option.get (Fiber.get Ocsigen_lib.env) in
        let client =
          (* TODO: Https not supported out of the box in [Cohttp_eio]. *)
          Cohttp_eio.Client.make ~https:None env#net
        in
        Cohttp_eio.Client.call client ~sw ~headers ~body meth uri
      in
      Ocsigen_extensions.Ext_found
        (fun () -> Ocsigen_response.of_cohttp (do_request ()))
    with
    | Ocsigen_extensions.Not_concerned -> Ocsigen_extensions.Ext_next err
    | e -> raise e)

let parse_config config_elem =
  let regexp = ref None in
  let full_url = ref true in
  let dest = ref None in
  let pipeline = ref true in
  let keephost = ref false in
  Ocsigen_extensions.(
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
      Ocsigen_extensions.badconfig "Missing attribute 'regexp' for <revproxy>"
  | _, _, None, _, _ ->
      Ocsigen_extensions.badconfig "Missing attribute 'dest' for <revproxy>"
  | Some regexp, full_url, Some dest, pipeline, keephost ->
      gen
        { regexp = Ocsigen_lib.Netstring_pcre.regexp ("^" ^ regexp ^ "$")
        ; full_url
        ; dest
        ; pipeline
        ; keephost }

let () =
  Ocsigen_extensions.register ~name:"revproxy"
    ~fun_site:(fun _ _ _ _ _ _ -> parse_config)
    ~respect_pipeline:true
    (* We ask ocsigen to respect pipeline order
                              when sending to extensions! *)
    ()

let run ~redirection () _ _ _ = gen redirection
