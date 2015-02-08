(* Ocsigen
 * http://www.ocsigen.org
 * Module accesscontrol.ml
 * Copyright (C) 2007 Vincent Balat, Stéphane Glondu
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

(** Filtering requests in the configuration file *)

(*

Then load it dynamically from Ocsigen's config file:
   <extension module=".../accesscontrol.cmo"/>

*)

open Ocsigen_lib

open Printf
open Lwt
open Ocsigen_extensions
open Simplexmlparser
open Ocsigen_http_frame


let section = Lwt_log.Section.make "ocsigen:ext:access-control"

(*****************************************************************************)
(* Parsing a condition *)

let rec parse_condition = function

    | Element ("ip", ["value", s], []) ->
        let prefix =
          try
            Ipaddr.Prefix.of_string_exn s
          with Ipaddr.Parse_error _ ->
            try
              let ip = Ipaddr.of_string_exn s in
              Ipaddr.Prefix.of_addr ip
            with _ ->
              badconfig "Bad ip/netmask [%s] in <ip> condition" s
        in
        (fun ri ->
           let r = Ipaddr.Prefix.mem
            (Lazy.force (Ocsigen_request_info.remote_ip_parsed ri)) prefix
           in
           if r then
             Lwt_log.ign_info_f ~section
               "IP: %a matches %s"
               (fun () -> Ocsigen_request_info.remote_ip) ri s
           else
             Lwt_log.ign_info_f ~section
               "IP: %a does not match %s"
               (fun () -> Ocsigen_request_info.remote_ip) ri s;
           r)
    | Element ("ip" as s, _, _) -> badconfig "Bad syntax for tag %s" s

    | Element ("port", ["value", s], []) ->
        let port =
          try
            int_of_string s
          with Failure _ ->
            badconfig "Bad port [%s] in <port> condition" s
        in
        (fun ri ->
           let r = Ocsigen_request_info.server_port ri = port in
           if r then
             Lwt_log.ign_info_f ~section
               "PORT: %d accepted" port
           else
             Lwt_log.ign_info_f ~section
               "PORT: %a not accepted (%d expected)"
               (fun () ri -> string_of_int (Ocsigen_request_info.server_port ri))
               ri port;
           r)
    | Element ("port" as s, _, _) -> badconfig "Bad syntax for tag %s" s

    | Element ("ssl", [], []) ->
        (fun ri ->
           let r = Ocsigen_request_info.ssl ri in
           if r then
             Lwt_log.ign_info ~section "SSL: accepted"
           else
             Lwt_log.ign_info ~section "SSL: not accepted";
           r)
    | Element ("ssl" as s, _, _) -> badconfig "Bad syntax for tag %s" s

    | Element ("header", ["name", name; "regexp", reg], []) ->
        let regexp =
          try
            Netstring_pcre.regexp ("^"^reg^"$")
          with Failure _ ->
            badconfig "Bad regular expression [%s] in <header> condition" reg
        in
        (fun ri ->
           let r =
             List.exists
               (fun a ->
                  let r = Netstring_pcre.string_match regexp a 0 <> None in
                  if r then
                    Lwt_log.ign_info_f "HEADER: header %s matches %S" name reg;
                  r)
               (try
                  (Http_headers.find_all
                     (Http_headers.name name)
                     (Ocsigen_request_info.http_frame ri)
                     .Ocsigen_http_frame.frame_header
                     .Ocsigen_http_frame.Http_header.headers)
                with
                  | Not_found -> [])
           in
           if not r
           then Lwt_log.ign_info_f "HEADER: header %s does not match %S" name reg;
           r)
    | Element ("header" as s, _, _) -> badconfig "Bad syntax for tag %s" s

    | Element ("method", ["value", s], []) ->
        let meth =
          try
            Framepp.method_of_string s
          with Failure _ ->
            badconfig "Bad method [%s] in <method> condition" s
        in
        (fun ri ->
           let r = meth = Ocsigen_request_info.meth ri in
           if r then
             Lwt_log.ign_info_f ~section
               "METHOD: %a matches %s"
               (fun () ri -> Framepp.string_of_method (Ocsigen_request_info.meth ri)) ri s
           else
             Lwt_log.ign_info_f ~section
               "METHOD: %a does not match %s"
              (fun () ri -> Framepp.string_of_method (Ocsigen_request_info.meth ri)) ri s;
           r)
    | Element ("method" as s, _, _) -> badconfig "Bad syntax for tag %s" s

    | Element ("protocol", ["value", s], []) ->
        let pr =
          try
            Framepp.proto_of_string s
          with Failure _ ->
            badconfig "Bad protocol [%s] in <protocol> condition" s
        in
        (fun ri ->
           let r = pr = Ocsigen_request_info.protocol ri in
           if r then
             Lwt_log.ign_info_f ~section
               "PROTOCOL: %a matches %s"
              (fun () ri -> Framepp.string_of_proto (Ocsigen_request_info.protocol ri)) ri s
           else
             Lwt_log.ign_info_f ~section
               "PROTOCOL: %a does not match %s"
               (fun () ri -> Framepp.string_of_proto (Ocsigen_request_info.protocol ri)) ri s;
           r)
    | Element ("protocol" as s, _, _) -> badconfig "Bad syntax for tag %s" s

    | Element ("path", ["regexp", s], []) ->
        let regexp =
          try
            Netstring_pcre.regexp ("^"^s^"$")
          with Failure _ ->
            badconfig "Bad regular expression [%s] in <path> condition" s
        in
        (fun ri ->
           let r =
             Netstring_pcre.string_match
               regexp (Ocsigen_request_info.sub_path_string ri) 0 <> None
           in
           if r then
             Lwt_log.ign_info_f ~section
               "PATH: \"%a\" matches %S"
               (fun () ri -> Ocsigen_request_info.sub_path_string ri) ri s
           else
             Lwt_log.ign_info_f ~section
               "PATH: \"%a\" does not match %S"
               (fun () ri -> Ocsigen_request_info.sub_path_string ri) ri s;
           r)
    | Element ("path" as s, _, _) -> badconfig "Bad syntax for tag %s" s

    | Element ("and", [], sub) ->
        let sub = List.map parse_condition sub in
        (fun ri -> List.for_all (fun cond -> cond ri) sub)
    | Element ("and" as s, _, _) -> badconfig "Bad syntax for tag %s" s

    | Element ("or", [], sub) ->
        let sub = List.map parse_condition sub in
        (fun ri -> List.exists (fun cond -> cond ri) sub)
    | Element ("or" as s, _, _) -> badconfig "Bad syntax for tag %s" s

    | Element ("not", [], [sub]) ->
        let sub = parse_condition sub in
        (fun ri -> not (sub ri))
    | Element ("not" as s, _, _) -> badconfig "Bad syntax for tag %s" s

    | _ ->
        badconfig "Bad syntax for condition"


(*****************************************************************************)
(* Parsing filters *)

let comma_space_regexp = Netstring_pcre.regexp "\ *,\ *"

let parse_config parse_fun = function

  | Element ("if", [], sub) ->
      let (condition, sub) = match sub with
        | cond::q -> (parse_condition cond, q)
        | _ -> badconfig "Bad condition in <if>"
      in
      let (ithen, sub) = match sub with
          | Element("then", [], ithen)::q -> (parse_fun ithen, q)
          | _ -> badconfig "Bad <then> branch in <if>"
      in
      let (ielse, sub) = match sub with
          | Element ("else", [], ielse)::([] as q) -> (parse_fun ielse, q)
          | [] -> (parse_fun [], [])
          | _ -> badconfig "Bad <else> branch in <if>"
      in
      (function
        | Ocsigen_extensions.Req_found (ri, _)
        | Ocsigen_extensions.Req_not_found (_, ri) ->
            Lwt.return
              (if condition ri.request_info then begin
                  Lwt_log.ign_info ~section "COND: going into <then> branch";
                  Ocsigen_extensions.Ext_sub_result ithen
               end
               else begin
                 Lwt_log.ign_info ~section "COND: going into <else> branch, if any";
                 Ocsigen_extensions.Ext_sub_result ielse
               end))
  | Element ("if" as s, _, _) -> badconfig "Bad syntax for tag %s" s


  | Element ("notfound", [], []) ->
      (fun rs ->
        Lwt_log.ign_info ~section "NOT_FOUND: taking in charge 404";
         Lwt.return (Ocsigen_extensions.Ext_stop_all
                       (Ocsigen_cookies.Cookies.empty, 404)))
  | Element ("notfound" as s, _, _) -> badconfig "Bad syntax for tag %s" s

  | Element ("nextsite", [], []) ->
      (function
         | Ocsigen_extensions.Req_found (_, r) ->
             Lwt.return (Ocsigen_extensions.Ext_found_stop
                           (fun () -> Lwt.return r))
         | Ocsigen_extensions.Req_not_found (err, ri) ->
             Lwt.return (Ocsigen_extensions.Ext_stop_site
                           (Ocsigen_cookies.Cookies.empty, 404)))

  | Element ("nexthost", [], []) ->
      (function
         | Ocsigen_extensions.Req_found (_, r) ->
             Lwt.return (Ocsigen_extensions.Ext_found_stop
                           (fun () -> Lwt.return r))
         | Ocsigen_extensions.Req_not_found (err, ri) ->
             Lwt.return (Ocsigen_extensions.Ext_stop_host
                           (Ocsigen_cookies.Cookies.empty, 404)))
  | Element ("nextsite" as s, _, _) -> badconfig "Bad syntax for tag %s" s

  | Element ("stop", [], []) ->
      (function
         | Ocsigen_extensions.Req_found (_, r) ->
             Lwt.return (Ocsigen_extensions.Ext_found_stop
                           (fun () -> Lwt.return r))
         | Ocsigen_extensions.Req_not_found (err, ri) ->
             Lwt.return (Ocsigen_extensions.Ext_stop_all
                           (Ocsigen_cookies.Cookies.empty, 404)))
  | Element ("stop" as s, _, _) -> badconfig "Bad syntax for tag %s" s

  | Element ("forbidden", [], []) ->
    (fun rs ->
       Lwt_log.ign_info ~section "FORBIDDEN: taking in charge 403";
         Lwt.return (Ocsigen_extensions.Ext_stop_all
                       (Ocsigen_cookies.Cookies.empty, 403)))
  | Element ("forbidden" as s, _, _) -> badconfig "Bad syntax for tag %s" s

  | Element ("iffound", [], sub) ->
      let ext = parse_fun sub in
      (function
         | Ocsigen_extensions.Req_found (_, _) ->
             Lwt.return (Ext_sub_result ext)
         | Ocsigen_extensions.Req_not_found (err, ri) ->
             Lwt.return (Ocsigen_extensions.Ext_next err))
  | Element ("iffound" as s, _, _) -> badconfig "Bad syntax for tag %s" s

  | Element ("ifnotfound", [], sub) ->
      let ext = parse_fun sub in
      (function
         | Ocsigen_extensions.Req_found (_, r) ->
             Lwt.return (Ocsigen_extensions.Ext_found
                           (fun () -> Lwt.return r))
         | Ocsigen_extensions.Req_not_found (err, ri) ->
             Lwt.return (Ext_sub_result ext))
  | Element ("ifnotfound", [("code", s)], sub) ->
      let ext = parse_fun sub in
      let r = Netstring_pcre.regexp ("^"^s^"$") in
      (function
         | Ocsigen_extensions.Req_found (_, r) ->
             Lwt.return (Ocsigen_extensions.Ext_found
                           (fun () -> Lwt.return r))
         | Ocsigen_extensions.Req_not_found (err, ri) ->
             if Netstring_pcre.string_match r (string_of_int err) 0 <> None then
               Lwt.return (Ext_sub_result ext)
             else
               Lwt.return (Ocsigen_extensions.Ext_next err))
  | Element ("ifnotfound" as s, _, _) -> badconfig "Bad syntax for tag %s" s

  | Element ("allow-forward-for", param, _) ->
    let apply request code =
      Lwt_log.ign_info ~section "Allowed proxy";
      let request =
        try
          let header = Http_headers.find Http_headers.x_forwarded_for
            (Ocsigen_request_info.http_frame request.request_info).frame_header.Http_header.headers in
          match Netstring_pcre.split comma_space_regexp header with
            | []
            | [_] -> Lwt_log.ign_info_f ~section "Malformed X-Forwarded-For field: %s" header;
        request
            | original_ip::proxies ->
        let last_proxy = List.last proxies in
        let proxy_ip = Ipaddr.of_string_exn last_proxy in
        let equal_ip = proxy_ip =
          Lazy.force (Ocsigen_request_info.remote_ip_parsed request.request_info) in
        let need_equal_ip =
          match param with
            | [] -> false
            | ["check-equal-ip",b] ->
              ( try bool_of_string b
          with Invalid_argument _ ->
            badconfig "Bad syntax for argument of tag allow-forward-for" )
            | _ -> badconfig "Bad syntax for argument of tag allow-forward-for"
        in
        if equal_ip || (not need_equal_ip)
        then
          { request with request_info =
              (Ocsigen_request_info.update request.request_info
               ~remote_ip:original_ip
               ~remote_ip_parsed:(lazy (Ipaddr.of_string_exn original_ip))
               ~forward_ip:proxies ()) }
        else (* the announced ip of the proxy is not its real ip *)
          ( Lwt_log.ign_warning_f ~section
              "X-Forwarded-For: host ip ( %a ) does not match the header ( %s )"
              (fun () -> Ocsigen_request_info.remote_ip) request.request_info
              header;
            request )
        with
          | Not_found -> request
      in
            Lwt.return
        (Ocsigen_extensions.Ext_continue_with
           ( request,
             Ocsigen_cookies.Cookies.empty,
             code ))
    in
    (function
      | Ocsigen_extensions.Req_found (request, resp) ->
        apply request (Ocsigen_http_frame.Result.code resp)
      | Ocsigen_extensions.Req_not_found (code, request) -> apply request code)

  | Element ("allow-forward-proto", _, _) ->
    let apply request code =
      Lwt_log.ign_info ~section "Allowed proxy for ssl";
      let request =
        try
          let header = Http_headers.find Http_headers.x_forwarded_proto
            (Ocsigen_request_info.http_frame request.request_info)
            .frame_header.Http_header.headers in
          match String.lowercase header with
            | "http" ->
        { request with request_info =
            (Ocsigen_request_info.update request.request_info
             ~ssl:false ()) }
            | "https" ->
        { request with request_info =
            (Ocsigen_request_info.update request.request_info
              ~ssl:true ()) }
            | _ ->
              Lwt_log.ign_info_f ~section
                "Malformed X-Forwarded-Proto field: %s" header;
        request
        with
          | Not_found -> request
      in
            Lwt.return
        (Ocsigen_extensions.Ext_continue_with
           ( request,
             Ocsigen_cookies.Cookies.empty,
             code ))
    in
    (function
      | Ocsigen_extensions.Req_found (request, resp) ->
        apply request (Ocsigen_http_frame.Result.code resp)
      | Ocsigen_extensions.Req_not_found (code, request) -> apply request code)
  | Element (t, _, _) -> raise (Bad_config_tag_for_extension t)
  | _ -> badconfig "(accesscontrol extension) Bad data"




(*****************************************************************************)
(** Registration of the extension *)
let () = register_extension
  ~name:"accesscontrol"
  ~fun_site:(fun _ _ _ _ -> parse_config)
  ~user_fun_site:(fun _ _ _ _ _ -> parse_config)
  ()
