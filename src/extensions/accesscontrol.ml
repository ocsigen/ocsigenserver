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

(* Filtering requests via the configuration file *)

open Ocsigen_lib

open Xml

let section = Lwt_log.Section.make "ocsigen:ext:access-control"

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
        Ocsigen_extensions.badconfig
          "Bad ip/netmask [%s] in <ip> condition" s
    in
    (fun ri ->
       let r =
         Ipaddr.Prefix.mem
           (Ocsigen_request.remote_ip_parsed ri)
           prefix
       in
       if r then
         Lwt_log.ign_info_f ~section
           "IP: %a matches %s"
           (fun () -> Ocsigen_request.remote_ip) ri s
       else
         Lwt_log.ign_info_f ~section
           "IP: %a does not match %s"
           (fun () -> Ocsigen_request.remote_ip) ri s;
       r)
  | Element ("ip" as s, _, _) ->
    Ocsigen_extensions.badconfig "Bad syntax for tag %s" s

  | Element ("port", ["value", s], []) ->
    let port =
      try
        int_of_string s
      with Failure _ ->
        Ocsigen_extensions.badconfig
          "Bad port [%s] in <port> condition" s
    in
    (fun ri ->
       let r = Ocsigen_request.port ri = port in
       if r then
         Lwt_log.ign_info_f ~section
           "PORT: %d accepted" port
       else
         Lwt_log.ign_info_f ~section
           "PORT: %a not accepted (%d expected)"
           (fun () ri ->
              string_of_int (Ocsigen_request.port ri))
           ri port;
       r)
  | Element ("port" as s, _, _) ->
    Ocsigen_extensions.badconfig "Bad syntax for tag %s" s

  | Element ("ssl", [], []) ->
    (fun ri ->
       let r = Ocsigen_request.ssl ri in
       if r then
         Lwt_log.ign_info ~section "SSL: accepted"
       else
         Lwt_log.ign_info ~section "SSL: not accepted";
       r)
  | Element ("ssl" as s, _, _) ->
    Ocsigen_extensions.badconfig "Bad syntax for tag %s" s

  | Element ("header", ["name", name; "regexp", reg], []) ->
    let regexp =
      try
        Netstring_pcre.regexp ("^" ^ reg ^ "$")
      with Failure _ ->
        Ocsigen_extensions.badconfig
          "Bad regular expression [%s] in <header> condition"
          reg
    in

    (fun ri ->
       let r =
         List.exists
           (fun a ->
              let r = Netstring_pcre.string_match regexp a 0 <> None in
              if r then
                Lwt_log.ign_info_f "HEADER: header %s matches %S" name reg;
              r)
           (Ocsigen_request.header_multi ri
              (Ocsigen_header.Name.of_string name))
       in
       if not r
       then Lwt_log.ign_info_f "HEADER: header %s does not match %S" name reg;
       r)
  | Element ("header" as s, _, _) ->
    Ocsigen_extensions.badconfig "Bad syntax for tag %s" s

  | Element ("method", ["value", s], []) -> fun ri ->
    let m  = Cohttp.Code.method_of_string s
    and m' = Ocsigen_request.meth ri in
    let s' = Cohttp.Code.string_of_method m' in
    let r = m = m' in
    if r then
      Lwt_log.ign_info_f ~section "METHOD: %s matches %s" s' s
    else
      Lwt_log.ign_info_f ~section "METHOD: %s does not match %s" s' s;
    r

  | Element ("method" as s, _, _) ->
    Ocsigen_extensions.badconfig "Bad syntax for tag %s" s

  | Element ("protocol", ["value", s], []) -> fun ri ->
    let v  = Cohttp.Code.version_of_string s
    and v' = Ocsigen_request.version ri in
    let s' = Cohttp.Code.string_of_version v' in
    let r = v = v' in
    if r then
      Lwt_log.ign_info_f ~section "PROTOCOL: %s matches %s" s' s
    else
      Lwt_log.ign_info_f ~section "PROTOCOL: %s does not match %s" s' s;
    r

  | Element ("protocol" as s, _, _) ->
    Ocsigen_extensions.badconfig "Bad syntax for tag %s" s

  | Element ("path", ["regexp", s], []) ->
    let regexp =
      try
        Netstring_pcre.regexp ("^"^s^"$")
      with Failure _ ->
        Ocsigen_extensions.badconfig
          "Bad regular expression [%s] in <path> condition" s
    in
    fun ri ->
      let sps = Ocsigen_request.sub_path_string ri in
      let r = Netstring_pcre.string_match regexp sps 0 <> None in
      if r then
        Lwt_log.ign_info_f ~section "PATH: \"%s\" matches %S" sps s
      else
        Lwt_log.ign_info_f ~section "PATH: \"%s\" does not match %S" sps s;
      r

  | Element ("path" as s, _, _) ->
    Ocsigen_extensions.badconfig "Bad syntax for tag %s" s

  | Element ("and", [], sub) ->
    let sub = List.map parse_condition sub in
    (fun ri -> List.for_all (fun cond -> cond ri) sub)

  | Element ("and" as s, _, _) ->
    Ocsigen_extensions.badconfig "Bad syntax for tag %s" s

  | Element ("or", [], sub) ->
    let sub = List.map parse_condition sub in
    (fun ri -> List.exists (fun cond -> cond ri) sub)

  | Element ("or" as s, _, _) ->
    Ocsigen_extensions.badconfig "Bad syntax for tag %s" s

  | Element ("not", [], [sub]) ->
    let sub = parse_condition sub in
    (fun ri -> not (sub ri))

  | Element ("not" as s, _, _) ->
    Ocsigen_extensions.badconfig "Bad syntax for tag %s" s

  | _ ->
    Ocsigen_extensions.badconfig "Bad syntax for condition"


(*****************************************************************************)
(* Parsing filters *)

let comma_space_regexp = Netstring_pcre.regexp "\ *,\ *"

let parse_config parse_fun = function

  | Element ("if", [], sub) ->
    let (condition, sub) = match sub with
      | cond :: q ->
        parse_condition cond, q
      | _ ->
        Ocsigen_extensions.badconfig "Bad condition in <if>"
    in
    let (ithen, sub) = match sub with
      | Element ("then", [], ithen) :: q ->
        parse_fun ithen, q
      | _ ->
        Ocsigen_extensions.badconfig "Bad <then> branch in <if>"
    in
    let (ielse, _sub) = match sub with
      | Element ("else", [], ielse)::([] as q) ->
        parse_fun ielse, q
      | [] -> (parse_fun [], [])
      | _ ->
        Ocsigen_extensions.badconfig "Bad <else> branch in <if>"
    in
    (function
      | Ocsigen_extensions.Req_found (ri, _)
      | Ocsigen_extensions.Req_not_found (_, ri) ->
        Lwt.return
          (if condition ri.Ocsigen_extensions.request_info then begin
             Lwt_log.ign_info ~section "COND: going into <then> branch";
             Ocsigen_extensions.Ext_sub_result ithen
           end
           else begin
             Lwt_log.ign_info ~section "COND: going into <else> branch, if any";
             Ocsigen_extensions.Ext_sub_result ielse
           end))
  | Element ("if" as s, _, _) ->
    Ocsigen_extensions.badconfig "Bad syntax for tag %s" s


  | Element ("notfound", [], []) ->
    (fun _rs ->
       Lwt_log.ign_info ~section "NOT_FOUND: taking in charge 404";
       Lwt.return (Ocsigen_extensions.Ext_stop_all
                     (Ocsigen_cookie_map.empty, `Not_found)))
  | Element ("notfound" as s, _, _) ->
    Ocsigen_extensions.badconfig "Bad syntax for tag %s" s

  | Element ("nextsite", [], []) ->
    (function
      | Ocsigen_extensions.Req_found (_, r) ->
        Lwt.return (Ocsigen_extensions.Ext_found_stop
                      (fun () -> Lwt.return r))
      | Ocsigen_extensions.Req_not_found _ ->
        Lwt.return (Ocsigen_extensions.Ext_stop_site
                      (Ocsigen_cookie_map.empty, `Not_found)))

  | Element ("nexthost", [], []) ->
    (function
      | Ocsigen_extensions.Req_found (_, r) ->
        Lwt.return (Ocsigen_extensions.Ext_found_stop
                      (fun () -> Lwt.return r))
      | Ocsigen_extensions.Req_not_found _ ->
        Lwt.return (Ocsigen_extensions.Ext_stop_host
                      (Ocsigen_cookie_map.empty, `Not_found)))
  | Element ("nextsite" as s, _, _) ->
    Ocsigen_extensions.badconfig "Bad syntax for tag %s" s

  | Element ("stop", [], []) ->
    (function
      | Ocsigen_extensions.Req_found (_, r) ->
        Lwt.return (Ocsigen_extensions.Ext_found_stop
                      (fun () -> Lwt.return r))
      | Ocsigen_extensions.Req_not_found _ ->
        Lwt.return (Ocsigen_extensions.Ext_stop_all
                      (Ocsigen_cookie_map.empty, `Not_found)))
  | Element ("stop" as s, _, _) ->
    Ocsigen_extensions.badconfig "Bad syntax for tag %s" s

  | Xml.Element ("forbidden", [], []) ->
    (fun _rs ->
       Lwt_log.ign_info ~section "FORBIDDEN: taking in charge 403";
       Lwt.return (Ocsigen_extensions.Ext_stop_all
                     (Ocsigen_cookie_map.empty, `Forbidden)))

  | Element ("forbidden" as s, _, _) ->
    Ocsigen_extensions.badconfig "Bad syntax for tag %s" s

  | Element ("iffound", [], sub) ->
    let ext = parse_fun sub in
    (function
      | Ocsigen_extensions.Req_found (_, _) ->
        Lwt.return (Ocsigen_extensions.Ext_sub_result ext)
      | Ocsigen_extensions.Req_not_found (err, _ri) ->
        Lwt.return (Ocsigen_extensions.Ext_next err))

  | Element ("iffound" as s, _, _) ->
    Ocsigen_extensions.badconfig "Bad syntax for tag %s" s

  | Element ("ifnotfound", [], sub) ->
    let ext = parse_fun sub in
    (function
      | Ocsigen_extensions.Req_found (_, r) ->
        Lwt.return (Ocsigen_extensions.Ext_found
                      (fun () -> Lwt.return r))
      | Ocsigen_extensions.Req_not_found _ ->
        Lwt.return (Ocsigen_extensions.Ext_sub_result ext))
  | Element ("ifnotfound", [("code", s)], sub) ->
    let ext = parse_fun sub in
    let r = Netstring_pcre.regexp ("^"^s^"$") in
    (function
      | Ocsigen_extensions.Req_found (_, r) ->
        Lwt.return (Ocsigen_extensions.Ext_found
                      (fun () -> Lwt.return r))
      | Ocsigen_extensions.Req_not_found (err, _ri) ->
        if
          let err =
            string_of_int
              Cohttp.Code.(code_of_status (err :> status_code))
          in
          Netstring_pcre.string_match r err 0 <> None
        then
          Lwt.return (Ocsigen_extensions.Ext_sub_result ext)
        else
          Lwt.return (Ocsigen_extensions.Ext_next err))
  | Element ("ifnotfound" as s, _, _) ->
    Ocsigen_extensions.badconfig "Bad syntax for tag %s" s

  | Element ("allow-forward-for", param, _) ->
    let apply ({Ocsigen_extensions.request_info ;_} as request) code =
      Lwt_log.ign_info ~section "Allowed proxy";
      let request =
        let header =
          Ocsigen_request.header
            request_info
            Ocsigen_header.Name.x_forwarded_for
        in
        match header with
        | Some header ->
          (match
             Ocsigen_lib.Netstring_pcre.split
               comma_space_regexp header
           with
           | original_ip :: proxies ->
             let last_proxy = List.last proxies in
             let proxy_ip = Ipaddr.of_string_exn last_proxy in
             let equal_ip =
               proxy_ip =
               Ocsigen_request.remote_ip_parsed request_info
             in
             let need_equal_ip =
               match param with
               | [] -> false
               | ["check-equal-ip", b] ->
                 ( try
                     bool_of_string b
                   with Invalid_argument _ ->
                     Ocsigen_extensions.badconfig
                       "Bad syntax for argument of tag allow-forward-for")
               | _ ->
                 Ocsigen_extensions.badconfig
                   "Bad syntax for argument of tag allow-forward-for"
             in
             if equal_ip || not need_equal_ip then
               { request
                 with
                   Ocsigen_extensions.request_info =
                     Ocsigen_request.update
                       ~forward_ip:proxies
                       ~remote_ip:original_ip
                       request_info
               }
             else (* the announced ip of the proxy is not its real ip *)
               (Lwt_log.ign_warning_f ~section
                  "X-Forwarded-For: host ip (%s) \
                   does not match the header (%s)"
                  (Ocsigen_request.remote_ip request_info)
                  header;
                request)
           | _ ->
             Lwt_log.ign_info_f ~section
               "Malformed X-Forwarded-For field: %s" header;
             request)
        | None ->
          request
      in
      Lwt.return
        (Ocsigen_extensions.Ext_continue_with
           ( request,
             Ocsigen_cookie_map.empty,
             code ))
    in
    (function
      | Ocsigen_extensions.Req_found (request, resp) ->
        apply request (Ocsigen_response.status resp)
      | Ocsigen_extensions.Req_not_found (code, request) ->
        apply request code)

  | Element ("allow-forward-proto", _, _) ->
    let apply ({ Ocsigen_extensions.request_info ; _ } as request) code =
      Lwt_log.ign_info ~section "Allowed proxy for ssl";
      let request_info =
        let header =
          Ocsigen_request.header
            request_info
            Ocsigen_header.Name.x_forwarded_proto
        in
        match header with
        | Some header ->
          (match String.lowercase_ascii header with
           | "http" ->
             Ocsigen_request.update ~ssl:false request_info
           | "https" ->
             Ocsigen_request.update ~ssl:true request_info
           | _ ->
             Lwt_log.ign_info_f ~section
               "Malformed X-Forwarded-Proto field: %s" header;
             request_info)
        | None ->
          request_info
      in
      Lwt.return
        (Ocsigen_extensions.Ext_continue_with
           ( { request with Ocsigen_extensions.request_info },
             Ocsigen_cookie_map.empty,
             code ))
    in
    (function
      | Ocsigen_extensions.Req_found (request, resp) ->
        apply request (Ocsigen_response.status resp)
      | Ocsigen_extensions.Req_not_found (code, request) -> apply request code)
  | Element (t, _, _) ->
    raise (Ocsigen_extensions.Bad_config_tag_for_extension t)
  | _ ->
    Ocsigen_extensions.badconfig "(accesscontrol extension) Bad data"

(* Registration of the extension *)
let () =
  Ocsigen_extensions.register
    ~name:"accesscontrol"
    ~fun_site:(fun _ _ _ _ _ -> parse_config)
    ()
