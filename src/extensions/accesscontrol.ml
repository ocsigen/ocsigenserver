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

let section = Logs.Src.create "ocsigen:ext:access-control"

type condition = Ocsigen_request.t -> bool

let ip s =
  let prefix =
    try Ipaddr.Prefix.of_string_exn s
    with Ipaddr.Parse_error _ -> (
      try
        let ip = Ipaddr.of_string_exn s in
        Ipaddr.Prefix.of_addr ip
      with _ ->
        Ocsigen_extensions.badconfig "Bad ip/netmask [%s] in <ip> condition" s)
  in
  fun ri ->
    let r =
      match Ocsigen_request.remote_ip_parsed ri with
      | `Ip ip -> Ipaddr.Prefix.mem ip prefix
      | `Unix _ -> false
    in
    if r
    then
      Logs.info ~src:section (fun fmt ->
        fmt "IP: %s matches %s" (Ocsigen_request.remote_ip ri) s)
    else
      Logs.info ~src:section (fun fmt ->
        fmt "IP: %s does not match %s" (Ocsigen_request.remote_ip ri) s);
    r

let port port ri =
  let r = Ocsigen_request.port ri = port in
  if r
  then Logs.info ~src:section (fun fmt -> fmt "PORT = %d: true" port)
  else
    Logs.info ~src:section (fun fmt ->
      fmt "PORT = %d: false (it is %s)" port
        (string_of_int (Ocsigen_request.port ri)));
  r

let ssl ri =
  let r = Ocsigen_request.ssl ri in
  if r
  then Logs.info ~src:section (fun fmt -> fmt "SSL: true")
  else Logs.info ~src:section (fun fmt -> fmt "SSL: false");
  r

let header ~name ~regexp:re =
  let regexp =
    try Netstring_pcre.regexp ("^" ^ re ^ "$")
    with Failure _ ->
      Ocsigen_extensions.badconfig
        "Bad regular expression [%s] in <header> condition" re
  in
  fun ri ->
    let r =
      List.exists
        (fun a ->
           let r = Netstring_pcre.string_match regexp a 0 <> None in
           if r
           then
             Logs.info (fun fmt -> fmt "HEADER: header %s matches %S" name re);
           r)
        (Ocsigen_request.header_multi ri (Ocsigen_header.Name.of_string name))
    in
    if not r
    then
      Logs.info (fun fmt -> fmt "HEADER: header %s does not match %S" name re);
    r

let method_ m ri =
  let m' = Ocsigen_request.meth ri in
  let s = Cohttp.Code.string_of_method m in
  let s' = Cohttp.Code.string_of_method m' in
  let r = m = m' in
  if r
  then Logs.info ~src:section (fun fmt -> fmt "METHOD: %s matches %s" s' s)
  else
    Logs.info ~src:section (fun fmt -> fmt "METHOD: %s does not match %s" s' s);
  r

let protocol v ri =
  let v' = Ocsigen_request.version ri in
  let s = Cohttp.Code.string_of_version v in
  let s' = Cohttp.Code.string_of_version v' in
  let r = v = v' in
  if r
  then Logs.info ~src:section (fun fmt -> fmt "PROTOCOL: %s matches %s" s' s)
  else
    Logs.info ~src:section (fun fmt ->
      fmt "PROTOCOL: %s does not match %s" s' s);
  r

let path ~regexp:s =
  let regexp =
    try Netstring_pcre.regexp ("^" ^ s ^ "$")
    with Failure _ ->
      Ocsigen_extensions.badconfig
        "Bad regular expression [%s] in <path> condition" s
  in
  fun ri ->
    let sps = Ocsigen_request.sub_path_string ri in
    let r = Netstring_pcre.string_match regexp sps 0 <> None in
    if r
    then Logs.info ~src:section (fun fmt -> fmt "PATH: \"%s\" matches %S" sps s)
    else
      Logs.info ~src:section (fun fmt ->
        fmt "PATH: \"%s\" does not match %S" sps s);
    r

let and_ sub ri = List.for_all (fun cond -> cond ri) sub
let or_ sub ri = List.exists (fun cond -> cond ri) sub
let not_ sub ri = not (sub ri)

let rec parse_condition = function
  | Element ("ip", [("value", s)], []) -> ip s
  | Element (("ip" as s), _, _) ->
      Ocsigen_extensions.badconfig "Bad syntax for tag %s" s
  | Element ("port", [("value", s)], []) ->
      let p =
        try int_of_string s
        with Failure _ ->
          Ocsigen_extensions.badconfig "Bad port [%s] in <port> condition" s
      in
      port p
  | Element (("port" as s), _, _) ->
      Ocsigen_extensions.badconfig "Bad syntax for tag %s" s
  | Element ("ssl", [], []) -> ssl
  | Element (("ssl" as s), _, _) ->
      Ocsigen_extensions.badconfig "Bad syntax for tag %s" s
  | Element ("header", [("name", name); ("regexp", regexp)], []) ->
      header ~name ~regexp
  | Element (("header" as s), _, _) ->
      Ocsigen_extensions.badconfig "Bad syntax for tag %s" s
  | Element ("method", [("value", s)], []) ->
      let m = Cohttp.Code.method_of_string s in
      method_ m
  | Element (("method" as s), _, _) ->
      Ocsigen_extensions.badconfig "Bad syntax for tag %s" s
  | Element ("protocol", [("value", s)], []) ->
      let v = Cohttp.Code.version_of_string s in
      protocol v
  | Element (("protocol" as s), _, _) ->
      Ocsigen_extensions.badconfig "Bad syntax for tag %s" s
  | Element ("path", [("regexp", s)], []) ->
      let regexp =
        try Netstring_pcre.regexp ("^" ^ s ^ "$")
        with Failure _ ->
          Ocsigen_extensions.badconfig
            "Bad regular expression [%s] in <path> condition" s
      in
      fun ri ->
        let sps = Ocsigen_request.sub_path_string ri in
        let r = Netstring_pcre.string_match regexp sps 0 <> None in
        if r
        then
          Logs.info ~src:section (fun fmt ->
            fmt "PATH: \"%s\" matches %S" sps s)
        else
          Logs.info ~src:section (fun fmt ->
            fmt "PATH: \"%s\" does not match %S" sps s);
        r
  | Element (("path" as s), _, _) ->
      Ocsigen_extensions.badconfig "Bad syntax for tag %s" s
  | Element ("and", [], sub) ->
      let sub = List.map parse_condition sub in
      fun ri -> List.for_all (fun cond -> cond ri) sub
  | Element (("and" as s), _, _) ->
      Ocsigen_extensions.badconfig "Bad syntax for tag %s" s
  | Element ("or", [], sub) ->
      let sub = List.map parse_condition sub in
      fun ri -> List.exists (fun cond -> cond ri) sub
  | Element (("or" as s), _, _) ->
      Ocsigen_extensions.badconfig "Bad syntax for tag %s" s
  | Element ("not", [], [sub]) ->
      let sub = parse_condition sub in
      fun ri -> not (sub ri)
  | Element (("not" as s), _, _) ->
      Ocsigen_extensions.badconfig "Bad syntax for tag %s" s
  | _ -> Ocsigen_extensions.badconfig "Bad syntax for condition"

(*****************************************************************************)
(* Parsing filters *)

let comma_space_regexp = Netstring_pcre.regexp " *, *"

let allow_forward_for_handler ?(check_equal_ip = false) () =
  let apply ({Ocsigen_extensions.request_info; _} as request) code =
    Logs.info ~src:section (fun fmt -> fmt "Allowed proxy");
    let request =
      let header =
        Ocsigen_request.header request_info Ocsigen_header.Name.x_forwarded_for
      in
      match header with
      | Some header -> (
        match Ocsigen_lib.Netstring_pcre.split comma_space_regexp header with
        | original_ip :: proxies ->
            let last_proxy = List.last proxies in
            let proxy_ip = Ipaddr.of_string_exn last_proxy in
            let equal_ip =
              match Ocsigen_request.remote_ip_parsed request_info with
              | `Ip r_ip -> Ipaddr.compare proxy_ip r_ip = 0
              | `Unix _ -> false
            in
            if equal_ip || not check_equal_ip
            then
              { request with
                Ocsigen_extensions.request_info =
                  Ocsigen_request.update ~forward_ip:proxies
                    ~remote_ip:original_ip request_info }
            else (
              (* the announced ip of the proxy is not its real ip *)
              Logs.warn ~src:section (fun fmt ->
                fmt
                  "X-Forwarded-For: host ip (%s) does not match the header (%s)"
                  (Ocsigen_request.remote_ip request_info)
                  header);
              request)
        | _ ->
            Logs.info ~src:section (fun fmt ->
              fmt "Malformed X-Forwarded-For field: %s" header);
            request)
      | None -> request
    in
    Ocsigen_extensions.Ext_continue_with
      (request, Ocsigen_cookie_map.empty, code)
  in
  function
  | Ocsigen_extensions.Req_found (request, resp) ->
      apply request (Ocsigen_response.status resp)
  | Ocsigen_extensions.Req_not_found (code, request) -> apply request code

let allow_forward_proto_handler =
  let apply ({Ocsigen_extensions.request_info; _} as request) code =
    Logs.info ~src:section (fun fmt -> fmt "Allowed proxy for ssl");
    let request_info =
      let header =
        Ocsigen_request.header request_info
          Ocsigen_header.Name.x_forwarded_proto
      in
      match header with
      | Some header -> (
        match String.lowercase_ascii header with
        | "http" -> Ocsigen_request.update ~ssl:false request_info
        | "https" -> Ocsigen_request.update ~ssl:true request_info
        | _ ->
            Logs.info ~src:section (fun fmt ->
              fmt "Malformed X-Forwarded-Proto field: %s" header);
            request_info)
      | None -> request_info
    in
    Ocsigen_extensions.Ext_continue_with
      ( {request with Ocsigen_extensions.request_info}
      , Ocsigen_cookie_map.empty
      , code )
  in
  function
  | Ocsigen_extensions.Req_found (request, resp) ->
      apply request (Ocsigen_response.status resp)
  | Ocsigen_extensions.Req_not_found (code, request) -> apply request code

let parse_config parse_fun = function
  | Element ("if", [], sub) -> (
      let condition, sub =
        match sub with
        | cond :: q -> parse_condition cond, q
        | _ -> Ocsigen_extensions.badconfig "Bad condition in <if>"
      in
      let ithen, sub =
        match sub with
        | Element ("then", [], ithen) :: q -> parse_fun ithen, q
        | _ -> Ocsigen_extensions.badconfig "Bad <then> branch in <if>"
      in
      let ielse, (_sub : _ list) =
        match sub with
        | Element ("else", [], ielse) :: ([] as q) -> parse_fun ielse, q
        | [] -> parse_fun [], []
        | _ -> Ocsigen_extensions.badconfig "Bad <else> branch in <if>"
      in
      function
      | Ocsigen_extensions.Req_found (ri, _)
      | Ocsigen_extensions.Req_not_found (_, ri) ->
          if condition ri.Ocsigen_extensions.request_info
          then (
            Logs.info ~src:section (fun fmt ->
              fmt "COND: going into <then> branch");
            Ocsigen_extensions.Ext_sub_result ithen)
          else (
            Logs.info ~src:section (fun fmt ->
              fmt "COND: going into <else> branch, if any");
            Ocsigen_extensions.Ext_sub_result ielse))
  | Element (("if" as s), _, _) ->
      Ocsigen_extensions.badconfig "Bad syntax for tag %s" s
  | Element ("notfound", [], []) ->
      fun _rs ->
        Logs.info ~src:section (fun fmt ->
          fmt "NOT_FOUND: taking in charge 404");
        Ocsigen_extensions.Ext_stop_all (Ocsigen_cookie_map.empty, `Not_found)
  | Element (("notfound" as s), _, _) ->
      Ocsigen_extensions.badconfig "Bad syntax for tag %s" s
  | Element ("nextsite", [], []) -> (
      function
      | Ocsigen_extensions.Req_found (_, r) ->
          Ocsigen_extensions.Ext_found_stop (fun () -> r)
      | Ocsigen_extensions.Req_not_found _ ->
          Ocsigen_extensions.Ext_stop_site (Ocsigen_cookie_map.empty, `Not_found)
      )
  | Element ("nexthost", [], []) -> (
      function
      | Ocsigen_extensions.Req_found (_, r) ->
          Ocsigen_extensions.Ext_found_stop (fun () -> r)
      | Ocsigen_extensions.Req_not_found _ ->
          Ocsigen_extensions.Ext_stop_host (Ocsigen_cookie_map.empty, `Not_found)
      )
  | Element (("nextsite" as s), _, _) ->
      Ocsigen_extensions.badconfig "Bad syntax for tag %s" s
  | Element ("stop", [], []) -> (
      function
      | Ocsigen_extensions.Req_found (_, r) ->
          Ocsigen_extensions.Ext_found_stop (fun () -> r)
      | Ocsigen_extensions.Req_not_found _ ->
          Ocsigen_extensions.Ext_stop_all (Ocsigen_cookie_map.empty, `Not_found)
      )
  | Element (("stop" as s), _, _) ->
      Ocsigen_extensions.badconfig "Bad syntax for tag %s" s
  | Xml.Element ("forbidden", [], []) ->
      fun _rs ->
        Logs.info ~src:section (fun fmt ->
          fmt "FORBIDDEN: taking in charge 403");
        Ocsigen_extensions.Ext_stop_all (Ocsigen_cookie_map.empty, `Forbidden)
  | Element (("forbidden" as s), _, _) ->
      Ocsigen_extensions.badconfig "Bad syntax for tag %s" s
  | Element ("iffound", [], sub) -> (
      let ext = parse_fun sub in
      function
      | Ocsigen_extensions.Req_found (_, _) ->
          Ocsigen_extensions.Ext_sub_result ext
      | Ocsigen_extensions.Req_not_found (err, _ri) ->
          Ocsigen_extensions.Ext_next err)
  | Element (("iffound" as s), _, _) ->
      Ocsigen_extensions.badconfig "Bad syntax for tag %s" s
  | Element ("ifnotfound", [], sub) -> (
      let ext = parse_fun sub in
      function
      | Ocsigen_extensions.Req_found (_, r) ->
          Ocsigen_extensions.Ext_found (fun () -> r)
      | Ocsigen_extensions.Req_not_found _ ->
          Ocsigen_extensions.Ext_sub_result ext)
  | Element ("ifnotfound", [("code", s)], sub) -> (
      let ext = parse_fun sub in
      let re = Netstring_pcre.regexp ("^" ^ s ^ "$") in
      function
      | Ocsigen_extensions.Req_found (_, r) ->
          Ocsigen_extensions.Ext_found (fun () -> r)
      | Ocsigen_extensions.Req_not_found (err, _ri) ->
          if
            let err =
              string_of_int Cohttp.Code.(code_of_status (err :> status_code))
            in
            Netstring_pcre.string_match re err 0 <> None
          then Ocsigen_extensions.Ext_sub_result ext
          else Ocsigen_extensions.Ext_next err)
  | Element (("ifnotfound" as s), _, _) ->
      Ocsigen_extensions.badconfig "Bad syntax for tag %s" s
  | Element ("allow-forward-for", param, _) ->
      let check_equal_ip =
        match param with
        | [] -> false
        | [("check-equal-ip", b)] -> (
          try bool_of_string b
          with Invalid_argument _ ->
            Ocsigen_extensions.badconfig
              "Bad syntax for argument of tag allow-forward-for")
        | _ ->
            Ocsigen_extensions.badconfig
              "Bad syntax for argument of tag allow-forward-for"
      in
      allow_forward_for_handler ~check_equal_ip ()
  | Element ("allow-forward-proto", _, _) -> allow_forward_proto_handler
  | Element (t, _, _) ->
      raise (Ocsigen_extensions.Bad_config_tag_for_extension t)
  | _ -> Ocsigen_extensions.badconfig "(accesscontrol extension) Bad data"

(* Registration of the extension for the config file: *)
let () =
  Ocsigen_extensions.register ~name:"accesscontrol"
    ~fun_site:(fun _ _ _ _ _ -> parse_config)
    ()

let if_ condition ithen ielse vh ci p = function
  | Ocsigen_extensions.Req_found (ri, _)
  | Ocsigen_extensions.Req_not_found (_, ri) ->
      if condition ri.Ocsigen_extensions.request_info
      then
        Ocsigen_extensions.Ext_sub_result
          (Ocsigen_extensions.compose (List.map (fun i -> i vh ci p) ithen))
      else
        Ocsigen_extensions.Ext_sub_result
          (Ocsigen_extensions.compose (List.map (fun i -> i vh ci p) ielse))

let iffound instrs vh ci p = function
  | Ocsigen_extensions.Req_found (_, _) ->
      Ocsigen_extensions.Ext_sub_result
        (Ocsigen_extensions.compose (List.map (fun i -> i vh ci p) instrs))
  | Ocsigen_extensions.Req_not_found (err, _ri) ->
      Ocsigen_extensions.Ext_next err

let ifnotfound ?code instrs vh ci p =
  let re = Option.map (fun s -> Netstring_pcre.regexp ("^" ^ s ^ "$")) code in
  function
  | Ocsigen_extensions.Req_found (_, r) ->
      Ocsigen_extensions.Ext_found (fun () -> r)
  | Ocsigen_extensions.Req_not_found (err, _) -> (
    match re with
    | None ->
        Ocsigen_extensions.Ext_sub_result
          (Ocsigen_extensions.compose (List.map (fun i -> i vh ci p) instrs))
    | Some re ->
        if
          let err =
            string_of_int Cohttp.Code.(code_of_status (err :> status_code))
          in
          Netstring_pcre.string_match re err 0 <> None
        then
          Ocsigen_extensions.Ext_sub_result
            (Ocsigen_extensions.compose (List.map (fun i -> i vh ci p) instrs))
        else Ocsigen_extensions.Ext_next err)

let notfound _ _ _ _ =
  Ocsigen_extensions.Ext_stop_all (Ocsigen_cookie_map.empty, `Not_found)

let nextsite _ _ _ = function
  | Ocsigen_extensions.Req_found (_, r) ->
      Ocsigen_extensions.Ext_found_stop (fun () -> r)
  | Ocsigen_extensions.Req_not_found _ ->
      Ocsigen_extensions.Ext_stop_site (Ocsigen_cookie_map.empty, `Not_found)

let nexthost _ _ _ = function
  | Ocsigen_extensions.Req_found (_, r) ->
      Ocsigen_extensions.Ext_found_stop (fun () -> r)
  | Ocsigen_extensions.Req_not_found _ ->
      Ocsigen_extensions.Ext_stop_host (Ocsigen_cookie_map.empty, `Not_found)

let stop _ _ _ = function
  | Ocsigen_extensions.Req_found (_, r) ->
      Ocsigen_extensions.Ext_found_stop (fun () -> r)
  | Ocsigen_extensions.Req_not_found _ ->
      Ocsigen_extensions.Ext_stop_all (Ocsigen_cookie_map.empty, `Not_found)

let forbidden _ _ _ _ =
  Ocsigen_extensions.Ext_stop_all (Ocsigen_cookie_map.empty, `Forbidden)

let allow_forward_for ?check_equal_ip () _ _ _ =
  allow_forward_for_handler ?check_equal_ip ()

let allow_forward_proto () _ _ _ = allow_forward_proto_handler
