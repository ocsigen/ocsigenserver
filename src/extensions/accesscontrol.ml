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

open Ocsigen_base.Lib
open Xml

let section = Logs.Src.create "ocsigen:ext:access-control"

type condition = Ocsigen.Request.t -> bool

(* The network [s] of the configuration file, an address or an address with a
   netmask, found in [where] *)
let prefix ~where s =
  match Ipaddr.Prefix.of_string s with
  | Ok prefix -> prefix
  | Error _ -> (
    match Ipaddr.of_string s with
    | Ok ip -> Ipaddr.Prefix.of_addr ip
    | Error _ ->
        Ocsigen.Extensions.badconfig "Bad ip/netmask [%s] in %s" s where)

let ip s =
  let prefix = prefix ~where:"<ip> condition" s in
  fun ri ->
    let r =
      match Ocsigen.Request.client_conn ri with
      | `Inet (ip, _) -> Ipaddr.Prefix.mem ip prefix
      (* The address given by a trusted proxy, after <allow-forward-for/> *)
      | `Forwarded_for ip -> (
        match Ipaddr.of_string ip with
        | Ok ip -> Ipaddr.Prefix.mem ip prefix
        | Error _ -> false)
      | `Unix _ | `Unknown -> false
    in
    if r
    then
      Logs.info ~src:section (fun fmt ->
        fmt "IP: %s matches %s" (Ocsigen.Request.client_conn_to_string ri) s)
    else
      Logs.info ~src:section (fun fmt ->
        fmt "IP: %s does not match %s"
          (Ocsigen.Request.client_conn_to_string ri)
          s);
    r

let port port ri =
  let r = Ocsigen.Request.port ri = port in
  if r
  then Logs.info ~src:section (fun fmt -> fmt "PORT = %d: true" port)
  else
    Logs.info ~src:section (fun fmt ->
      fmt "PORT = %d: false (it is %s)" port
        (string_of_int (Ocsigen.Request.port ri)));
  r

let ssl ri =
  let r = Ocsigen.Request.ssl ri in
  if r
  then Logs.info ~src:section (fun fmt -> fmt "SSL: true")
  else Logs.info ~src:section (fun fmt -> fmt "SSL: false");
  r

let header ~name ~regexp:re =
  let regexp =
    try Netstring_pcre.regexp ("^" ^ re ^ "$")
    with Failure _ ->
      Ocsigen.Extensions.badconfig
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
        (Ocsigen.Request.header_multi ri
           (Ocsigen_http.Header.Name.of_string name))
    in
    if not r
    then
      Logs.info (fun fmt -> fmt "HEADER: header %s does not match %S" name re);
    r

let method_ m ri =
  let m' = Ocsigen.Request.meth ri in
  let s = Cohttp.Code.string_of_method m in
  let s' = Cohttp.Code.string_of_method m' in
  let r = m = m' in
  if r
  then Logs.info ~src:section (fun fmt -> fmt "METHOD: %s matches %s" s' s)
  else
    Logs.info ~src:section (fun fmt -> fmt "METHOD: %s does not match %s" s' s);
  r

let protocol v ri =
  let v' = Ocsigen.Request.version ri in
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
      Ocsigen.Extensions.badconfig
        "Bad regular expression [%s] in <path> condition" s
  in
  fun ri ->
    let sps = Ocsigen.Request.sub_path_string ri in
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
      Ocsigen.Extensions.badconfig "Bad syntax for tag %s" s
  | Element ("port", [("value", s)], []) ->
      let p =
        try int_of_string s
        with Failure _ ->
          Ocsigen.Extensions.badconfig "Bad port [%s] in <port> condition" s
      in
      port p
  | Element (("port" as s), _, _) ->
      Ocsigen.Extensions.badconfig "Bad syntax for tag %s" s
  | Element ("ssl", [], []) -> ssl
  | Element (("ssl" as s), _, _) ->
      Ocsigen.Extensions.badconfig "Bad syntax for tag %s" s
  | Element ("header", [("name", name); ("regexp", regexp)], []) ->
      header ~name ~regexp
  | Element (("header" as s), _, _) ->
      Ocsigen.Extensions.badconfig "Bad syntax for tag %s" s
  | Element ("method", [("value", s)], []) ->
      let m = Cohttp.Code.method_of_string s in
      method_ m
  | Element (("method" as s), _, _) ->
      Ocsigen.Extensions.badconfig "Bad syntax for tag %s" s
  | Element ("protocol", [("value", s)], []) ->
      let v = Cohttp.Code.version_of_string s in
      protocol v
  | Element (("protocol" as s), _, _) ->
      Ocsigen.Extensions.badconfig "Bad syntax for tag %s" s
  | Element ("path", [("regexp", s)], []) ->
      let regexp =
        try Netstring_pcre.regexp ("^" ^ s ^ "$")
        with Failure _ ->
          Ocsigen.Extensions.badconfig
            "Bad regular expression [%s] in <path> condition" s
      in
      fun ri ->
        let sps = Ocsigen.Request.sub_path_string ri in
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
      Ocsigen.Extensions.badconfig "Bad syntax for tag %s" s
  | Element ("and", [], sub) ->
      let sub = List.map parse_condition sub in
      fun ri -> List.for_all (fun cond -> cond ri) sub
  | Element (("and" as s), _, _) ->
      Ocsigen.Extensions.badconfig "Bad syntax for tag %s" s
  | Element ("or", [], sub) ->
      let sub = List.map parse_condition sub in
      fun ri -> List.exists (fun cond -> cond ri) sub
  | Element (("or" as s), _, _) ->
      Ocsigen.Extensions.badconfig "Bad syntax for tag %s" s
  | Element ("not", [], [sub]) ->
      let sub = parse_condition sub in
      fun ri -> not (sub ri)
  | Element (("not" as s), _, _) ->
      Ocsigen.Extensions.badconfig "Bad syntax for tag %s" s
  | _ -> Ocsigen.Extensions.badconfig "Bad syntax for condition"

(*****************************************************************************)
(* Parsing filters *)

(* A comma, with the spaces and tabs around it (RFC 9110, section 5.6.3) *)
let comma_space_regexp = Netstring_pcre.regexp "[ \t]*,[ \t]*"
let blanks_regexp = Netstring_pcre.regexp "[ \t\r\n]+"

(* An entry of X-Forwarded-For: an address, or another value, such as
   "unknown" *)
type forwarded_entry = Address of Ipaddr.t | Other of string

(* The entry [s], without the port that some proxies add: "192.0.2.1:4711"
   and "[2001:db8::1]:4711" give the address alone. *)
let forwarded_entry s =
  match Ipaddr.with_port_of_string ~default:0 s with
  | Ok (ip, _) -> Address ip
  | Error _ -> Other s

let string_of_forwarded_entry = function
  | Address ip -> Ipaddr.to_string ip
  | Other s -> s

(* [split_client ~proxies entries] is [Some (client, forward)], for the
   entries of X-Forwarded-For from left to right, or [None] if there is none.
   Each proxy adds the address of the host that connected to it, so the
   entries are read from the right, skipping those of the trusted [proxies].
   The client is the rightmost other one, or the leftmost entry if all are
   trusted. [forward] are the entries at its right, those of the proxies: the
   entries at its left come from the client, which can choose them. *)
let split_client ~proxies entries =
  let trusted = function
    | Address ip -> List.exists (Ipaddr.Prefix.mem ip) proxies
    | Other _ -> false
  in
  let rec split client forward = function
    | next :: before when trusted client ->
        split next (client :: forward) before
    | _ -> client, forward
  in
  match List.rev entries with
  | [] -> None
  | last :: before -> Some (split last [] before)

let allow_forward_for_handler ~proxies =
  let apply ({Ocsigen.Extensions.request_info; _} as request) code =
    Logs.info ~src:section (fun fmt -> fmt "Allowed proxy");
    let request =
      (* Several lines of the header make one list (RFC 9110, section 5.3) *)
      match
        Ocsigen.Request.header_multi request_info
          Ocsigen_http.Header.Name.x_forwarded_for
      with
      | [] -> request
      | lines -> (
          let header = String.concat ", " lines in
          let entries =
            Ocsigen_base.Lib.Netstring_pcre.split comma_space_regexp header
            |> List.filter (fun entry -> entry <> "")
            |> List.map forwarded_entry
          in
          match split_client ~proxies entries with
          | Some (client, forward) ->
              { request with
                Ocsigen.Extensions.request_info =
                  Ocsigen.Request.update
                    ~forward_ip:(List.map string_of_forwarded_entry forward)
                    ~client_conn:
                      (`Forwarded_for (string_of_forwarded_entry client))
                    request_info }
          | None ->
              Logs.info ~src:section (fun fmt ->
                fmt "Malformed X-Forwarded-For field: %s" header);
              request)
    in
    Lwt.return
      (Ocsigen.Extensions.Ext_continue_with
         (request, Ocsigen_cookie_map.empty, code))
  in
  function
  | Ocsigen.Extensions.Req_found (request, resp) ->
      apply request (Ocsigen.Response.status resp)
  | Ocsigen.Extensions.Req_not_found (code, request) -> apply request code

let allow_forward_proto_handler =
  let apply ({Ocsigen.Extensions.request_info; _} as request) code =
    Logs.info ~src:section (fun fmt -> fmt "Allowed proxy for ssl");
    let request_info =
      let header =
        Ocsigen.Request.header request_info
          Ocsigen_http.Header.Name.x_forwarded_proto
      in
      match header with
      | Some header -> (
        match String.lowercase_ascii header with
        | "http" -> Ocsigen.Request.update ~ssl:false request_info
        | "https" -> Ocsigen.Request.update ~ssl:true request_info
        | _ ->
            Logs.info ~src:section (fun fmt ->
              fmt "Malformed X-Forwarded-Proto field: %s" header);
            request_info)
      | None -> request_info
    in
    Lwt.return
      (Ocsigen.Extensions.Ext_continue_with
         ( {request with Ocsigen.Extensions.request_info}
         , Ocsigen_cookie_map.empty
         , code ))
  in
  function
  | Ocsigen.Extensions.Req_found (request, resp) ->
      apply request (Ocsigen.Response.status resp)
  | Ocsigen.Extensions.Req_not_found (code, request) -> apply request code

let parse_config parse_fun = function
  | Element ("if", [], sub) -> (
      let condition, sub =
        match sub with
        | cond :: q -> parse_condition cond, q
        | _ -> Ocsigen.Extensions.badconfig "Bad condition in <if>"
      in
      let ithen, sub =
        match sub with
        | Element ("then", [], ithen) :: q -> parse_fun ithen, q
        | _ -> Ocsigen.Extensions.badconfig "Bad <then> branch in <if>"
      in
      let ielse, (_sub : _ list) =
        match sub with
        | Element ("else", [], ielse) :: ([] as q) -> parse_fun ielse, q
        | [] -> parse_fun [], []
        | _ -> Ocsigen.Extensions.badconfig "Bad <else> branch in <if>"
      in
      function
      | Ocsigen.Extensions.Req_found (ri, _)
      | Ocsigen.Extensions.Req_not_found (_, ri) ->
          Lwt.return
            (if condition ri.Ocsigen.Extensions.request_info
             then (
               Logs.info ~src:section (fun fmt ->
                 fmt "COND: going into <then> branch");
               Ocsigen.Extensions.Ext_sub_result ithen)
             else (
               Logs.info ~src:section (fun fmt ->
                 fmt "COND: going into <else> branch, if any");
               Ocsigen.Extensions.Ext_sub_result ielse)))
  | Element (("if" as s), _, _) ->
      Ocsigen.Extensions.badconfig "Bad syntax for tag %s" s
  | Element ("notfound", [], []) ->
      fun _rs ->
        Logs.info ~src:section (fun fmt ->
          fmt "NOT_FOUND: taking in charge 404");
        Lwt.return
          (Ocsigen.Extensions.Ext_stop_all (Ocsigen_cookie_map.empty, `Not_found))
  | Element (("notfound" as s), _, _) ->
      Ocsigen.Extensions.badconfig "Bad syntax for tag %s" s
  | Element ("nextsite", [], []) -> (
      function
      | Ocsigen.Extensions.Req_found (_, r) ->
          Lwt.return
            (Ocsigen.Extensions.Ext_found_stop (fun () -> Lwt.return r))
      | Ocsigen.Extensions.Req_not_found _ ->
          Lwt.return
            (Ocsigen.Extensions.Ext_stop_site
               (Ocsigen_cookie_map.empty, `Not_found)))
  | Element ("nexthost", [], []) -> (
      function
      | Ocsigen.Extensions.Req_found (_, r) ->
          Lwt.return
            (Ocsigen.Extensions.Ext_found_stop (fun () -> Lwt.return r))
      | Ocsigen.Extensions.Req_not_found _ ->
          Lwt.return
            (Ocsigen.Extensions.Ext_stop_host
               (Ocsigen_cookie_map.empty, `Not_found)))
  | Element (("nextsite" as s), _, _) ->
      Ocsigen.Extensions.badconfig "Bad syntax for tag %s" s
  | Element ("stop", [], []) -> (
      function
      | Ocsigen.Extensions.Req_found (_, r) ->
          Lwt.return
            (Ocsigen.Extensions.Ext_found_stop (fun () -> Lwt.return r))
      | Ocsigen.Extensions.Req_not_found _ ->
          Lwt.return
            (Ocsigen.Extensions.Ext_stop_all
               (Ocsigen_cookie_map.empty, `Not_found)))
  | Element (("stop" as s), _, _) ->
      Ocsigen.Extensions.badconfig "Bad syntax for tag %s" s
  | Xml.Element ("forbidden", [], []) ->
      fun _rs ->
        Logs.info ~src:section (fun fmt ->
          fmt "FORBIDDEN: taking in charge 403");
        Lwt.return
          (Ocsigen.Extensions.Ext_stop_all (Ocsigen_cookie_map.empty, `Forbidden))
  | Element (("forbidden" as s), _, _) ->
      Ocsigen.Extensions.badconfig "Bad syntax for tag %s" s
  | Element ("iffound", [], sub) -> (
      let ext = parse_fun sub in
      function
      | Ocsigen.Extensions.Req_found (_, _) ->
          Lwt.return (Ocsigen.Extensions.Ext_sub_result ext)
      | Ocsigen.Extensions.Req_not_found (err, _ri) ->
          Lwt.return (Ocsigen.Extensions.Ext_next err))
  | Element (("iffound" as s), _, _) ->
      Ocsigen.Extensions.badconfig "Bad syntax for tag %s" s
  | Element ("ifnotfound", [], sub) -> (
      let ext = parse_fun sub in
      function
      | Ocsigen.Extensions.Req_found (_, r) ->
          Lwt.return (Ocsigen.Extensions.Ext_found (fun () -> Lwt.return r))
      | Ocsigen.Extensions.Req_not_found _ ->
          Lwt.return (Ocsigen.Extensions.Ext_sub_result ext))
  | Element ("ifnotfound", [("code", s)], sub) -> (
      let ext = parse_fun sub in
      let re = Netstring_pcre.regexp ("^" ^ s ^ "$") in
      function
      | Ocsigen.Extensions.Req_found (_, r) ->
          Lwt.return (Ocsigen.Extensions.Ext_found (fun () -> Lwt.return r))
      | Ocsigen.Extensions.Req_not_found (err, _ri) ->
          if
            let err =
              string_of_int Cohttp.Code.(code_of_status (err :> status_code))
            in
            Netstring_pcre.string_match re err 0 <> None
          then Lwt.return (Ocsigen.Extensions.Ext_sub_result ext)
          else Lwt.return (Ocsigen.Extensions.Ext_next err))
  | Element (("ifnotfound" as s), _, _) ->
      Ocsigen.Extensions.badconfig "Bad syntax for tag %s" s
  | Element ("allow-forward-for", param, _) ->
      let proxies =
        match param with
        | _ when List.mem_assoc "check-equal-ip" param ->
            Ocsigen.Extensions.badconfig
              "check-equal-ip is no longer supported in <allow-forward-for/>: see its proxies attribute"
        | [] -> []
        | [("proxies", s)] ->
            List.map
              (prefix ~where:"the proxies of <allow-forward-for/>")
              (Netstring_pcre.split blanks_regexp s)
        | _ ->
            Ocsigen.Extensions.badconfig
              "Bad syntax for argument of tag allow-forward-for"
      in
      allow_forward_for_handler ~proxies
  | Element ("allow-forward-proto", _, _) -> allow_forward_proto_handler
  | Element (t, _, _) ->
      raise (Ocsigen.Extensions.Bad_config_tag_for_extension t)
  | _ -> Ocsigen.Extensions.badconfig "(accesscontrol extension) Bad data"

(* Registration of the extension for the config file: *)
let () =
  Ocsigen.Extensions.register ~name:"accesscontrol"
    ~fun_site:(fun userconf _ _ _ _ parse_fun -> function
       (* A user must not make the server trust headers that any client can
         send. *)
       | Element ((("allow-forward-for" | "allow-forward-proto") as tag), _, _)
         when Option.is_some userconf ->
           raise
             (Ocsigen.Extensions.Error_in_user_config_file
                ("<" ^ tag ^ "/> is not allowed in user configuration files"))
       | element -> parse_config parse_fun element)
    ()

let if_ condition ithen ielse vh ci p = function
  | Ocsigen.Extensions.Req_found (ri, _)
  | Ocsigen.Extensions.Req_not_found (_, ri) ->
      Lwt.return
        (if condition ri.Ocsigen.Extensions.request_info
         then
           Ocsigen.Extensions.Ext_sub_result
             (Ocsigen.Extensions.compose (List.map (fun i -> i vh ci p) ithen))
         else
           Ocsigen.Extensions.Ext_sub_result
             (Ocsigen.Extensions.compose (List.map (fun i -> i vh ci p) ielse)))

let iffound instrs vh ci p = function
  | Ocsigen.Extensions.Req_found (_, _) ->
      Lwt.return
        (Ocsigen.Extensions.Ext_sub_result
           (Ocsigen.Extensions.compose (List.map (fun i -> i vh ci p) instrs)))
  | Ocsigen.Extensions.Req_not_found (err, _ri) ->
      Lwt.return (Ocsigen.Extensions.Ext_next err)

let ifnotfound ?code instrs vh ci p =
  let re = Option.map (fun s -> Netstring_pcre.regexp ("^" ^ s ^ "$")) code in
  function
  | Ocsigen.Extensions.Req_found (_, r) ->
      Lwt.return (Ocsigen.Extensions.Ext_found (fun () -> Lwt.return r))
  | Ocsigen.Extensions.Req_not_found (err, _) -> (
    match re with
    | None ->
        Lwt.return
          (Ocsigen.Extensions.Ext_sub_result
             (Ocsigen.Extensions.compose (List.map (fun i -> i vh ci p) instrs)))
    | Some re ->
        if
          let err =
            string_of_int Cohttp.Code.(code_of_status (err :> status_code))
          in
          Netstring_pcre.string_match re err 0 <> None
        then
          Lwt.return
            (Ocsigen.Extensions.Ext_sub_result
               (Ocsigen.Extensions.compose
                  (List.map (fun i -> i vh ci p) instrs)))
        else Lwt.return (Ocsigen.Extensions.Ext_next err))

let notfound _ _ _ _ =
  Lwt.return
    (Ocsigen.Extensions.Ext_stop_all (Ocsigen_cookie_map.empty, `Not_found))

let nextsite _ _ _ = function
  | Ocsigen.Extensions.Req_found (_, r) ->
      Lwt.return (Ocsigen.Extensions.Ext_found_stop (fun () -> Lwt.return r))
  | Ocsigen.Extensions.Req_not_found _ ->
      Lwt.return
        (Ocsigen.Extensions.Ext_stop_site (Ocsigen_cookie_map.empty, `Not_found))

let nexthost _ _ _ = function
  | Ocsigen.Extensions.Req_found (_, r) ->
      Lwt.return (Ocsigen.Extensions.Ext_found_stop (fun () -> Lwt.return r))
  | Ocsigen.Extensions.Req_not_found _ ->
      Lwt.return
        (Ocsigen.Extensions.Ext_stop_host (Ocsigen_cookie_map.empty, `Not_found))

let stop _ _ _ = function
  | Ocsigen.Extensions.Req_found (_, r) ->
      Lwt.return (Ocsigen.Extensions.Ext_found_stop (fun () -> Lwt.return r))
  | Ocsigen.Extensions.Req_not_found _ ->
      Lwt.return
        (Ocsigen.Extensions.Ext_stop_all (Ocsigen_cookie_map.empty, `Not_found))

let forbidden _ _ _ _ =
  Lwt.return
    (Ocsigen.Extensions.Ext_stop_all (Ocsigen_cookie_map.empty, `Forbidden))

let allow_forward_for ?(proxies = []) () =
  let proxies =
    List.map (prefix ~where:"the proxies of allow_forward_for") proxies
  in
  fun _ _ _ -> allow_forward_for_handler ~proxies

let allow_forward_proto () _ _ _ = allow_forward_proto_handler
