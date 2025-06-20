(* Ocsigen
 * http://www.ocsigen.org
 * Module ocsigen_parseconfig.ml
 * Copyright (C) 2005-2008 Vincent Balat, Nataliya Guts, Stéphane Glondu
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

(******************************************************************)
(** Config file parsing *)

open Xml
open Ocsigen_config
module Netstring_pcre = Ocsigen_lib.Netstring_pcre

let section = Logs.Src.create "ocsigen:config"

let blah_of_string f tag s =
  try f (Ocsigen_lib.String.remove_spaces s 0 (String.length s - 1))
  with Failure _ ->
    raise
      (Ocsigen_config.Config_file_error
         ("While parsing <" ^ tag ^ "> - " ^ s ^ " is not a valid value."))

let int_of_string = blah_of_string int_of_string
let float_of_string = blah_of_string float_of_string

let default_default_hostname =
  let hostname = Unix.gethostname () in
  try
    (*VVV Is it ok? Is it reliable? *)
    (List.hd
       (Unix.getaddrinfo hostname "www"
          [Unix.AI_CANONNAME; Unix.AI_SOCKTYPE Unix.SOCK_STREAM]))
      .Unix.ai_canonname
  with Failure _ ->
    Logs.warn ~src:section (fun fmt ->
      fmt
        "Cannot determine default host name. Will use \"%s\" to create absolute links or redirections dynamically if you do not set <host defaulthostname=\"...\" ...> in config file."
        hostname);
    (*VVV Is it the right behaviour? *)
    hostname

let parse_size =
  let kilo = Int64.of_int 1000 in
  let mega = Int64.of_int 1000000 in
  let giga = Int64.mul kilo mega in
  let tera = Int64.mul mega mega in
  let kibi = Int64.of_int 1024 in
  let mebi = Int64.of_int 1048576 in
  let gibi = Int64.mul kibi mebi in
  let tebi = Int64.mul mebi mebi in
  fun s ->
    let l = String.length s in
    let s = Ocsigen_lib.String.remove_spaces s 0 (l - 1) in
    let v l =
      try Int64.of_string (String.sub s 0 l)
      with Failure _ -> failwith "Ocsigen_parseconfig.parse_size"
    in
    let o l =
      let l1 = l - 1 in
      if l1 > 0
      then
        let c1 = s.[l1] in
        if c1 = 'o' || c1 = 'B' then v l1 else v l
      else v l
    in
    if s = "" || s = "infinity"
    then None
    else
      Some
        (let l = String.length s in
         let l1 = l - 1 in
         if l1 > 0
         then
           let c1 = String.sub s l1 1 in
           if c1 = "T"
           then Int64.mul tebi (v l1)
           else if c1 = "G"
           then Int64.mul gibi (v l1)
           else if c1 = "M"
           then Int64.mul mebi (v l1)
           else if c1 = "k"
           then Int64.mul kibi (v l1)
           else
             let l2 = l - 2 in
             if l2 > 0
             then
               let c2 = String.sub s l2 2 in
               if c2 = "To" || c2 = "TB"
               then Int64.mul tera (v l2)
               else if c2 = "Go" || c2 = "GB"
               then Int64.mul giga (v l2)
               else if c2 = "Mo" || c2 = "MB"
               then Int64.mul mega (v l2)
               else if c2 = "ko" || c2 = "kB"
               then Int64.mul kilo (v l2)
               else
                 let l3 = l - 3 in
                 if l3 > 0
                 then
                   let c3 = String.sub s l3 3 in
                   if c3 = "Tio" || c3 = "TiB"
                   then Int64.mul tebi (v l3)
                   else if c3 = "Gio" || c3 = "GiB"
                   then Int64.mul gibi (v l3)
                   else if c3 = "Mio" || c3 = "MiB"
                   then Int64.mul mebi (v l3)
                   else if c3 = "kio" || c3 = "kiB"
                   then Int64.mul kibi (v l3)
                   else o l
                 else o l
             else o l
         else o l)

let parse_size_tag tag s =
  try parse_size s
  with Failure _ ->
    raise
      (Ocsigen_config.Config_file_error
         ("While parsing <" ^ tag ^ "> - " ^ s ^ " is not a valid size."))

let rec parse_string = function
  | [] -> ""
  | PCData s :: l -> s ^ parse_string l
  | _ -> failwith "ocsigen_parseconfig.parse_string"

let parse_string_tag tag s =
  try parse_string s
  with Failure _ ->
    raise
      (Ocsigen_config.Config_file_error
         ("While parsing <" ^ tag ^ "> - String expected."))

let parser_config =
  let rec parse_servers n = function
    | [] -> (
      match n with
      | [] -> raise (Config_file_error "<server> tag expected")
      | _ -> n)
    | Element ("server", [], nouveau) :: ll ->
        (match ll with
        | [] -> ()
        | _ ->
            Logs.warn ~src:section (fun fmt ->
              fmt
                "At most one <server> tag possible in config file. Ignoring trailing data."));
        parse_servers (n @ [nouveau]) []
        (* ll *)
        (* Multiple server not supported any more *)
        (* nouveau at the end *)
    | _ -> raise (Config_file_error "syntax error inside <ocsigen>")
  in
  function
  | Element ("ocsigen", [], l) -> parse_servers [] l
  | _ -> raise (Config_file_error "<ocsigen> tag expected")

let parse_ext file = parser_config (Xml.parse_file file)
let preloadfile config () = Ocsigen_extensions.set_config config
let postloadfile () = Ocsigen_extensions.set_config []

(* Checking hostnames.  We make only make looze efforts.
   See RFC 921 and 952 for further details *)
let correct_hostname =
  let regexp = Netstring_pcre.regexp "^[a-zA-Z0-9]+((\\.|-)[a-zA-Z0-9]+)*$" in
  fun h -> Netstring_pcre.string_match regexp h 0 <> None

(* Splits the [host] field, first according to spaces
   (which encode disjunction),
   and then according to wildcards '*' ; we then transform the hosts-with-regexp
   into a regepx that matches a potential host.
   The whole result is cached because user config files (for userconf)
   are read at each request. *)
let parse_host_field =
  let h = Hashtbl.create 17 in
  fun (hostfilter : string option) ->
    try Hashtbl.find h hostfilter
    with Not_found ->
      let r =
        match hostfilter with
        | None -> ["*", Netstring_pcre.regexp ".*$", None] (* default = "*:*" *)
        | Some s ->
            let parse_one_host ss =
              let host, port =
                try
                  let dppos = String.index ss ':' and len = String.length ss in
                  let host = String.sub ss 0 dppos
                  and port =
                    match String.sub ss (dppos + 1) (len - dppos - 1) with
                    | "*" -> None
                    | p -> Some (int_of_string "host" p)
                  in
                  host, port
                with
                | Not_found -> ss, None
                | Failure _ -> raise (Config_file_error "bad port number")
              in
              let split_host = function
                | Str.Delim _ -> ".*"
                | Str.Text t -> Re.Pcre.quote t
              in
              ( host
              , Netstring_pcre.regexp
                  (String.concat ""
                     (List.map split_host
                        (Str.full_split (Str.regexp "[*]+") host)
                     @ ["$"]))
              , port )
            in
            List.map parse_one_host (Str.split (Str.regexp "[ \t]+") s)
      in
      Hashtbl.add h hostfilter r;
      (r : Ocsigen_extensions.virtual_hosts)

(* Extract a default hostname from the "host" field if no default is
   provided *)
let get_defaulthostname ~defaulthostname ~host =
  match defaulthostname with
  | Some d -> d
  | None ->
      (* We look for a hostname without wildcard (second case).
       Something more clever could be envisioned *)
      let rec aux = function
        | [] -> default_default_hostname
        | (t, _, (Some 80 | None)) :: _ when not (String.contains t '*') -> t
        | _ :: q -> aux q
      in
      let host = aux host in
      Logs.warn ~src:section (fun fmt ->
        fmt
          "While parsing config file, tag <host>: No defaulthostname, assuming it is \"%s\""
          host);
      if correct_hostname host
      then host
      else
        raise (Ocsigen_config.Config_file_error ("Incorrect hostname " ^ host))

let later_pass_host_attr
      ( name
      , charset
      , defaulthostname
      , defaulthttpport
      , defaulthttpsport
      , ishttps )
  = function
  | "hostfilter", s -> (
    match name with
    | None ->
        ( Some s
        , charset
        , defaulthostname
        , defaulthttpport
        , defaulthttpsport
        , ishttps )
    | _ ->
        raise
          (Ocsigen_config.Config_file_error "Duplicate attribute name in <host>")
    )
  | "charset", s -> (
    match charset with
    | None ->
        ( name
        , Some s
        , defaulthostname
        , defaulthttpport
        , defaulthttpsport
        , ishttps )
    | _ ->
        raise
          (Ocsigen_config.Config_file_error
             "Duplicate attribute charset in <host>"))
  | "defaulthostname", s -> (
    match defaulthostname with
    | None ->
        if correct_hostname s
        then name, charset, Some s, defaulthttpport, defaulthttpsport, ishttps
        else
          raise (Ocsigen_config.Config_file_error ("Incorrect hostname " ^ s))
    | _ ->
        raise
          (Ocsigen_config.Config_file_error
             "Duplicate attribute defaulthostname in <host>"))
  | "defaulthttpport", s -> (
    match defaulthttpport with
    | None -> name, charset, defaulthostname, Some s, defaulthttpsport, ishttps
    | _ ->
        raise
          (Ocsigen_config.Config_file_error
             "Duplicate attribute defaulthttpport in <host>"))
  | "defaulthttpsport", s -> (
    match defaulthttpsport with
    | None -> name, charset, defaulthostname, defaulthttpport, Some s, ishttps
    | _ ->
        raise
          (Ocsigen_config.Config_file_error
             "Duplicate attribute defaulthttpsport in <host>"))
  | "defaultprotocol", s -> (
    match ishttps with
    | None ->
        ( name
        , charset
        , defaulthostname
        , defaulthttpport
        , defaulthttpsport
        , Some s )
    | _ ->
        raise
          (Ocsigen_config.Config_file_error
             "Duplicate attribute defaultprotocol in <host>"))
  | attr, _ ->
      raise
        (Ocsigen_config.Config_file_error ("Wrong attribute for <host>: " ^ attr))

let later_pass_host attrs l =
  let ( host
      , charset
      , defaulthostname
      , defaulthttpport
      , defaulthttpsport
      , defaultprotocol )
    =
    List.fold_left later_pass_host_attr
      (None, None, None, None, None, None)
      (List.rev attrs)
  in
  let host = parse_host_field host in
  let charset =
    match charset, Ocsigen_config.get_default_charset () with
    | Some charset, _ | None, Some charset -> charset
    | None, None -> "utf-8"
  and defaulthttpport =
    match defaulthttpport with
    | None -> Ocsigen_config.get_default_port ()
    | Some p -> int_of_string "host" p
  and defaulthostname = get_defaulthostname ~defaulthostname ~host
  and defaulthttpsport =
    match defaulthttpsport with
    | None -> Ocsigen_config.get_default_sslport ()
    | Some p -> int_of_string "host" p
  and serve_everything =
    { Ocsigen_extensions.do_not_serve_regexps = []
    ; do_not_serve_files = []
    ; do_not_serve_extensions = [] }
  in
  let conf =
    { Ocsigen_extensions.default_hostname = defaulthostname
    ; default_httpport = defaulthttpport
    ; default_httpsport = defaulthttpsport
    ; default_protocol_is_https = defaultprotocol = Some "https"
    ; mime_assoc = Ocsigen_charset_mime.default_mime_assoc ()
    ; charset_assoc =
        Ocsigen_charset_mime.empty_charset_assoc ~default:charset ()
    ; default_directory_index = ["index.html"]
    ; list_directory_content = false
    ; follow_symlinks = `Owner_match
    ; do_not_serve_404 = serve_everything
    ; do_not_serve_403 = serve_everything
    ; uploaddir = Ocsigen_config.get_uploaddir ()
    ; maxuploadfilesize = Ocsigen_config.get_maxuploadfilesize () }
  in
  let parse_config =
    Ocsigen_extensions.make_parse_config []
      (Ocsigen_extensions.parse_config_item None host conf)
  in
  (* default site for host *)
  host, conf, parse_config l

let later_pass_extension tag attrs l =
  (* We do not reload extensions *)
  match attrs with
  | [] ->
      raise
        (Config_file_error
           ("missing module, name or findlib-package attribute in " ^ tag))
  | [("name", s)] ->
      Ocsigen_loader.init_module (preloadfile l) postloadfile false s
  | [("module", s)] ->
      Ocsigen_loader.loadfiles (preloadfile l) postloadfile false [s]
  | [("findlib-package", s)] ->
      Ocsigen_loader.loadfiles (preloadfile l) postloadfile false
        (Ocsigen_loader.findfiles s)
  | _ -> raise (Config_file_error ("Wrong attribute for " ^ tag))

let rec later_pass_extconf dir =
  let f acc s =
    if Filename.check_suffix s "conf"
    then
      match
        let filename = dir ^ "/" ^ s in
        try
          Logs.info ~src:section (fun fmt ->
            fmt "Parsing configuration file %s" filename);
          parse_ext filename
        with e ->
          Logs.err ~src:section (fun fmt ->
            fmt
              ("Error while loading configuration file %s (ignored)" ^^ "@\n%s")
              filename (Printexc.to_string e));
          []
      with
      | [] -> acc
      | s :: _ -> acc @ later_pass s
    else acc
  in
  try
    let files = Sys.readdir dir in
    Array.sort compare files; Array.fold_left f [] files
  with Sys_error _ as e ->
    Logs.err ~src:section (fun fmt ->
      fmt
        ("Error while loading configuration file (ignored)" ^^ "@\n%s")
        (Printexc.to_string e));
    []

(* Config file is parsed twice. This is the second parsing (site
   loading). *)
and later_pass = function
  | [] -> []
  | Element ("port", _atts, _p) :: ll -> later_pass ll
  | Element (("charset" as st), _atts, p) :: ll ->
      set_default_charset (Some (parse_string_tag st p));
      later_pass ll
  | Element ("logdir", [], _p) :: ll -> later_pass ll
  | Element ("syslog", [], _p) :: ll -> later_pass ll
  | Element ("ssl", [], _p) :: ll -> later_pass ll
  | Element ("user", [], _p) :: ll -> later_pass ll
  | Element ("group", [], _p) :: ll -> later_pass ll
  | Element (("uploaddir" as st), [], p) :: ll ->
      set_uploaddir (Some (parse_string_tag st p));
      later_pass ll
  | Element (("datadir" as st), [], p) :: ll ->
      set_datadir (parse_string_tag st p);
      later_pass ll
  | Element ("minthreads", [], _p) :: ll -> later_pass ll
  | Element ("maxthreads", [], _p) :: ll -> later_pass ll
  | Element (("maxconnected" as st), [], p) :: ll ->
      set_max_number_of_connections (int_of_string st (parse_string_tag st p));
      later_pass ll
  | Element (("mimefile" as st), [], p) :: ll ->
      Ocsigen_config.set_mimefile (parse_string_tag st p);
      later_pass ll
  | Element (("maxretries" as st), [], p) :: ll ->
      set_maxretries (int_of_string st (parse_string_tag st p));
      later_pass ll
  | Element (("timeout" as st), [], p) :: ll
  | Element (("clienttimeout" as st), [], p) :: ll ->
      set_client_timeout (int_of_string st (parse_string_tag st p));
      later_pass ll
  | Element (("servertimeout" as st), [], p) :: ll ->
      set_server_timeout (int_of_string st (parse_string_tag st p));
      later_pass ll
  | Element (("netbuffersize" as st), [], p) :: ll ->
      Ocsigen_stream.set_net_buffer_size
        (int_of_string st (parse_string_tag st p));
      later_pass ll
  | Element (("filebuffersize" as st), [], p) :: ll ->
      set_filebuffersize (int_of_string st (parse_string_tag st p));
      later_pass ll
  | Element (("maxrequestbodysize" as st), [], p) :: ll ->
      set_maxrequestbodysize (parse_size_tag st (parse_string_tag st p));
      later_pass ll
  | Element (("maxuploadfilesize" as st), [], p) :: ll ->
      set_maxuploadfilesize (parse_size_tag st (parse_string_tag st p));
      later_pass ll
  | Element (("commandpipe" as st), [], p) :: ll ->
      set_command_pipe (parse_string_tag st p);
      later_pass ll
  | Element (("shutdowntimeout" as st), [], p) :: ll ->
      set_shutdown_timeout
        (match parse_string_tag st p with
        | "notimeout" -> None
        | p -> Some (float_of_string st p));
      later_pass ll
  | Element ("debugmode", [], []) :: ll -> set_debugmode true; later_pass ll
  | Element ("usedefaulthostname", [], []) :: ll ->
      set_usedefaulthostname true;
      later_pass ll
  | Element ("disablepartialrequests", [], []) :: ll ->
      set_disablepartialrequests true;
      later_pass ll
  | Element ("respectpipeline", [], []) :: ll ->
      set_respect_pipeline (); later_pass ll
  | Element ("findlib", [("path", p)], []) :: ll ->
      Ocsigen_loader.add_ocamlpath p;
      later_pass ll
  | Element ("require", atts, l) :: ll | Element ("extension", atts, l) :: ll ->
      later_pass_extension "<extension>" atts l;
      later_pass ll
  | Element ("library", atts, l) :: ll ->
      later_pass_extension "<library>" atts l;
      later_pass ll
  | Element ("host", atts, l) :: ll ->
      (* The evaluation order is important here *)
      let h = later_pass_host atts l in
      h :: later_pass ll
  | Element ("extconf", [("dir", dir)], []) :: ll ->
      (* The evaluation order is important here *)
      let h = later_pass_extconf dir in
      h @ later_pass ll
  | Element (tag, _, _) :: _ ->
      raise (Config_file_error ("tag <" ^ tag ^ "> unexpected inside <server>"))
  | _ -> raise (Config_file_error "Syntax error")

let later_pass l = Ocsigen_extensions.set_hosts (later_pass l)

(* Parsing <port> tags *)
let parse_port =
  let all_ipv6 = Netstring_pcre.regexp "^\\[::\\]:([0-9]+)$" in
  let all_ipv4 = Netstring_pcre.regexp "^\\*:([0-9]+)$" in
  let single_ipv6 = Netstring_pcre.regexp "^\\[([0-9A-Fa-f.:]+)\\]:([0-9]+)$" in
  let single_ipv4 = Netstring_pcre.regexp "^([0-9.]+):([0-9]+)$" in
  fun s ->
    let do_match r = Netstring_pcre.string_match r s 0 in
    let get x i = Netstring_pcre.matched_group x i s in
    match do_match all_ipv6 with
    | Some r -> `IPv6 Unix.inet6_addr_any, int_of_string "port" (get r 1)
    | None -> (
      match do_match all_ipv4 with
      | Some r -> `IPv4 Unix.inet_addr_any, int_of_string "port" (get r 1)
      | None -> (
        match do_match single_ipv6 with
        | Some r ->
            ( `IPv6 (Unix.inet_addr_of_string (get r 1))
            , int_of_string "port" (get r 2) )
        | None -> (
          match do_match single_ipv4 with
          | Some r ->
              ( `IPv4 (Unix.inet_addr_of_string (get r 1))
              , int_of_string "port" (get r 2) )
          | None -> `All, int_of_string "port" s)))

let parse_lwt_log_facility = function
  | "auth" -> `Auth
  | "authpriv" -> `Authpriv
  | "console" -> `Console
  | "cron" -> `Cron
  | "daemon" -> `Daemon
  | "ftp" -> `FTP
  | "kernel" -> `Kernel
  | "lpr" -> `LPR
  | "local0" -> `Local0
  | "local1" -> `Local1
  | "local2" -> `Local2
  | "local3" -> `Local3
  | "local4" -> `Local4
  | "local5" -> `Local5
  | "local6" -> `Local6
  | "local7" -> `Local7
  | "mail" -> `Mail
  | "ntp" -> `NTP
  | "news" -> `News
  | "security" -> `Security
  | "syslog" -> `Syslog
  | "uucp" -> `UUCP
  | "user" -> `User
  | t -> raise (Config_file_error ("Unknown " ^ t ^ " facility in <syslog>"))

let parse_facility s =
  (* Translating from [Lwt_log] facility type to [Syslog_message]. *)
  let facility_code = function
    | `Kernel -> 0
    | `User -> 1
    | `Mail -> 2
    | `Daemon -> 3
    | `Auth -> 4
    | `Syslog -> 5
    | `LPR -> 6
    | `News -> 7
    | `UUCP -> 8
    | `Cron -> 9
    | `Authpriv -> 10
    | `FTP -> 11
    | `NTP -> 12
    | `Security -> 13
    | `Console -> 14
    | `Local0 -> 16
    | `Local1 -> 17
    | `Local2 -> 18
    | `Local3 -> 19
    | `Local4 -> 20
    | `Local5 -> 21
    | `Local6 -> 22
    | `Local7 -> 23
  in
  match
    Syslog_message.facility_of_int (facility_code (parse_lwt_log_facility s))
  with
  | Some s -> s
  | None -> raise (Config_file_error ("Unknown " ^ s ^ " facility in <syslog>"))

(* First parsing of config file *)

let config_error_for_some s = function
  | None -> ()
  | _ -> raise (Config_file_error s)

let make_ssl_info ~certificate ~privatekey ~ciphers ~dhfile ~curve =
  { Ocsigen_config.ssl_certificate = certificate
  ; ssl_privatekey = privatekey
  ; ssl_ciphers = ciphers
  ; ssl_dhfile = dhfile
  ; ssl_curve = curve }

let rec parse_ssl l ~certificate ~privatekey ~ciphers ~dhfile ~curve =
  match l with
  | [] -> Some (make_ssl_info ~certificate ~privatekey ~ciphers ~dhfile ~curve)
  | Element (("certificate" as st), [], p) :: l ->
      config_error_for_some "Two certificates inside <ssl>" certificate;
      let certificate = Some (parse_string_tag st p) in
      parse_ssl ~certificate ~privatekey ~ciphers ~dhfile ~curve l
  | Element (("privatekey" as st), [], p) :: l ->
      config_error_for_some "Two private keys inside <ssl>" privatekey;
      let privatekey = Some (parse_string_tag st p) in
      parse_ssl ~certificate ~privatekey ~ciphers ~dhfile ~curve l
  | Element (("ciphers" as st), [], p) :: l ->
      config_error_for_some "Two cipher strings inside <ssl>" ciphers;
      let ciphers = Some (parse_string_tag st p) in
      parse_ssl ~certificate ~privatekey ~ciphers ~dhfile ~curve l
  | Element (("dhfile" as st), [], p) :: l ->
      config_error_for_some "Two DH files inside <ssl>" dhfile;
      let dhfile = Some (parse_string_tag st p) in
      parse_ssl ~certificate ~privatekey ~ciphers ~dhfile ~curve l
  | Element (("curve" as st), [], p) :: l ->
      config_error_for_some "Two (EC) curves inside <ssl>" curve;
      let curve = Some (parse_string_tag st p) in
      parse_ssl ~certificate ~privatekey ~ciphers ~dhfile ~curve l
  | Element (tag, _, _) :: _l ->
      raise (Config_file_error ("<" ^ tag ^ "> tag unexpected inside <ssl>"))
  | _ -> raise (Config_file_error "Unexpected content inside <ssl>")

let first_pass c =
  let rec aux ssl ports sslports = function
    | [] -> ssl, ports, sslports
    | Element (("logdir" as st), [], p) :: ll ->
        set_logdir (parse_string_tag st p);
        aux ssl ports sslports ll
    | Element (("syslog" as st), [], p) :: ll ->
        let str = String.lowercase_ascii (parse_string_tag st p) in
        set_syslog_facility (Some (parse_facility str));
        aux ssl ports sslports ll
    | Element (("port" as st), atts, p) :: ll -> (
      match atts with
      | [] | [("protocol", "HTTP")] ->
          let po =
            try parse_port (parse_string_tag st p)
            with Failure _ ->
              raise (Config_file_error "Wrong value for <port> tag")
          in
          aux ssl (po :: ports) sslports ll
      | [("protocol", "HTTPS")] ->
          let po =
            try parse_port (parse_string_tag st p)
            with Failure _ ->
              raise (Config_file_error "Wrong value for <port> tag")
          in
          aux ssl ports (po :: sslports) ll
      | _ -> raise (Config_file_error "Wrong attribute for <port>"))
    | Element (("minthreads" as st), [], p) :: ll ->
        set_minthreads (int_of_string st (parse_string_tag st p));
        aux ssl ports sslports ll
    | Element (("maxthreads" as st), [], p) :: ll ->
        set_maxthreads (int_of_string st (parse_string_tag st p));
        aux ssl ports sslports ll
    | Element ("ssl", [], p) :: ll -> (
      match ssl with
      | None ->
          let ssl =
            let certificate = None
            and privatekey = None
            and ciphers = None
            and dhfile = None
            and curve = None in
            parse_ssl ~certificate ~privatekey ~ciphers ~dhfile ~curve p
          in
          aux ssl ports sslports ll
      | _ ->
          raise
            (Config_file_error
               "Only one ssl certificate for each server supported for now"))
    | Element ("user", [], _) :: ll | Element ("group", [], _) :: ll ->
        Logs.warn ~src:section (fun fmt ->
          fmt
            "Config file: <user> and <group> deprecated. Please do not launch as root.");
        aux ssl ports sslports ll
    | Element (("commandpipe" as st), [], p) :: ll ->
        set_command_pipe (parse_string_tag st p);
        aux ssl ports sslports ll
    | Element _ :: ll -> aux ssl ports sslports ll
    | _ -> raise (Config_file_error "Syntax error")
  in
  let si, ports, ssl_ports = aux None [] [] c in
  Ocsigen_config.set_ssl_info si;
  Ocsigen_config.set_ports ports;
  Ocsigen_config.set_ssl_ports ssl_ports;
  ()

let parse_config ?file () =
  let file =
    match file with None -> Ocsigen_config.get_config_file () | Some f -> f
  in
  parser_config (Xml.parse_file file)
