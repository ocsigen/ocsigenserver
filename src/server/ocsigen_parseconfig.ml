(* Ocsigen
 * http://www.ocsigen.org
 * Module ocsigen_parseconfig.ml
 * Copyright (C) 2005-2008 Vincent Balat, Nataliya Guts, St�phane Glondu
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

open Ocsigen_lib

open Simplexmlparser
open Ocsigen_config

let section = Lwt_log.Section.make "ocsigen:config"

let blah_of_string f tag s =
  try
    f (String.remove_spaces s 0 ((String.length s) -1))
  with Failure _ -> raise (Ocsigen_config.Config_file_error
                             ("While parsing <"^tag^"> - "^s^
                              " is not a valid value."))

let int_of_string = blah_of_string int_of_string
let float_of_string = blah_of_string float_of_string

(*****************************************************************************)
let default_default_hostname =
  let hostname = Unix.gethostname () in
  try
    (*VVV Is it ok? Is it reliable? *)
    (List.hd
       (Unix.getaddrinfo hostname "www"
          [Unix.AI_CANONNAME;
           Unix.AI_SOCKTYPE Unix.SOCK_STREAM])).Unix.ai_canonname
  with Failure _ ->
    Lwt_log.ign_warning_f ~section
      "Cannot determine default host name. Will use \"%s\" \
       to create absolute links or redirections dynamically \
       if you do not set <host defaulthostname=\"...\" ...> \
       in config file." hostname;
    (*VVV Is it the right behaviour? *)
    hostname
(*****************************************************************************)

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
    let s = String.remove_spaces s 0 (l-1) in
    let v l =
      try
        Int64.of_string (String.sub s 0 l)
      with Failure _ -> failwith "Ocsigen_parseconfig.parse_size"
    in
    let o l =
      let l1 = l-1 in
      if l1>0
      then
        let c1 = s.[l1] in
        if (c1 = 'o') || (c1 = 'B')
        then v l1
        else v l
      else v l
    in
    if (s = "") || (s = "infinity")
    then None
    else Some
        (let l = String.length s in
         let l1 = l-1 in
         if l1>0
         then
           let c1 = String.sub s l1 1 in
           if (c1 = "T")
           then Int64.mul tebi (v l1)
           else
           if (c1 = "G")
           then Int64.mul gibi (v l1)
           else
           if (c1 = "M")
           then Int64.mul mebi (v l1)
           else
           if (c1 = "k")
           then Int64.mul kibi (v l1)
           else
             let l2 = l-2 in
             if l2>0
             then
               let c2 = String.sub s l2 2 in
               if (c2 = "To") || (c2 = "TB")
               then Int64.mul tera (v l2)
               else
               if (c2 = "Go") || (c2 = "GB")
               then Int64.mul giga (v l2)
               else
               if (c2 = "Mo") || (c2 = "MB")
               then Int64.mul mega (v l2)
               else
               if (c2 = "ko") || (c2 = "kB")
               then Int64.mul kilo (v l2)
               else
                 let l3 = l-3 in
                 if l3>0
                 then
                   let c3 = String.sub s l3 3 in
                   if (c3 = "Tio") || (c3 = "TiB")
                   then Int64.mul tebi (v l3)
                   else
                   if (c3 = "Gio") || (c3 = "GiB")
                   then Int64.mul gibi (v l3)
                   else
                   if (c3 = "Mio") || (c3 = "MiB")
                   then Int64.mul mebi (v l3)
                   else
                   if (c3 = "kio") || (c3 = "kiB")
                   then Int64.mul kibi (v l3)
                   else o l
                 else o l
             else o l
         else o l)


let parse_size_tag tag s =
  try
    parse_size s
  with Failure _ ->
    raise
      (Ocsigen_config.Config_file_error
         ("While parsing <"^tag^"> - "^s^" is not a valid size."))






(* My xml parser is not really adapted to this.
   It is the parser for the syntax extension.
   But it works.
*)


let rec parse_string = function
  | [] -> ""
  | (PCData s)::l -> s^(parse_string l)
  | _ -> failwith "ocsigen_parseconfig.parse_string"

let parse_string_tag tag s =
  try
    parse_string s
  with Failure _ ->
    raise
      (Ocsigen_config.Config_file_error
         ("While parsing <"^tag^"> - String expected."))


let rec parser_config =
  let rec verify_empty = function
    | [] -> ()
    | _ -> raise (Config_file_error "Don't know what to do with trailing data")
  in let rec parse_servers n = function
    | [] -> (match n with
        | [] -> raise (Config_file_error ("<server> tag expected"))
        | _ -> n)
    | (Element ("server", [], nouveau))::ll ->
        (match ll with
        | [] -> ()
        | _ ->
          Lwt_log.ign_warning ~section
            "At most one <server> tag possible in config file. \
             Ignoring trailing data.");
        parse_servers (n@[nouveau]) [] (* ll *)
        (* Multiple server not supported any more *)
        (* nouveau at the end *)
    | _ -> raise (Config_file_error ("syntax error inside <ocsigen>"))
  in function
    | (Element ("ocsigen", [], l))::ll ->
      verify_empty ll;
      parse_servers [] l
    | _ -> raise (Config_file_error "<ocsigen> tag expected")


let parse_ext file =
  parser_config (Simplexmlparser.xmlparser_file file)


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
  (fun (hostfilter : string option) ->
     try Hashtbl.find h hostfilter
     with Not_found ->
       let r = match hostfilter with
         | None ->
           [Ocsigen_extensions.VirtualHost.make
              ~host:"*"
              ~pattern:(Netstring_pcre.regexp ".*$") ()] (* default = "*:*" *)
         | Some s ->
           let parse_one_host ss =
             let host, port =
               try
                 let dppos = String.index ss ':'
                 and len = String.length ss in
                 let host = String.sub ss 0 dppos
                 and port =
                   match String.sub ss (dppos+1) ((len - dppos) - 1) with
                   | "*" -> None
                   | p -> Some (int_of_string "host" p)
                 in host, port
               with
               | Not_found -> ss, None
               | Failure _ ->
                 raise (Config_file_error "bad port number")
             in
             let split_host = function
               | Netstring_str.Delim _ -> ".*"
               | Netstring_str.Text t -> Netstring_pcre.quote t
             in
             let pattern =
               Netstring_pcre.regexp
                 (String.concat ""
                    ((List.map split_host
                        (Netstring_str.full_split
                           (Netstring_str.regexp "[*]+") host))@["$"]))
             in
             Ocsigen_extensions.VirtualHost.make
               ~host
               ~pattern
               ?port ()
           in
           List.map parse_one_host
             (Netstring_str.split (Netstring_str.regexp "[ \t]+") s)
       in
       Hashtbl.add h hostfilter r;
       (r : Ocsigen_extensions.virtual_hosts)
  )


(* Extract a default hostname from the "host" field if no default is provided *)
let get_defaulthostname ~defaulthostname ~defaulthttpport ~host =
  match defaulthostname with
  | Some d -> d
  | None ->
    (* We look for a hostname without wildcard (second case) *)
    (* Something more clever could be envisioned *)
    let rec aux = function
      | [] -> default_default_hostname
      | x :: _ when
        String.contains (Ocsigen_extensions.VirtualHost.host x) '*' = false
        && ((Ocsigen_extensions.VirtualHost.port x) = Some 80
            || (Ocsigen_extensions.VirtualHost.port x) = None) ->
        Ocsigen_extensions.VirtualHost.host x
      | _ :: q -> aux q
    in
    let host = aux host in
    Lwt_log.ign_warning_f ~section
      "While parsing config file, tag <host>: No defaulthostname, \
       assuming it is \"%s\"" host;
    if correct_hostname host then
       host
    else
      raise (Ocsigen_config.Config_file_error
               ("Incorrect hostname " ^ host))


(* Config file is parsed twice.
   This is the second parsing (site loading)
*)
let parse_server isreloading c =
  let rec parse_server_aux = function
      | [] -> []
      | (Element ("port", atts, p))::ll ->
          parse_server_aux ll
      | (Element ("charset" as st, atts, p))::ll ->
          set_default_charset (Some (parse_string_tag st p));
          parse_server_aux ll
      | (Element ("logdir", [], p))::ll ->
          parse_server_aux ll
      | (Element ("syslog", [], p))::ll ->
          parse_server_aux ll
      | (Element ("ssl", [], p))::ll ->
          parse_server_aux ll
      | (Element ("user", [], p))::ll ->
          parse_server_aux ll
      | (Element ("group", [], p))::ll ->
          parse_server_aux ll
      | (Element ("uploaddir" as st, [], p))::ll ->
          set_uploaddir (Some (parse_string_tag st p));
          parse_server_aux ll
      | (Element ("datadir" as st, [], p))::ll ->
          set_datadir (parse_string_tag st p);
          parse_server_aux ll
      | (Element ("minthreads" as st, [], p))::ll ->
          set_minthreads (int_of_string st (parse_string_tag st p));
          parse_server_aux ll
      | (Element ("maxthreads" as st, [], p))::ll ->
          set_maxthreads (int_of_string st (parse_string_tag st p));
          parse_server_aux ll
      | (Element ("maxdetachedcomputationsqueued" as st, [], p))::ll ->
          set_max_number_of_threads_queued (int_of_string st (parse_string_tag st p));
          parse_server_aux ll
      | (Element ("maxconnected" as st, [], p))::ll ->
          set_max_number_of_connections (int_of_string st (parse_string_tag st p));
          parse_server_aux ll
      | (Element ("mimefile" as st, [], p))::ll ->
          Ocsigen_config.set_mimefile (parse_string_tag st p);
          parse_server_aux ll
      | (Element ("maxretries" as st, [], p))::ll ->
          set_maxretries (int_of_string st (parse_string_tag st p));
          parse_server_aux ll
      | (Element ("timeout" as st, [], p))::ll
(*VVV timeout: backward compatibility with <= 0.99.4 *)
      | (Element ("clienttimeout" as st, [], p))::ll ->
          set_client_timeout (int_of_string st (parse_string_tag st p));
          parse_server_aux ll
      | (Element ("servertimeout" as st, [], p))::ll ->
          set_server_timeout (int_of_string st (parse_string_tag st p));
          parse_server_aux ll
(*VVV For now we use silentservertimeout and silentclienttimeout also
  for keep alive :-(
      | (Element ("keepalivetimeout" as st, [], p))::ll ->
          set_keepalive_timeout (int_of_string st (parse_string_tag st p));
          parse_server_aux ll
      | (Element ("keepopentimeout" as st, [], p))::ll ->
          set_keepopen_timeout (int_of_string st (parse_string_tag st p));
          parse_server_aux ll
*)
      | (Element ("netbuffersize" as st, [], p))::ll ->
          set_netbuffersize (int_of_string st (parse_string_tag st p));
          parse_server_aux ll
      | (Element ("filebuffersize" as st, [], p))::ll ->
          set_filebuffersize (int_of_string st (parse_string_tag st p));
          parse_server_aux ll
      | (Element ("maxrequestbodysize" as st, [], p))::ll ->
          set_maxrequestbodysize (parse_size_tag st (parse_string_tag st p));
          parse_server_aux ll
      | (Element ("maxuploadfilesize" as st, [], p))::ll ->
          set_maxuploadfilesize (parse_size_tag st
                                   (parse_string_tag st p));
          parse_server_aux ll
      | (Element ("commandpipe" as st, [], p))::ll ->
          set_command_pipe (parse_string_tag st p);
          parse_server_aux ll
      | (Element ("shutdowntimeout" as st, [], p))::ll ->
          let p = parse_string_tag st p in
          let t =
            if p = "notimeout"
            then None
            else Some (float_of_string st p)
          in
          set_shutdown_timeout t;
          parse_server_aux ll
      | (Element ("debugmode", [], []))::ll ->
          set_debugmode true;
          parse_server_aux ll
      | (Element ("usedefaulthostname", [], []))::ll ->
          set_usedefaulthostname true;
          parse_server_aux ll
      | (Element ("disablepartialrequests", [], []))::ll ->
          set_disablepartialrequests true;
          parse_server_aux ll
      | (Element ("respectpipeline", [], []))::ll ->
          set_respect_pipeline ();
          parse_server_aux ll
      | (Element ("findlib", ["path",p], []))::ll ->
          Ocsigen_loader.add_ocamlpath p;
          parse_server_aux ll
      | (Element ("require", atts, l))::ll
      | (Element ("extension", atts, l))::ll ->
          (* We do not reload extensions *)
          let modules = match atts with
            | [] ->
                raise
                  (Config_file_error "missing module, name or findlib-package attribute in <extension>")
            | [("name", s)] -> `Name s
            | [("module", s)] -> `Files [s]
            | [("findlib-package", s)] -> `Files (Ocsigen_loader.findfiles s)
            | _ ->
                raise (Config_file_error "Wrong attribute for <extension>")
          in begin
            match modules with
              | `Files modules ->
                  Ocsigen_loader.loadfiles
                    (preloadfile l) postloadfile false modules;
              | `Name name ->
                  Ocsigen_loader.init_module
                    (preloadfile l) postloadfile false name
          end;
          parse_server_aux ll
      | (Element ("library", atts, l))::ll ->
          let modules = match atts with
            | [] ->
                raise
                  (Config_file_error "missing module or findlib-package attribute in <library>")
            | [("name", s)] -> `Name s
            | [("module", s)] -> `Files [s]
            | [("findlib-package", s)] -> `Files (Ocsigen_loader.findfiles s)
            | _ -> raise (Config_file_error "Wrong attribute for <library>")
          in begin
            match modules with
              | `Files modules ->
                  Ocsigen_loader.loadfiles (preloadfile l) postloadfile true modules;
              | `Name name ->
                  Ocsigen_loader.init_module (preloadfile l) postloadfile true name
          end;
          parse_server_aux ll
      | (Element ("host", atts, l))::ll ->
          let rec parse_attrs ((name,
                                charset,
                                defaulthostname,
                                defaulthttpport,
                                defaulthttpsport,
                                ishttps) as r) = function
            | [] -> r
            | ("hostfilter", s)::suite ->
                (match name with
                | None -> parse_attrs ((Some s), charset,
                                       defaulthostname,
                                       defaulthttpport,
                                       defaulthttpsport,
                                       ishttps) suite
                | _ -> raise (Ocsigen_config.Config_file_error
                                ("Duplicate attribute name in <host>")))
            | ("charset", s)::suite ->
                (match charset with
                | None -> parse_attrs (name, Some s,
                                       defaulthostname,
                                       defaulthttpport,
                                       defaulthttpsport,
                                       ishttps) suite
                | _ -> raise (Ocsigen_config.Config_file_error
                                ("Duplicate attribute charset in <host>")))
            | ("defaulthostname", s)::suite ->
                (match defaulthostname with
                | None ->
                    if correct_hostname s then
                      parse_attrs (name, charset,
                                   (Some s),
                                   defaulthttpport,
                                   defaulthttpsport,
                                   ishttps) suite
                    else
                      raise (Ocsigen_config.Config_file_error
                               ("Incorrect hostname " ^ s))
                | _ -> raise (Ocsigen_config.Config_file_error
                                ("Duplicate attribute defaulthostname in <host>")))
            | ("defaulthttpport", s)::suite ->
                (match defaulthttpport with
                | None -> parse_attrs (name, charset,
                                       defaulthostname,
                                       (Some s),
                                       defaulthttpsport,
                                       ishttps) suite
                | _ -> raise (Ocsigen_config.Config_file_error
                                ("Duplicate attribute defaulthttpport in <host>")))
            | ("defaulthttpsport", s)::suite ->
                (match defaulthttpsport with
                | None -> parse_attrs (name, charset,
                                       defaulthostname,
                                       defaulthttpport,
                                       Some s,
                                       ishttps) suite
                | _ -> raise (Ocsigen_config.Config_file_error
                                ("Duplicate attribute defaulthttpsport in <host>")))
            | ("defaultprotocol", s)::suite ->
                (match ishttps with
                | None -> parse_attrs (name, charset,
                                       defaulthostname,
                                       defaulthttpport,
                                       defaulthttpsport,
                                       Some s) suite
                | _ -> raise (Ocsigen_config.Config_file_error
                                ("Duplicate attribute defaultprotocol in <host>")))
            | (s, _)::_ ->
                raise (Ocsigen_config.Config_file_error
                         ("Wrong attribute for <host>: "^s))
          in
          let host, charset, defaulthostname, defaulthttpport,
            defaulthttpsport, defaultprotocol =
            parse_attrs (None, None, None, None, None, None) atts
          in
          let host = parse_host_field host in
          let charset =
            match charset, Ocsigen_config.get_default_charset () with
              | Some charset, _
              | None, Some charset -> charset
              | None, None -> "utf-8"
          in
          let defaulthttpport = match defaulthttpport with
            | None -> Ocsigen_config.get_default_port ()
            | Some p -> int_of_string "host" p
          in
          let defaulthostname = get_defaulthostname
            ~defaulthostname ~defaulthttpport ~host in
          let defaulthttpsport = match defaulthttpsport with
            | None -> Ocsigen_config.get_default_sslport ()
            | Some p -> int_of_string "host" p
          in
          let serve_everything = {
            Ocsigen_extensions.do_not_serve_regexps = [];
            do_not_serve_files = [];
            do_not_serve_extensions = [];
          } in
          let conf = {
            Ocsigen_extensions.default_hostname = defaulthostname;
            default_httpport = defaulthttpport;
            default_httpsport = defaulthttpsport;
            default_protocol_is_https = defaultprotocol = Some "https";
            mime_assoc = Ocsigen_charset_mime.default_mime_assoc ();
            charset_assoc = Ocsigen_charset_mime.empty_charset_assoc
              ~default:charset ();
            default_directory_index = ["index.html"];
            list_directory_content = false;
            follow_symlinks = Ocsigen_extensions.FollowSymlinksIfOwnerMatch;
            do_not_serve_404 = serve_everything;
            do_not_serve_403 = serve_everything;
            uploaddir = Ocsigen_config.get_uploaddir ();
            maxuploadfilesize = Ocsigen_config.get_maxuploadfilesize ();
          }
          in
          let parse_host = Ocsigen_extensions.parse_config_item host conf in
          let parse_config =
            Ocsigen_extensions.make_parse_config [] parse_host
          in
          (* default site for host *)
          (host, conf, parse_config l)::(parse_server_aux ll)
      | (Element ("extconf", [("dir", dir)], []))::ll ->
          let one =
            try
              let files = Sys.readdir dir in
              Array.sort compare files;
              Array.fold_left
                (fun l s ->
                  if Filename.check_suffix s "conf" then
                    let filename = dir^"/"^s in
                    let filecont =
                      try
                        Lwt_log.ign_info_f ~section "Parsing configuration file %s" filename;
                        parse_ext filename
                      with e ->
                        Lwt_log.ign_error_f ~section ~exn:e
                          "Error while loading configuration file %s (ignored)" filename;
                        []
                    in
                    (match filecont with
                    | [] -> l
                    | s::_ -> l@(parse_server_aux s)
                    )
                  else l
                )
                []
                files
            with
            | Sys_error _ as e ->
              Lwt_log.ign_error ~section ~exn:e
                "Error while loading configuration file (ignored)";
              []
          in
          one@(parse_server_aux ll)
      | (Element (tag, _, _))::_ ->
          raise (Config_file_error
                   ("tag <"^tag^"> unexpected inside <server>"))
      | _ ->
          raise (Config_file_error "Syntax error")
  in Ocsigen_extensions.set_hosts (parse_server_aux c)


(* Types of socket declarable in configuration file *)
type socket_type = Ocsigen_socket.socket_type =
  | All
  | IPv4 of Unix.inet_addr
  | IPv6 of Unix.inet_addr

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
    | Some r -> IPv6 (Unix.inet6_addr_any), int_of_string "port" (get r 1)
    | None -> match do_match all_ipv4 with
      | Some r -> IPv4 (Unix.inet_addr_any), int_of_string "port" (get r 1)
      | None -> match do_match single_ipv6 with
        | Some r -> IPv6 (Unix.inet_addr_of_string (get r 1)),
                    int_of_string "port" (get r 2)
        | None -> match do_match single_ipv4 with
          | Some r -> IPv4 (Unix.inet_addr_of_string (get r 1)),
                      int_of_string "port" (get r 2)
          | None -> All, int_of_string "port" s

let parse_facility = function
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

(* First parsing of config file *)
let extract_info c =
  let rec parse_ssl certificate privatekey = function
      [] -> Some (certificate,privatekey)
    | (Element ("certificate" as st, [], p))::l ->
      (match certificate with
         None ->
         parse_ssl (Some (parse_string_tag st p)) privatekey l
       | _ -> raise (Config_file_error
                       "Two certificates inside <ssl>"))
    | (Element ("privatekey" as st, [], p))::l ->
      (match privatekey with
         None ->
         parse_ssl certificate (Some (parse_string_tag st p)) l
       | _ -> raise (Config_file_error
                       "Two private keys inside <ssl>"))
    | (Element (tag,_,_))::l ->
      raise (Config_file_error ("<"^tag^"> tag unexpected inside <ssl>"))
    | _ -> raise (Config_file_error ("Unexpected content inside <ssl>"))
  in
  let rec aux user group ssl ports sslports minthreads maxthreads = function
      [] -> ((user, group), (ssl, ports,sslports), (minthreads, maxthreads))
    | (Element ("logdir" as st, [], p))::ll ->
      set_logdir (parse_string_tag st p);
      aux user group ssl ports sslports minthreads maxthreads ll
    | (Element ("syslog" as st, [], p))::ll ->
      let str = String.lowercase (parse_string_tag st p) in
      set_syslog_facility (Some (parse_facility str));
      aux user group ssl ports sslports minthreads maxthreads ll
    | (Element ("port" as st, atts, p))::ll ->
      (match atts with
         []
       | [("protocol", "HTTP")] ->
         let po = try
             parse_port (parse_string_tag st p)
           with Failure _ ->
             raise (Config_file_error "Wrong value for <port> tag")
         in aux user group ssl (po::ports) sslports minthreads maxthreads ll
       | [("protocol", "HTTPS")] ->
         let po = try
             parse_port (parse_string_tag st p)
           with Failure _ ->
             raise (Config_file_error "Wrong value for <port> tag")
         in
         aux user group ssl ports (po::sslports) minthreads maxthreads ll
       | _ -> raise (Config_file_error "Wrong attribute for <port>"))
    | (Element ("minthreads" as st, [], p))::ll ->
      aux user group ssl ports sslports
        (Some (int_of_string st (parse_string_tag st p))) maxthreads ll
    | (Element ("maxthreads" as st, [], p))::ll ->
      aux user group ssl ports sslports minthreads
        (Some (int_of_string st (parse_string_tag st p))) ll
    | (Element ("ssl", [], p))::ll ->
      (match ssl with
         None ->
         aux user group (parse_ssl None None p) ports sslports
           minthreads maxthreads ll
       | _ ->
         raise
           (Config_file_error
              "Only one ssl certificate for each server supported for now"))
    | (Element ("user" as st, [], p))::ll ->
      (match user with
         None ->
         aux (Some (parse_string_tag st p)) group ssl ports sslports
           minthreads maxthreads ll
       | _ -> raise (Config_file_error
                       "Only one <user> tag for each server allowed"))
    | (Element ("group" as st, [], p))::ll ->
      (match group with
         None ->
         aux user (Some (parse_string_tag st p)) ssl ports sslports
           minthreads maxthreads ll
       | _ -> raise (Config_file_error
                       "Only one <group> tag for each server allowed"))
    | (Element ("commandpipe" as st, [], p))::ll ->
      set_command_pipe (parse_string_tag st p);
      aux user group ssl ports sslports minthreads maxthreads ll
    | (Element (tag, _, _))::ll ->
      aux user group ssl ports sslports minthreads maxthreads ll
    | _ ->
      raise (Config_file_error "Syntax error")
  in
  let (user, group),
      (ssl, ports, ssl_ports),
      (mint, maxt) =
      aux None None None [] [] None None c in
  let user = match user with
      None -> None (* Some (get_default_user ()) *)
    | Some s -> if s = "" then None else Some s
  in
  let group = match group with
      None -> None (* Some (get_default_group ()) *)
    | Some s -> if s = "" then None else Some s
  in
  let mint = match mint with
    | Some t -> t
    | None -> get_minthreads ()
  in
  let maxt = match maxt with
    | Some t -> t
    | None -> get_maxthreads ()
  in
  let ssl_context = match ssl with
    | Some (Some a, Some b) -> Some (a, b)
    | _ -> None
  in
  Ocsigen_server_configuration.make
    ?user ?group ?ssl_context ~threads:(mint, maxt) ports ssl_ports

let parse_config ?file () =
  let file =
    match file with
    | None -> Ocsigen_config.get_config_file ()
    | Some f -> f
  in
  parser_config (Simplexmlparser.xmlparser_file file)

(******************************************************************)
