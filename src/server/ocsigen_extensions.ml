(* Ocsigen
 * http://www.ocsigen.org
 * Module ocsigen_extensions.ml
 * Copyright (C) 2005 2007 Vincent Balat
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
(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)

(** Writing extensions for Ocsigen                                           *)

(* TODO

   - awake must be called after each Ext_found or Ext_found_continue_with
   or Ext_found_stop sent by an extension. It is perhaps called too often.

*)

let section = Lwt_log.Section.make "ocsigen:ext"

open Lwt
open Ocsigen_lib
open Ocsigen_cookies

include Ocsigen_request_info
include Ocsigen_command

module Ocsigen_request_info = Ocsigen_request_info

exception Ocsigen_http_error of (Ocsigen_cookies.cookieset * int)
exception Ocsigen_Looping_request


(** Xml tag not recognized by an extension (usually not a real error) *)
exception Bad_config_tag_for_extension of string

(** Error in a <site> tag inside the main ocsigen.conf file *)
exception Error_in_config_file of string

(** Option incorrect in a userconf file *)
exception Error_in_user_config_file of string


let badconfig fmt = Printf.ksprintf (fun s -> raise (Error_in_config_file s)) fmt

(*****************************************************************************)
(* virtual hosts: *)
type virtual_hosts = (string * Netstring_pcre.regexp * int option) list

(* We cannot use generic comparison, as regexpes are abstract values that
   cannot be compared or hashed. However the string essentially contains
   the value that is compiled into a regexp, and we compare this instead *)

let hash_virtual_hosts (l : virtual_hosts) =
  Hashtbl.hash (List.map (fun (s, _, p) -> (s, p)) l)

let rec equal_virtual_hosts (l1 : virtual_hosts) (l2 : virtual_hosts) =
  match l1, l2 with
  | [], [] -> true
  | [], _::_ | _::_, [] -> false
  | (s1, _, p1) :: q1, (s2, _, p2) :: q2 ->
    s1 = s2 && p1 = p2 && equal_virtual_hosts q1 q2

(*****************************************************************************)

type client = Ocsigen_http_com.connection

let client_id = Ocsigen_http_com.connection_id
let client_connection x = x
let client_of_connection x = x


(*****************************************************************************)

(* Server configuration, for local files that must not be sent *)

type do_not_serve = {
  do_not_serve_regexps: string list;
  do_not_serve_files: string list;
  do_not_serve_extensions: string list;
}

(* BY TODO : Use unbalanced trees instead *)
let join_do_not_serve d1 d2 = {
  do_not_serve_regexps = d1.do_not_serve_regexps @ d2.do_not_serve_regexps;
  do_not_serve_files = d1.do_not_serve_files @ d2.do_not_serve_files;
  do_not_serve_extensions = d1.do_not_serve_extensions @ d2.do_not_serve_extensions;
}

let hash_consed_do_not_serve = Hashtbl.create 17

exception IncorrectRegexpes of do_not_serve

let do_not_serve_to_regexp d =
  try Hashtbl.find hash_consed_do_not_serve d
  with Not_found ->
    let wrap l = if l = [] then None else Some l
    and bind f = function None -> None | Some v -> Some (f v)
    in
    let files, extensions, regexps =
      wrap d.do_not_serve_files,
      wrap d.do_not_serve_extensions,
      wrap d.do_not_serve_regexps
    in
    let paren_quote l =
      String.concat "|" (List.map (fun s -> Printf.sprintf "(%s)"
                                      (Netstring_pcre.quote s)) l)
    and paren l =
      String.concat "|" (List.map (fun s -> Printf.sprintf "(%s)"  s) l)
    in
    let files = bind paren_quote files
    and extensions = bind paren_quote extensions
    and regexps = bind paren regexps
    in
    let files = bind (Printf.sprintf ".*/(%s)") files
    and extensions = bind (Printf.sprintf ".*\\.(%s)") extensions
    in
    let l = List.fold_left (fun r -> function None -> r | Some v -> v :: r)
        [] [files;extensions;regexps]
    in
    let regexp =
      if l = [] then
        (* This regexp should not never match *) "$^"
      else
        Printf.sprintf "^(%s)$" (paren l)
    in
    (try
       Lwt_log.ign_info_f ~section "Compiling exclusion regexp %s" regexp;
       let r = Netstring_pcre.regexp regexp in
       Hashtbl.add hash_consed_do_not_serve d r;
       r
     with _ -> raise (IncorrectRegexpes d)
    )


(*****************************************************************************)

(* Main server configuration *)


type config_info = {
  default_hostname: string;
  default_httpport: int;
  default_httpsport: int;
  default_protocol_is_https: bool;

  mime_assoc: Ocsigen_charset_mime.mime_assoc;

  charset_assoc : Ocsigen_charset_mime.charset_assoc;

  (** Default name to use as index file when a directory is requested.
      Use [None] if no index should be tried. The various indexes
      are tried in the given order. If no index is specified,
      or the index does not exists, the content of the directory
      might be listed, according to [list_directry_content] *)
  default_directory_index : string list;

  (** Should the list of files in a directory be displayed
      if there is no index in this directory ? *)
  list_directory_content : bool;

  (** Should symlinks be followed when accessign a local file? *)
  follow_symlinks: follow_symlink;

  do_not_serve_404: do_not_serve;
  do_not_serve_403: do_not_serve;

  uploaddir: string option;
  maxuploadfilesize: int64 option;
}
and follow_symlink =
  | DoNotFollowSymlinks (** Never follow a symlink *)
  | FollowSymlinksIfOwnerMatch (** Follow a symlink if the symlink and its
                                   target have the same owner *)
  | AlwaysFollowSymlinks (** Always follow symlinks *)



(* Requests *)
type request = {
  request_info: request_info;
  request_config: config_info;
}

exception Ocsigen_Is_a_directory
  of (Ocsigen_request_info.request_info -> Neturl.url)

type answer =
  | Ext_do_nothing
  (** I don't want to do anything *)
  | Ext_found of (unit -> Ocsigen_http_frame.result Lwt.t)
  (** "OK stop! I will take the page.
      You can start the following request of the same pipelined connection.
      Here is the function to generate the page".
      The extension must return Ext_found as soon as possible
      when it is sure it is safe to start next request.
      Usually immediately. But in some case, for example proxies,
      you don't want the request of one connection to be handled in
      different order. (for example revproxy.ml starts its requests
      to another server before returning Ext_found, to ensure that all
      requests are done in same order).
  *)
  | Ext_found_stop of (unit -> Ocsigen_http_frame.result Lwt.t)
  (** Found but do not try next extensions *)
  | Ext_next of int (** Page not found. Try next extension.
                        The integer is the HTTP error code.
                        It is usally 404, but may be for ex 403 (forbidden)
                        if you want another extension to try after a 403.
                        Same as Ext_continue_with but does not change
                        the request.
                    *)
  | Ext_stop_site of (Ocsigen_cookies.cookieset * int)
  (** Error. Do not try next extension, but
      try next site.
      The integer is the HTTP error code, usally 403.
  *)
  | Ext_stop_host of (Ocsigen_cookies.cookieset * int)
  (** Error. Do not try next extension,
      do not try next site,
      but try next host.
      The integer is the HTTP error code, usally 403.
  *)
  | Ext_stop_all of (Ocsigen_cookies.cookieset * int)
  (** Error. Do not try next extension,
      do not try next site,
      do not try next host.
      The integer is the HTTP error code, usally 403.
  *)
  | Ext_continue_with of (request * Ocsigen_cookies.cookieset * int)
  (** Used to modify the request before giving it to next extension.
      The extension returns the request (possibly modified)
      and a set of cookies if it wants to set or cookies
      ({!Ocsigen_cookies.Cookies.empty} for no cookies).
      You must add these cookies yourself in request if you
      want them to be seen by subsequent extensions,
      for example using {!Ocsigen_http_frame.compute_new_ri_cookies}.
      The integer is usually equal to the error code received
      from preceding extension (but you may want to modify it).
  *)
  | Ext_retry_with of request * Ocsigen_cookies.cookieset
  (** Used to retry all the extensions with a new request.
      The extension returns the request (possibly modified)
      and a set of cookies if it wants to set or cookies
      ({!Ocsigen_cookies.Cookies.empty} for no cookies).
      You must add these cookies yourself in request if you
      want them to be seen by subsequent extensions,
      for example using {!Ocsigen_http_frame.compute_new_ri_cookies}.
  *)
  | Ext_sub_result of extension2
  (** Used if your extension want to define option that may contain
      other options from other extensions.
      In that case, while parsing the configuration file, call
      the parsing function (of type [parse_fun]),
      that will return something of type [extension2].
  *)
  | Ext_found_continue_with of
      (unit -> (Ocsigen_http_frame.result * request) Lwt.t)
  (** Same as [Ext_found] but may modify the request. *)
  | Ext_found_continue_with' of (Ocsigen_http_frame.result * request)
  (** Same as [Ext_found_continue_with] but does not allow to delay
      the computation of the page. You should probably not use it,
      but for output filters.
  *)

and request_state =
  | Req_not_found of (int * request)
  | Req_found of (request * Ocsigen_http_frame.result)

and extension2 =
  (unit -> unit) ->
  Ocsigen_cookies.cookieset ->
  request_state ->
  (answer * Ocsigen_cookies.cookieset) Lwt.t


type extension = request_state -> answer Lwt.t


type parse_fun = Simplexmlparser.xml list -> extension2


type parse_host =
    Parse_host of
      (Url.path ->
       parse_host -> parse_fun -> Simplexmlparser.xml -> extension)

let (hosts : (virtual_hosts * config_info * extension2) list ref) =
  ref []



let set_hosts v = hosts := v
let get_hosts () = !hosts


(* Default hostname is either the Host header or the hostname set in
   the configuration file. *)
let get_hostname req =
  if Ocsigen_config.get_usedefaulthostname ()
  then req.request_config.default_hostname
  else match Ocsigen_request_info.host req.request_info with
    | None -> req.request_config.default_hostname
    | Some host -> host

(* Default port is either
   - the port the server is listening at
   - or the port in the Host header
   - or the default port set in the configuration file. *)
let get_port req =
  if Ocsigen_config.get_usedefaulthostname ()
  then (if Ocsigen_request_info.ssl req.request_info
        then req.request_config.default_httpsport
        else req.request_config.default_httpport)
  else match Ocsigen_request_info.port_from_host_field req.request_info with
    | Some p -> p
    | None ->
      match Ocsigen_request_info.host req.request_info with
      | Some _ -> if Ocsigen_request_info.ssl req.request_info then 443 else 80
      | None -> Ocsigen_request_info.server_port req.request_info


let http_url_syntax = Hashtbl.find Neturl.common_url_syntax "http"

let new_url_of_directory_request request ri =
  Lwt_log.ign_info ~section "Sending 301 Moved permanently";
  let port = get_port request in
  let ssl = Ocsigen_request_info.ssl ri in
  let new_url = Neturl.make_url
      ~scheme:(if ssl then "https" else "http")
      ~host:(get_hostname request)
      ?port:(if (port = 80 && not ssl)
             || (ssl && port = 443)
             then None
             else Some port)
      ~path:(""::(Url.add_end_slash_if_missing
                    (Ocsigen_request_info.full_path ri)))
      ?query:(Ocsigen_request_info.get_params_string ri)
      http_url_syntax
  in new_url



(*****************************************************************************)
(* To give parameters to extensions: *)
let dynlinkconfig = ref ([] : Simplexmlparser.xml list)
let set_config s = dynlinkconfig := s
let get_config () = !dynlinkconfig


(*****************************************************************************)
let site_match request (site_path : string list) url =
  (* We are sure that there is no / at the end or beginning of site_path *)
  (* and no / at the beginning of url *)
  (* and no // or ../ inside both of them *)
  (* We return the subpath without / at beginning *)
  let rec aux site_path url =
    match site_path, url with
    | [], [] ->
      raise (Ocsigen_Is_a_directory
               (new_url_of_directory_request request))
    | [], p -> Some p
    | a::l, aa::ll when a = aa -> aux l ll
    | _ -> None
  in
  match site_path, url with
  | [], [] -> Some []
  | _ -> aux site_path url




let add_to_res_cookies res cookies_to_set =
  if cookies_to_set = Ocsigen_cookies.Cookies.empty then
    res
  else
    (Ocsigen_http_frame.Result.update res
       ~cookies:
         (Ocsigen_cookies.add_cookies
            (Ocsigen_http_frame.Result.cookies res) cookies_to_set) ())

let make_ext awake cookies_to_set req_state (genfun : extension) (genfun2 : extension2) =
  genfun req_state
  >>= fun res ->
  let rec aux cookies_to_set = function
    | Ext_do_nothing -> genfun2 awake cookies_to_set req_state
    | Ext_found r ->
      awake ();
      r () >>= fun r' ->
      let ri = match req_state with
        | Req_found (ri, _) -> ri
        | Req_not_found (_, ri) -> ri
      in
      genfun2
        id (* already awoken *)
        Ocsigen_cookies.Cookies.empty
        (Req_found (ri, add_to_res_cookies r' cookies_to_set))
    | Ext_found_continue_with r ->
      awake ();
      r () >>= fun (r', req) ->
      genfun2
        id (* already awoken *)
        Ocsigen_cookies.Cookies.empty
        (Req_found (req, add_to_res_cookies r' cookies_to_set))
    | Ext_found_continue_with' (r', req) ->
      awake ();
      genfun2
        id (* already awoken *)
        Ocsigen_cookies.Cookies.empty
        (Req_found (req, add_to_res_cookies r' cookies_to_set))
    | Ext_next e ->
      let ri = match req_state with
        | Req_found (ri, _) -> ri
        | Req_not_found (_, ri) -> ri
      in
      genfun2 awake cookies_to_set (Req_not_found (e, ri))
    | Ext_continue_with (ri, cook, e) ->
      genfun2
        awake
        (Ocsigen_cookies.add_cookies cook cookies_to_set)
        (Req_not_found (e, ri))
    | Ext_found_stop _
    | Ext_stop_site _
    | Ext_stop_host _
    | Ext_stop_all _
    | Ext_retry_with _ as res ->
      Lwt.return (res, cookies_to_set)
    | Ext_sub_result sr ->
      sr awake cookies_to_set req_state
      >>= fun (res, cookies_to_set) ->
      aux cookies_to_set res
  in
  aux cookies_to_set res


(*****************************************************************************)
let fun_beg = ref (fun () -> ())
let fun_end = ref (fun () -> ())
let fun_exn = ref (fun exn -> (raise exn : string))

let rec default_parse_config
    (host : virtual_hosts)
    config_info
    prevpath
    (Parse_host parse_host)
    (parse_fun : parse_fun) = function
  | Simplexmlparser.Element ("site", atts, l) ->
    let rec parse_site_attrs (enc,dir) = function
      | [] -> (match dir with
          | None ->
            raise (Ocsigen_config.Config_file_error
                     ("Missing dir attribute in <site>"))
          | Some s -> (enc, s))
        | ("path", s)::suite
        | ("dir", s)::suite ->
            (match dir with
            | None -> parse_site_attrs (enc, Some s) suite
            | _ -> raise (Ocsigen_config.Config_file_error
                            ("Duplicate attribute dir in <site>")))
        | ("charset", s)::suite ->
            (match enc with
            | None -> parse_site_attrs ((Some s), dir) suite
            | _ -> raise (Ocsigen_config.Config_file_error
                            ("Duplicate attribute charset in <site>")))
        | (s, _)::_ ->
            raise
              (Ocsigen_config.Config_file_error ("Wrong attribute for <site>: "^s))
      in
      let charset, dir = parse_site_attrs (None, None) atts in
      let path =
        prevpath@
        Url.remove_slash_at_end
          (Url.remove_slash_at_beginning
             (Url.remove_dotdot (Neturl.split_path dir)))
      in
      let parse_config = make_parse_config path parse_host l in
      let ext awake cookies_to_set =
        function
          | Req_found (ri, res) ->
              Lwt.return (Ext_found_continue_with' (res, ri), cookies_to_set)
          | Req_not_found (e, oldri) ->
              let oldri = match charset with
                | None -> oldri
                | Some charset ->
                    { oldri with request_config =
                        { oldri.request_config with charset_assoc =
                            Ocsigen_charset_mime.set_default_charset
                              oldri.request_config.charset_assoc charset } }
              in
              match site_match oldri path (Ocsigen_request_info.full_path oldri.request_info) with
              | None ->
                Lwt_log.ign_info_f ~section
                  "site \"%a\" does not match url \"%a\"."
                  (fun () path  -> Url.string_of_url_path ~encode:true path) path
                  (fun () oldri -> Url.string_of_url_path ~encode:true
                      (Ocsigen_request_info.full_path oldri.request_info)) oldri;
                Lwt.return (Ext_next e, cookies_to_set)
              | Some sub_path ->
                Lwt_log.ign_info_f ~section
                  "site found: url \"%a\" matches \"%a\"."
                  (fun () oldri -> Url.string_of_url_path ~encode:true
                      (Ocsigen_request_info.full_path oldri.request_info)) oldri
                  (fun () path -> Url.string_of_url_path ~encode:true path) path;
                let ri = {oldri with
                              request_info =
                                (Ocsigen_request_info.update oldri.request_info
                                    ~sub_path:sub_path
                                    ~sub_path_string:
                                    (Url.string_of_url_path
                                      ~encode:true sub_path) ()) }
                  in
                  parse_config awake cookies_to_set (Req_not_found (e, ri))
                  >>= function
                      (* After a site, we turn back to old ri *)
                    | (Ext_stop_site (cs, err), cookies_to_set)
                    | (Ext_continue_with (_, cs, err), cookies_to_set) ->
                        Lwt.return
                          (Ext_continue_with (oldri, cs, err), cookies_to_set)
                    | (Ext_found_continue_with r, cookies_to_set) ->
                        awake ();
                        r () >>= fun (r', req) ->
                        Lwt.return
                          (Ext_found_continue_with' (r', oldri), cookies_to_set)
                    | (Ext_found_continue_with' (r, req), cookies_to_set) ->
                        Lwt.return
                          (Ext_found_continue_with' (r, oldri), cookies_to_set)
                    | (Ext_do_nothing, cookies_to_set) ->
                        Lwt.return
                          (Ext_continue_with (oldri,
                                              Ocsigen_cookies.Cookies.empty,
                                              e), cookies_to_set)
                    | r -> Lwt.return r
      in
      (function
        | Req_found (ri, r) ->
            Lwt.return (Ext_found_continue_with' (r, ri))
        | Req_not_found (err, ri) ->
            Lwt.return (Ext_sub_result ext))
  | Simplexmlparser.Element (tag,_,_) ->
    raise (Bad_config_tag_for_extension tag)
  | _ -> raise (Ocsigen_config.Config_file_error
                  ("Unexpected content inside <host>"))

and make_parse_config path parse_host l : extension2 =
  let f = parse_host path (Parse_host parse_host) in
  (* creates all site data, if any *)
  let rec parse_config : _ -> extension2 = function
    | [] ->
      (fun (awake : unit -> unit) cookies_to_set -> function
         | Req_found (ri, res) ->
           Lwt.return (Ext_found_continue_with' (res, ri), cookies_to_set)
         | Req_not_found (e, ri) ->
           Lwt.return
             (Ext_continue_with
                (ri, Ocsigen_cookies.Cookies.empty, e), cookies_to_set))
    (* was Lwt.return (Ext_next e, cookies_to_set))
       but to use make_parse_site with userconf,
       we need to know current ri after parsing the sub-configuration.
    *)
    | xmltag::ll ->
      try
        let genfun = f parse_config xmltag in
        let genfun2 = parse_config ll in
        fun awake cookies_to_set req_state ->
          make_ext awake cookies_to_set req_state genfun genfun2
      with
      | Bad_config_tag_for_extension t ->
        (* This case happens only if no extension has recognized the
           tag at all *)
        badconfig
          "Unexpected tag <%s> inside <site dir=\"%s\">" t
          (Url.string_of_url_path ~encode:true path)
      | Error_in_config_file _ as e -> raise e
      | e ->
        badconfig
          "Error while parsing configuration file: %s"
          (try !fun_exn e
           with e -> Printexc.to_string e)
  in
  !fun_beg ();
  let r =
    try
      parse_config l
    with e -> !fun_end (); raise e
    (*VVV May be we should avoid calling fun_end after parinf user config files
      (with extension userconf) ... See eliommod.ml
    *)
  in
  !fun_end ();
  r

(*****************************************************************************)

type userconf_info = {
  localfiles_root : string;
}

type parse_config = virtual_hosts -> config_info -> parse_config_aux
and parse_config_user = userconf_info -> parse_config
and parse_config_aux =
    Url.path -> parse_host ->
    (parse_fun -> Simplexmlparser.xml ->
     extension
    )



let user_extension_void_fun_site : parse_config_user =
  fun _ _ _ _ _ _ -> function
    | Simplexmlparser.Element (t, _, _) -> raise (Bad_config_tag_for_extension t)
    | _ -> raise (Error_in_config_file "Unexpected data in config file")

let extension_void_fun_site : parse_config = fun _ _ _ _ _ -> function
  | Simplexmlparser.Element (t, _, _) -> raise (Bad_config_tag_for_extension t)
  | _ -> raise (Error_in_config_file "Unexpected data in config file")


let register_extension, parse_config_item, parse_user_site_item, get_beg_init, get_end_init, get_init_exn_handler =
  let ref_fun_site = ref default_parse_config in
  let ref_user_fun_site = ref (fun (_ : userconf_info) -> default_parse_config) in

  ((* ********* register_extension ********* *)
    (fun
      ?fun_site
      ?user_fun_site
      ?begin_init
      ?end_init
      ?(exn_handler=raise)
      ?(respect_pipeline=false)
      ()
      ->

        if respect_pipeline then Ocsigen_config.set_respect_pipeline ();

        (match fun_site with
         | None -> ()
         | Some fun_site ->
           let old_fun_site = !ref_fun_site in
           ref_fun_site :=
             (fun host conf_info ->
                let oldf = old_fun_site host conf_info in
                let newf = fun_site host conf_info in
                fun path parse_host ->
                  let oldf = oldf path parse_host in
                  let newf = newf path parse_host in
                  fun parse_config config_tag ->
                    try
                      oldf parse_config config_tag
                    with
                    | Bad_config_tag_for_extension c ->
                      newf parse_config config_tag
             ));

        (match user_fun_site with
         | None -> ()
         | Some user_fun_site ->
           let old_fun_site = !ref_user_fun_site in
           ref_user_fun_site :=
             (fun path host conf_info ->
                let oldf = old_fun_site path host conf_info in
                let newf = user_fun_site path host conf_info in
                fun path parse_host ->
                  let oldf = oldf path parse_host in
                  let newf = newf path parse_host in
                  fun parse_config config_tag ->
                    try
                      oldf parse_config config_tag
                    with
                    | Bad_config_tag_for_extension c ->
                      newf parse_config config_tag
             ));


        (match begin_init with
         | Some begin_init -> fun_beg := comp begin_init !fun_beg
         | None -> ());
        (match end_init with
         | Some end_init -> fun_end := comp end_init !fun_end;
         | None -> ());
        let curexnfun = !fun_exn in
        fun_exn := fun e -> try curexnfun e with e -> exn_handler e),


    (* ********* parse_config_item ********* *)
    (fun host conf -> !ref_fun_site host conf),

    (* ********* parse_user_site_item ********* *)
    (fun host conf -> !ref_user_fun_site host conf),

    (* ********* get_beg_init ********* *)
    (fun () -> !fun_beg),

    (* ********* get_end_init ********* *)
    (fun () -> !fun_end),

    (* ********* get_init_exn_handler ********* *)
    (fun () -> !fun_exn)
  )

let default_parse_extension ext_name = function
  | [] -> ()
  | _ -> raise (Error_in_config_file
                  (Printf.sprintf "Unexpected content found in configuration of extension %s: %s does not accept options" ext_name ext_name))

let register_extension
    ~name
    ?fun_site
    ?user_fun_site
    ?begin_init
    ?end_init
    ?init_fun
    ?exn_handler
    ?respect_pipeline
    () =
  Ocsigen_loader.set_module_init_function name
    (fun () ->
       (match init_fun with
        | None -> default_parse_extension name (get_config ())
        | Some f -> f (get_config ()));
       register_extension ?fun_site ?user_fun_site ?begin_init ?end_init
         ?exn_handler ?respect_pipeline ())

module Configuration = struct

  type attribute' = {
    attribute_obligatory : bool;
    attribute_value_func : string -> unit
  }
  type attribute = string * attribute'

  type element' = {
    obligatory : bool;
    init : unit -> unit;
    elements : element list;
    attributes : attribute list;
    pcdata : (string -> unit) option;
    other_elements : (string -> (string * string) list -> Simplexmlparser.xml list -> unit) option;
    other_attributes : (string -> string -> unit) option;
  }
  and element = string * element'

  let element ~name
      ?(obligatory=false)
      ?(init=ignore)
      ?(elements=[])
      ?(attributes=[])
      ?pcdata
      ?other_elements
      ?other_attributes
      () : element =
    name, { obligatory; init; elements; attributes; pcdata; other_elements; other_attributes }

  let attribute ~name ?(obligatory=false) f : attribute =
    name, { attribute_obligatory = obligatory; attribute_value_func = f }

  let ignore_blank_pcdata ~in_tag =
    fun str ->
      String.iter
        (fun c ->
           if not (List.mem c [' '; '\n'; '\r'; '\t']) then
             raise (Error_in_user_config_file
                      ("Non-blank PCDATA in tag "^in_tag)))
        str

  let refuse_pcdata ~in_tag =
    fun _ ->
      raise (Error_in_user_config_file
               ("No PCDATA allowed in tag "^in_tag))

  let check_attribute_occurrence ~in_tag ?other_elements attributes = function
    | name, { attribute_obligatory = true } ->
      (try ignore (List.assoc name attributes)
       with Not_found ->
         raise (Error_in_user_config_file
                  ("Obligatory attribute "^name^" not in tag "^in_tag)))
    | _ -> ()

  let check_element_occurrence ~in_tag elements = function
    | name, { obligatory = true } ->
      let corresponding_element = function
        | Simplexmlparser.Element (name', _, _) -> name = name'
        | _ -> false
      in
      if not (List.exists corresponding_element elements) then
        raise (Error_in_user_config_file
                 ("Obligatory element "^name^" not in tag "^in_tag))
    | _ -> ()

  let process_attribute =
    fun ~in_tag ~attributes:spec_attributes ?other_attributes:spec_other_attributes (attribute, value) ->
      try
        (List.assoc attribute spec_attributes).attribute_value_func value
      with Not_found ->
        match spec_other_attributes with
        | Some spec_other_attributes ->
          spec_other_attributes attribute value
        | None ->
          raise (Error_in_user_config_file
                   ("Unexpected attribute "^attribute^" in tag "^in_tag))

  let rec process_element ~in_tag ~elements:spec_elements
      ?pcdata:spec_pcdata ?other_elements:spec_other_elements =
    function
    | Simplexmlparser.PCData str ->
      let spec_pcdata =
        Option.get (fun () -> ignore_blank_pcdata ~in_tag) spec_pcdata
      in
      spec_pcdata str
    | Simplexmlparser.Element (name, attributes, elements) ->
      try
        let spec = List.assoc name spec_elements in
        List.iter
          (check_attribute_occurrence ~in_tag:name attributes)
          spec.attributes;
        List.iter
          (check_element_occurrence ~in_tag:name elements)
          spec.elements;
        spec.init ();
        List.iter
          (process_attribute ~in_tag:name
             ~attributes:spec.attributes
             ?other_attributes:spec.other_attributes)
          attributes;
        List.iter
          (process_element ~in_tag:name
             ~elements:spec.elements
             ?pcdata:spec.pcdata
             ?other_elements:spec.other_elements)
          elements
      with Not_found ->
        match spec_other_elements with
        | Some spec_other_elements ->
          spec_other_elements name attributes elements
        | None ->
          raise (Error_in_config_file
                   ("Unknown tag "^name^" in tag "^in_tag))

  let process_elements ~in_tag ~elements:spec_elements ?pcdata ?other_elements
      ?(init=ignore) elements =
    List.iter
      (check_element_occurrence ~in_tag elements)
      spec_elements;
    init ();
    List.iter
      (process_element ~in_tag ~elements:spec_elements ?pcdata ?other_elements)
      elements

end


(*****************************************************************************)
let start_initialisation, during_initialisation,
    end_initialisation, get_numberofreloads =
  let init = ref true in
  let nb = ref (-1) in
  ((fun () ->
      init := true;
      nb := !nb+1;
    ),
   (fun () -> !init),
   (fun () ->
      init := false;
   ),
   (fun () -> !nb))

(********)


let host_match ~(virtual_hosts : virtual_hosts) ~host ~port =
  let port_match = function
    | None -> true
    | Some p -> p = port
  in match host with
  | None -> List.exists (fun (_, _, p) -> port_match p) virtual_hosts
  (*VVV Warning! For HTTP/1.0, when host is absent,
     we take the first one, even if it doesn't match!  *)
  | Some host ->
    let host_match regexp =
      (Netstring_pcre.string_match regexp host 0 <> None)
    in
    let rec aux = function
      | [] -> false
      | (_, a, p)::l -> (port_match p && host_match a) || aux l
    in
    aux virtual_hosts



(* Currently used only for error messages. *)
let string_of_host (h : virtual_hosts) =
  let aux1 (host, _, port) =
    match port with
    | None -> host
    | Some p -> host ^ ":" ^ string_of_int p
  in List.fold_left (fun d arg -> d ^ aux1  arg ^" ") "" h



let compute_result
    ?(previous_cookies = Ocsigen_cookies.Cookies.empty)
    ?(awake_next_request = false) ri =

  let host = Ocsigen_request_info.host ri in
  let port = Ocsigen_request_info.server_port ri in

  let conn = client_connection (Ocsigen_request_info.client ri) in
  let awake =
    if awake_next_request
    then
      (let tobeawoken = ref true in
       (* must be awoken once and only once *)
       fun () ->
         if !tobeawoken then begin
           tobeawoken := false;
           Ocsigen_http_com.wakeup_next_request conn
         end)
    else id
  in

  let rec do2 sites cookies_to_set ri =
    Ocsigen_request_info.update_nb_tries ri (Ocsigen_request_info.nb_tries ri + 1);
    if (Ocsigen_request_info.nb_tries ri) > Ocsigen_config.get_maxretries ()
    then fail Ocsigen_Looping_request
    else
      let string_of_host_option = function
        | None -> "<no host>:"^(string_of_int port)
        | Some h -> h^":"^(string_of_int port)
      in
      let rec aux_host ri prev_err cookies_to_set = function
        | [] -> fail (Ocsigen_http_error (cookies_to_set, prev_err))
        | (h, conf_info, host_function)::l when
            host_match ~virtual_hosts:h ~host ~port ->
          Lwt_log.ign_info_f ~section
            "host found! %a matches %a"
            (fun () -> string_of_host_option) host
            (fun () -> string_of_host) h;
          host_function
            awake
            cookies_to_set
            (Req_not_found (prev_err, { request_info = ri;
                                        request_config = conf_info }))
          >>= fun (res_ext, cookies_to_set) ->
          (match res_ext with
           | Ext_found r
           | Ext_found_stop r ->
             awake ();
             r () >>= fun r' ->
             Lwt.return (add_to_res_cookies r' cookies_to_set)
           | Ext_do_nothing ->
             aux_host ri prev_err cookies_to_set l
           | Ext_found_continue_with r ->
             awake ();
             r () >>= fun (r', _) ->
             return (add_to_res_cookies r' cookies_to_set)
           | Ext_found_continue_with' (r, _) ->
             awake ();
             return (add_to_res_cookies r cookies_to_set)
           | Ext_next e ->
             aux_host ri e cookies_to_set l
           (* try next site *)
           | Ext_stop_host (cook, e)
           | Ext_stop_site (cook, e) ->
             aux_host ri e (Ocsigen_cookies.add_cookies cook cookies_to_set) l
           (* try next site *)
           | Ext_stop_all (cook, e) ->
             fail (Ocsigen_http_error (cookies_to_set, e))
           | Ext_continue_with (_, cook, e) ->
             aux_host ri e
               (Ocsigen_cookies.add_cookies cook cookies_to_set) l
           | Ext_retry_with (request2, cook) ->
             do2
               (get_hosts ())
               (Ocsigen_cookies.add_cookies cook cookies_to_set)
               request2.request_info
           (* retry all *)
           | Ext_sub_result sr ->
             assert false
          )
        | (h, _, _)::l ->
          Lwt_log.ign_info_f ~section
            "host = %a does not match %a"
            (fun () -> string_of_host_option) host
            (fun () -> string_of_host) h;
          aux_host ri prev_err cookies_to_set l
      in aux_host ri 404 cookies_to_set sites
  in
  Lwt.finalize
    (fun () ->
       do2 (get_hosts ()) previous_cookies ri
    )
    (fun () ->
       awake ();
       Lwt.return ()
    )


(*****************************************************************************)

(* This is used by server.ml.
   I put that here because I need it to be accessible for profiling. *)
let sockets = ref []
let sslsockets = ref []

let get_number_of_connected,
    incr_connected,
    decr_connected,
    wait_fewer_connected =
  let connected = ref 0 in
  let maxr = ref (-1000) in
  let mvar = Lwt_mvar.create_empty () in
  ((fun () -> !connected),
   (fun n -> connected := !connected + n),
   (fun () ->
      let c = !connected in
      connected := c - 1;
      if !connected <= 0 && !sockets = [] && !sslsockets = []
      then exit 0;
      if c = !maxr
      then begin
        Lwt_log.ign_warning ~section "Number of connections now ok";
        maxr := -1000;
        Lwt_mvar.put mvar ()
      end
      else Lwt.return ()
   ),
   (fun max ->
      maxr := max;
      Lwt_mvar.take mvar)
  )



let get_server_address ri =
  let socket = Ocsigen_http_com.connection_fd (client_connection (Ocsigen_request_info.client ri)) in
  match Lwt_ssl.getsockname socket with
  | Unix.ADDR_UNIX _ -> failwith "unix domain socket have no ip"
  | Unix.ADDR_INET (addr,port) -> addr,port


(* user directories *)

exception NoSuchUser

type ud_string = Nodir of string | Withdir of string * string * string

let user_dir_regexp = Netstring_pcre.regexp "(.*)\\$u\\(([^\\)]*)\\)(.*)"

let parse_user_dir s =
  match Netstring_pcre.full_split user_dir_regexp s with
  | [ Netstring_pcre.Delim _;
      Netstring_pcre.Group (1, s1);
      Netstring_pcre.Group (2, u);
      Netstring_pcre.Group (3, s2)] ->
    Withdir (s1, u, s2)
  | _ -> Nodir s


let replace_user_dir regexp dest pathstring =
  match dest with
  | Nodir dest ->
    Netstring_pcre.global_replace regexp dest pathstring
  | Withdir (s1, u, s2) ->
      try
        let s1 = Netstring_pcre.global_replace regexp s1 pathstring in
        let u = Netstring_pcre.global_replace regexp u pathstring in
        let s2 = Netstring_pcre.global_replace regexp s2 pathstring in
        let userdir = (Unix.getpwnam u).Unix.pw_dir in
        Lwt_log.ign_info_f ~section "User %s" u;
        s1^userdir^s2
      with Not_found ->
        Lwt_log.ign_info_f ~section "No such user %s" u;
        raise NoSuchUser


(*****************************************************************************)
(* Finding redirections *)

exception Not_concerned

let find_redirection regexp full_url dest
    https host port
    get_params_string
    sub_path_string
    full_path_string
  =
  if full_url
  then
    match host with
      | None -> raise Not_concerned
      | Some host ->
          let path =
            match get_params_string with
              | None -> full_path_string
              | Some g -> full_path_string ^ "?" ^ g
          in
          let path =
            Url.make_absolute_url https host port ("/"^path)
          in
          (match Netstring_pcre.string_match regexp path 0 with
             | None -> raise Not_concerned
             | Some _ -> (* Matching regexp found! *)
                 Netstring_pcre.global_replace regexp dest path
          )
  else
    let path =
      match get_params_string with
      | None -> sub_path_string
      | Some g -> sub_path_string ^ "?" ^ g
    in
    match Netstring_pcre.string_match regexp path 0 with
    | None -> raise Not_concerned
    | Some _ -> (* Matching regexp found! *)
        Netstring_pcre.global_replace regexp dest path
