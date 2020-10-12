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

let section = Lwt_log.Section.make "ocsigen:ext"

open Lwt.Infix

module Url = Ocsigen_lib.Url

include Ocsigen_command

exception Ocsigen_http_error = Ocsigen_cohttp.Ocsigen_http_error
exception Ocsigen_Looping_request

(** Xml tag not recognized by an extension (usually not a real error) *)
exception Bad_config_tag_for_extension of string

(** Error in a <site> tag inside the main ocsigen.conf file *)
exception Error_in_config_file of string

(** Option incorrect in a userconf file *)
exception Error_in_user_config_file of string

type file_info = Ocsigen_multipart.file_info = {
  tmp_filename : string ;
  filesize : int64 ;
  raw_original_filename : string ;
  file_content_type : ((string * string) * (string * string) list) option
}

let badconfig fmt = Printf.ksprintf (fun s -> raise (Error_in_config_file s)) fmt

(* virtual hosts: *)
type virtual_hosts = (string * Pcre.regexp * int option) list

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

(* Server configuration, for local files that must not be sent *)

type do_not_serve = {
  do_not_serve_regexps: string list;
  do_not_serve_files: string list;
  do_not_serve_extensions: string list;
}

let serve_everything = {
  do_not_serve_regexps = [];
  do_not_serve_files = [];
  do_not_serve_extensions = []
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
      String.concat "|"
        (List.map (fun s -> Printf.sprintf "(%s)" (Pcre.quote s)) l)
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
       let r = Ocsigen_lib.Netstring_pcre.regexp regexp in
       Hashtbl.add hash_consed_do_not_serve d r;
       r
     with _ -> raise (IncorrectRegexpes d)
    )

type config_info = {
  default_hostname: string;
  default_httpport: int;
  default_httpsport: int;
  default_protocol_is_https: bool;

  mime_assoc: Ocsigen_charset_mime.mime_assoc;

  charset_assoc : Ocsigen_charset_mime.charset_assoc;

  default_directory_index : string list;   (** Default name to use as index file
                                              when a directory is requested.
                                              Use [None] if no index should be
                                              tried. The various indexes are
                                              tried in the given order. If no
                                              index is specified, or the index
                                              does not exists, the content of
                                              the directory might be listed,
                                              according to
                                              [list_directry_content] *)

  list_directory_content : bool; (** Should the list of files in a directory be
                                    displayed if there is no index in this
                                    directory ? *)

  follow_symlinks: [`No | `Owner_match | `Always]; (** Should symlinks be
                                                      followed when accessign a
                                                      local file? *)

  do_not_serve_404: do_not_serve;
  do_not_serve_403: do_not_serve;

  uploaddir: string option;
  maxuploadfilesize: int64 option;
}

let default_config_info () =
  let do_not_serve_404 = {
    do_not_serve_regexps = [];
    do_not_serve_files = [];
    do_not_serve_extensions = [];
  } in {
    default_hostname = Unix.gethostname ();
    default_httpport = Ocsigen_config.get_default_port ();
    default_httpsport = Ocsigen_config.get_default_sslport ();
    default_protocol_is_https = false;
    mime_assoc = Ocsigen_charset_mime.default_mime_assoc ();
    charset_assoc =
      Ocsigen_charset_mime.empty_charset_assoc
        ?default:(Ocsigen_config.get_default_charset ()) ();
    default_directory_index = ["index.html"];
    list_directory_content = false;
    follow_symlinks = `Owner_match;
    do_not_serve_404 ;
    do_not_serve_403 = do_not_serve_404 ;
    uploaddir = Ocsigen_config.get_uploaddir ();
    maxuploadfilesize = Ocsigen_config.get_maxuploadfilesize ();
  }

(* Requests *)
type request = {
  request_info   : Ocsigen_request.t;
  request_config : config_info;
}

exception Ocsigen_is_dir = Ocsigen_cohttp.Ocsigen_is_dir

type answer =
  | Ext_do_nothing
  (** I don't want to do anything *)
  | Ext_found of (unit -> Ocsigen_response.t Lwt.t)
  (** "OK stop! I will take the page.  You can start the following
      request of the same pipelined connection.  Here is the function
      to generate the page".  The extension must return Ext_found as
      soon as possible when it is sure it is safe to start next
      request.  Usually immediately. But in some case, for example
      proxies, you don't want the request of one connection to be
      handled in different order. (for example revproxy.ml starts its
      requests to another server before returning Ext_found, to ensure
      that all requests are done in same order). *)
  | Ext_found_stop of (unit -> Ocsigen_response.t Lwt.t)
  (** Found but do not try next extensions *)
  | Ext_next of Cohttp.Code.status
  (** Page not found. Try next extension. The status is usually
      `Not_found, but may be for example `Forbidden (403) if you want
      to try another extension afterwards. Same as Ext_continue_with
      but does not change the request. *)
  | Ext_stop_site of (Ocsigen_cookie_map.t * Cohttp.Code.status)
  (** Error. Do not try next extension, but try next site. *)
  | Ext_stop_host of (Ocsigen_cookie_map.t * Cohttp.Code.status)
  (** Error.
      Do not try next extension,
      do not try next site,
      but try next host. *)
  | Ext_stop_all of (Ocsigen_cookie_map.t * Cohttp.Code.status)
  (** Error. Do not try next extension,
      do not try next site,
      do not try next host. *)
  | Ext_continue_with of
      (request * Ocsigen_cookie_map.t * Cohttp.Code.status)
  (** Used to modify the request before giving it to next extension.
      The extension returns the request (possibly modified) and a set
      of cookies if it wants to set or cookies
      ({!Ocsigen_cookie_set.empty} for no cookies).  You must add
      these cookies yourself in request if you want them to be seen by
      subsequent extensions, for example using
      {!Ocsigen_http_frame.compute_new_ri_cookies}.  The status is
      usually equal to the one received from preceding extension (but
      you may want to modify it). *)
  | Ext_retry_with of request * Ocsigen_cookie_map.t
  (** Used to retry all the extensions with a new request.  The
      extension returns the request (possibly modified) and a set of
      cookies if it wants to set or cookies
      ({!Ocsigen_cookie_set.empty} for no cookies).  You must add
      these cookies yourself in request if you want them to be seen by
      subsequent extensions, for example using
      {!Ocsigen_http_frame.compute_new_ri_cookies}. *)
  | Ext_sub_result of extension_composite
  (** Used if your extension want to define option that may contain
      other options from other extensions.  In that case, while
      parsing the configuration file, call the parsing function (of
      type [parse_fun]), that will return something of type
      [extension_composite]. *)
  | Ext_found_continue_with of
      (unit -> (Ocsigen_response.t * request) Lwt.t)
  (** Same as [Ext_found] but may modify the request. *)
  | Ext_found_continue_with' of (Ocsigen_response.t * request)
  (** Same as [Ext_found_continue_with] but does not allow to delay
      the computation of the page. You should probably not use it, but
      for output filters. *)

and request_state =
  | Req_not_found of (Cohttp.Code.status * request)
  | Req_found of (request * Ocsigen_response.t)

and extension_composite =
  Ocsigen_cookie_map.t ->
  request_state ->
  (answer * Ocsigen_cookie_map.t) Lwt.t

type extension = request_state -> answer Lwt.t

type parse_fun = Xml.xml list -> extension_composite

type parse_host =
    Parse_host of
      (Url.path ->
       parse_host -> parse_fun -> Xml.xml -> extension)

let hosts
  : (virtual_hosts * config_info * extension_composite) list ref
  = ref []

let set_hosts v = hosts := v
let get_hosts () = !hosts

(* Default hostname is either the Host header or the hostname set in
   the configuration file. *)
let get_hostname {request_info ; request_config = {default_hostname;_};_} =
  if Ocsigen_config.get_usedefaulthostname () then
    default_hostname
  else
    match Ocsigen_request.host request_info with
    | None -> default_hostname
    | Some host -> host

(* Default port is either
   - the port the server is listening at
   - or the port in the Host header
   - or the default port set in the configuration file. *)
let get_port
    {
      request_info ;
      request_config = { default_httpport ; default_httpsport ;_ }
    } =
  if Ocsigen_config.get_usedefaulthostname () then
    if Ocsigen_request.ssl request_info then
      default_httpsport
    else
      default_httpport
  else
    Ocsigen_request.port request_info

let new_url_of_directory_request request ri =
  Lwt_log.ign_info ~section "Sending 301 Moved permanently";
  let ssl = Ocsigen_request.ssl ri in
  let scheme = if ssl then "https" else "http"
  and host = get_hostname request
  and port =
    let port = get_port request in
    if port = if ssl then 443 else 80 then
      None
    else
      Some port
  and path =
    let path = Ocsigen_request.path_string ri in
    if path.[String.length path - 1] = '/' then path else path ^ "/"
  and query = Ocsigen_request.get_params ri in
  Uri.make ~scheme ~host ?port ~path ~query ()

(* To give parameters to extensions: *)
let dynlinkconfig = ref ([] : Xml.xml list)
let set_config s = dynlinkconfig := s
let get_config () = !dynlinkconfig

let site_match request (site_path : string list) url =
  (* We are sure that there is no / at the end or beginning of site_path *)
  (* and no / at the beginning of url *)
  (* and no // or ../ inside both of them *)
  (* We return the subpath without / at beginning *)
  let rec aux site_path url =
    match site_path, url with
    | [], [] ->
      raise (Ocsigen_is_dir (new_url_of_directory_request request))
    | [], p -> Some p
    | a::l, aa::ll when a = aa -> aux l ll
    | _ -> None
  in
  match site_path, url with
  | [], [] -> Some []
  | _ -> aux site_path url

let default_extension_composite : extension_composite =
  fun cookies_to_set -> function
    | Req_found (ri, res) ->
      Lwt.return (Ext_found_continue_with' (res, ri), cookies_to_set)
    | Req_not_found (e, ri) ->
      Lwt.return
        (Ext_continue_with
           (ri, Ocsigen_cookie_map.empty, e), cookies_to_set)

let compose_step (f : extension) (g : extension_composite)
  : extension_composite
  = fun cookies_to_set req_state ->
    f req_state >>= fun res ->
    let rec aux cookies_to_set = function
      | Ext_do_nothing ->
        g cookies_to_set req_state
      | Ext_found r ->
        r () >>= fun r' ->
        let ri = match req_state with
          | Req_found (ri, _) -> ri
          | Req_not_found (_, ri) -> ri
        in
        g Ocsigen_cookie_map.empty
          (Req_found (ri, Ocsigen_response.add_cookies r' cookies_to_set))
      | Ext_found_continue_with r ->
        r () >>= fun (r', req) ->
        g Ocsigen_cookie_map.empty
          (Req_found (req, Ocsigen_response.add_cookies r' cookies_to_set))
      | Ext_found_continue_with' (r', req) ->
        g Ocsigen_cookie_map.empty
          (Req_found (req, Ocsigen_response.add_cookies r' cookies_to_set))
      | Ext_next e ->
        let ri = match req_state with
          | Req_found (ri, _) -> ri
          | Req_not_found (_, ri) -> ri
        in
        g cookies_to_set (Req_not_found (e, ri))
      | Ext_continue_with (ri, cook, e) ->
        g (Ocsigen_cookie_map.add_multi cook cookies_to_set)
          (Req_not_found (e, ri))
      | Ext_found_stop _
      | Ext_stop_site _
      | Ext_stop_host _
      | Ext_stop_all _
      | Ext_retry_with _ as res ->
        Lwt.return (res, cookies_to_set)
      | Ext_sub_result sr ->
        sr cookies_to_set req_state >>= fun (res, cookies_to_set) ->
        aux cookies_to_set res
    in
    aux cookies_to_set res

let rec compose = function
  | [] ->
    default_extension_composite
  | e :: rest ->
    compose_step e (compose rest)

let fun_end = ref (fun () -> ())
let fun_exn = ref (fun exn -> (raise exn : string))

let rec parse_site_attrs (enc, dir) = function
  | [] ->
    (match dir with
     | None ->
       raise (Ocsigen_config.Config_file_error
                ("Missing dir attribute in <site>"))
     | Some s -> enc, s)
  | ("path", s) :: rest | ("dir", s) :: rest ->
    (match dir with
     | None -> parse_site_attrs (enc, Some s) rest
     | _ ->
       raise (Ocsigen_config.Config_file_error
                ("Duplicate attribute dir in <site>")))
  | ("charset", s) :: rest ->
    (match enc with
     | None -> parse_site_attrs ((Some s), dir) rest
     | _ ->
       raise (Ocsigen_config.Config_file_error
                ("Duplicate attribute charset in <site>")))
  | (s, _) :: _ ->
    raise
      (Ocsigen_config.Config_file_error ("Wrong attribute for <site>: "^s))

let make_parse_config path parse_host l : extension_composite =
  let f = parse_host path (Parse_host parse_host) in
  (* creates all site data, if any *)
  let rec parse_config : _ -> extension_composite = function
    | [] ->
      default_extension_composite
    | xmltag :: ll ->
       try
         (* The evaluation order is important here *)
         let f = f parse_config xmltag in
         compose_step f (parse_config ll)
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
  let r =
    try
      parse_config l
    with e -> !fun_end (); raise e
    (*VVV Maybe we should avoid calling fun_end after parsing user
      config files (with extension userconf) ... See eliommod.ml *)
  in
  !fun_end ();
  r

let site_ext ext_of_children charset path cookies_to_set = function
  | Req_found (ri, res) ->
    Lwt.return (Ext_found_continue_with' (res, ri), cookies_to_set)
  | Req_not_found (e, oldri) ->
    let oldri = match charset with
      | None -> oldri
      | Some charset ->
        { oldri with
          request_config =
            { oldri.request_config with
              charset_assoc =
                Ocsigen_charset_mime.set_default_charset
                  oldri.request_config.charset_assoc charset
            }
        }
    in
    match
      site_match oldri path
        (Ocsigen_request.path oldri.request_info)
    with
    | None ->
      Lwt_log.ign_info_f ~section
        "site \"%a\" does not match url \"%a\"."
        (fun () path  ->
           Url.string_of_url_path ~encode:true path) path
        (fun () oldri ->
           Url.string_of_url_path ~encode:true
             (Ocsigen_request.path oldri.request_info))
        oldri;
      Lwt.return (Ext_next e, cookies_to_set)
    | Some sub_path ->
      Lwt_log.ign_info_f ~section
        "site found: url \"%a\" matches \"%a\"."
        (fun () oldri ->
           Url.string_of_url_path ~encode:true
             (Ocsigen_request.path oldri.request_info))
        oldri
        (fun () path -> Url.string_of_url_path ~encode:true path) path;
      let ri =
        { oldri with
          request_info =
            Ocsigen_request.update oldri.request_info
              ~sub_path:
                (Url.string_of_url_path ~encode:true sub_path)
        } in
      ext_of_children cookies_to_set (Req_not_found (e, ri)) >>= function
        (* After a site, we turn back to old ri *)
      | (Ext_stop_site (cs, err), cookies_to_set)
      | (Ext_continue_with (_, cs, err), cookies_to_set) ->
        Lwt.return
          (Ext_continue_with (oldri, cs, err), cookies_to_set)
      | (Ext_found_continue_with r, cookies_to_set) ->
        r () >>= fun (r', _req) ->
        Lwt.return
          (Ext_found_continue_with' (r', oldri), cookies_to_set)
      | (Ext_found_continue_with' (r, _req), cookies_to_set) ->
        Lwt.return
          (Ext_found_continue_with' (r, oldri), cookies_to_set)
      | (Ext_do_nothing, cookies_to_set) ->
        Lwt.return
          (Ext_continue_with
             (oldri,
              Ocsigen_cookie_map.empty,
              e), cookies_to_set)
      | r ->
        Lwt.return r

let site_ext ext_of_children charset path : extension = function
  | Req_found (ri, r) ->
    Lwt.return (Ext_found_continue_with' (r, ri))
  | Req_not_found _ ->
    Lwt.return (Ext_sub_result (site_ext ext_of_children charset path))

let preprocess_site_path p = Url.(
  remove_dotdot p
  |> remove_slash_at_beginning
  |> remove_slash_at_end
)

(* Implements only <site> parsing. Uses parse_host to recursively
   parse children of <site>. *)
let default_parse_config
    _userconf_info
    (_host : virtual_hosts)
    _config_info
    prevpath
    (Parse_host parse_host)
    (_parse_fun : parse_fun) = function
  | Xml.Element ("site", atts, l) ->
    let charset, dir = parse_site_attrs (None, None) atts in
    let path = prevpath @ preprocess_site_path (Url.split_path dir) in
    let ext_of_children = make_parse_config path parse_host l in
    site_ext ext_of_children charset path
  | Xml.Element (tag,_,_) ->
    raise (Bad_config_tag_for_extension tag)
  | _ -> raise (Ocsigen_config.Config_file_error
                  ("Unexpected content inside <host>"))

type userconf_info = {
  localfiles_root : string;
}

type parse_config =
  userconf_info option ->
  virtual_hosts ->
  config_info ->
  parse_config_aux
and parse_config_aux =
    Url.path -> parse_host ->
    (parse_fun -> Xml.xml ->
     extension
    )

let _extension_void_fun_site : parse_config = fun _ _ _ _ _ _ -> function
  | Xml.Element (t, _, _) -> raise (Bad_config_tag_for_extension t)
  | _ -> raise (Error_in_config_file "Unexpected data in config file")

let register, parse_config_item, get_init_exn_handler =
  let ref_fun_site = ref default_parse_config in

  (fun
    ?fun_site
    ?end_init
    ?(exn_handler=raise)
    ?(respect_pipeline=false)
    () ->
    if respect_pipeline then Ocsigen_config.set_respect_pipeline ();
    (match fun_site with
     | None -> ()
     | Some fun_site ->
       let old_fun_site = !ref_fun_site in
       ref_fun_site :=
         (fun path host conf_info ->
            let oldf = old_fun_site path host conf_info in
            let newf =     fun_site path host conf_info in
            fun path parse_host ->
              let oldf = oldf path parse_host in
              let newf = newf path parse_host in
              fun parse_config config_tag ->
                try
                  oldf parse_config config_tag
                with
                | Bad_config_tag_for_extension _c ->
                  newf parse_config config_tag
         ));
    (match end_init with
     | Some end_init -> fun_end := Ocsigen_lib.comp end_init !fun_end;
     | None -> ());
    let curexnfun = !fun_exn in
    fun_exn := fun e -> try curexnfun e with e -> exn_handler e),

  (fun path host conf -> !ref_fun_site path host conf),

  (fun () -> !fun_exn)

let default_parse_extension ext_name = function
  | [] -> ()
  | _ -> raise (Error_in_config_file
                  (Printf.sprintf "Unexpected content found in configuration of extension %s: %s does not accept options" ext_name ext_name))

let register
    ~name
    ?fun_site
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
       register ?fun_site ?end_init ?exn_handler ?respect_pipeline ())
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
    other_elements : (string -> (string * string) list -> Xml.xml list -> unit) option;
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

  let check_attribute_occurrence ~in_tag attributes = function
    | name, { attribute_obligatory = true ;_ } ->
      (try ignore (List.assoc name attributes)
       with Not_found ->
         raise (Error_in_user_config_file
                  ("Obligatory attribute "^name^" not in tag "^in_tag)))
    | _ -> ()

  let check_element_occurrence ~in_tag elements = function
    | name, { obligatory = true ;_ } ->
      let corresponding_element = function
        | Xml.Element (name', _, _) -> name = name'
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
    | Xml.PCData str ->
      let spec_pcdata =
        Ocsigen_lib.Option.get
          (fun () -> ignore_blank_pcdata ~in_tag)
          spec_pcdata
      in
      spec_pcdata str
    | Xml.Element (name, attributes, elements) ->
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
      (Ocsigen_lib.Netstring_pcre.string_match regexp host 0 <> None)
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
    ?(previous_cookies = Ocsigen_cookie_map.empty)
    request_info =

  let host = Ocsigen_request.host request_info
  and port = Ocsigen_request.port request_info in

  let string_of_host_option = function
    | None -> "<no host>:"^(string_of_int port)
    | Some h -> h^":"^(string_of_int port)
  in

  let rec fold_hosts
      request_info
      (prev_err : Cohttp.Code.status)
      cookies_to_set =
    function
    | [] ->
      Lwt.fail (Ocsigen_http_error (cookies_to_set, prev_err))
    | (virtual_hosts, request_config, host_function) :: l when
        host_match ~virtual_hosts ~host ~port ->
      Lwt_log.ign_info_f ~section
        "host found! %a matches %a"
        (fun () -> string_of_host_option) host
        (fun () -> string_of_host) virtual_hosts;
      host_function
        cookies_to_set
        (Req_not_found (prev_err, { request_info ; request_config }))
      >>= fun (res_ext, cookies_to_set) ->
      (match res_ext with
       | Ext_found r
       | Ext_found_stop r ->
         r () >>= fun r' ->
         Lwt.return (Ocsigen_response.add_cookies r' cookies_to_set)
       | Ext_do_nothing ->
         fold_hosts request_info prev_err cookies_to_set l
       | Ext_found_continue_with r ->
         r () >>= fun (r', _) ->
         Lwt.return (Ocsigen_response.add_cookies r' cookies_to_set)
       | Ext_found_continue_with' (r, _) ->
         Lwt.return (Ocsigen_response.add_cookies r cookies_to_set)
       | Ext_next e ->
         fold_hosts request_info e cookies_to_set l
       (* try next site *)
       | Ext_stop_host (cook, e)
       | Ext_stop_site (cook, e) ->
         fold_hosts request_info e
           (Ocsigen_cookie_map.add_multi cook cookies_to_set) l
       (* try next site *)
       | Ext_stop_all (_cook, e) ->
         Lwt.fail (Ocsigen_http_error (cookies_to_set, e))
       | Ext_continue_with (_, cook, e) ->
         fold_hosts request_info e
           (Ocsigen_cookie_map.add_multi cook cookies_to_set) l
       | Ext_retry_with (request2, cook) ->
         fold_hosts_limited
           (get_hosts ())
           (Ocsigen_cookie_map.add_multi cook cookies_to_set)
           request2.request_info
       (* retry all *)
       | Ext_sub_result _sr ->
         assert false
      )
    | (h, _, _)::l ->
      Lwt_log.ign_info_f ~section
        "host = %a does not match %a"
        (fun () -> string_of_host_option) host
        (fun () -> string_of_host) h;
      fold_hosts request_info prev_err cookies_to_set l

  and fold_hosts_limited sites cookies_to_set request_info =
    Ocsigen_request.incr_tries request_info;
    if
      Ocsigen_request.tries request_info >
      Ocsigen_config.get_maxretries ()
    then
      Lwt.fail Ocsigen_Looping_request
    else
      fold_hosts request_info `Not_found cookies_to_set sites
  in

  fold_hosts_limited (get_hosts ()) previous_cookies request_info

let get_number_of_connected = Ocsigen_cohttp.get_number_of_connected

(* user directories *)

exception NoSuchUser

type ud_string = Nodir of string | Withdir of string * string * string

let user_dir_regexp =
  Ocsigen_lib.Netstring_pcre.regexp "(.*)\\$u\\(([^\\)]*)\\)(.*)"

let parse_user_dir s =
  match Pcre.full_split ~rex:user_dir_regexp ~max:(-1) s with
  | [ Pcre.Delim _;
      Pcre.Group (1, s1);
      Pcre.Group (2, u);
      Pcre.Group (3, s2)] ->
    Withdir (s1, u, s2)
  | _ -> Nodir s


let replace_user_dir regexp dest pathstring =
  match dest with
  | Nodir dest ->
    Ocsigen_lib.Netstring_pcre.global_replace regexp dest pathstring
  | Withdir (s1, u, s2) ->
    try
      let s1 =
        Ocsigen_lib.Netstring_pcre.global_replace
          regexp s1 pathstring
      in
      let u =
        Ocsigen_lib.Netstring_pcre.global_replace
          regexp u pathstring
      in
      let s2 =
        Ocsigen_lib.Netstring_pcre.global_replace
          regexp s2 pathstring
      in
      let userdir = (Unix.getpwnam u).Unix.pw_dir in
      Lwt_log.ign_info_f ~section "User %s" u;
      s1^userdir^s2
    with Not_found ->
      Lwt_log.ign_info_f ~section "No such user %s" u;
      raise NoSuchUser

exception Not_concerned

let (>|!) v f =
  match v with
  | None ->
    raise Not_concerned
  | Some v ->
    f v

let find_redirection regexp full_url dest r =
  if full_url then
    Ocsigen_request.host r >|! fun host ->
    let path =
      let full_path = Ocsigen_request.path_string r in
      match Ocsigen_request.query r with
      | None -> full_path
      | Some g -> full_path ^ "?" ^ g
    in
    let path =
      Url.make_absolute_url
        ~https:(Ocsigen_request.ssl r)
        ~host
        ~port:(Ocsigen_request.port r)
        ("/" ^ path)
    in
    Ocsigen_lib.Netstring_pcre.string_match regexp path 0 >|! fun _ ->
    (* Matching regexp found! *)
    Ocsigen_lib.Netstring_pcre.global_replace regexp dest path
  else
    let path =
      let sub_path = Ocsigen_request.sub_path_string r in
      match Ocsigen_request.query r with
      | None -> sub_path
      | Some g -> sub_path ^ "?" ^ g
    in
    Ocsigen_lib.Netstring_pcre.string_match regexp path 0 >|! fun _ ->
    (* Matching regexp found! *)
    Ocsigen_lib.Netstring_pcre.global_replace regexp dest path
