(* Ocsigen
 * http://www.ocsigen.org
 * Module pagesearch.mli
 * Copyright (C) 2005 Vincent Balat
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

(** Extensions interface for Ocsigen Server *)

val section : Logs.src
(** use Lwt_log.Section.set_level in order to debug *)

include module type of Ocsigen_command

exception Ocsigen_http_error of Ocsigen_cookie_map.t * Cohttp.Code.status

exception Bad_config_tag_for_extension of string
(** Xml tag not recognized by an extension (usually not a real error) *)

exception Error_in_config_file of string
(** Error in a <site> tag inside the main ocsigen.conf file *)

exception Error_in_user_config_file of string
(** Option incorrect in a userconf file *)

type file_info = Ocsigen_multipart.file_info =
  { tmp_filename : string
  ; filesize : int64
  ; raw_original_filename : string
  ; file_content_type : ((string * string) * (string * string) list) option }

val badconfig : ('a, unit, string, 'b) format4 -> 'a
(** Convenient function for raising Error_in_config_file exceptions with
    a sprintf-formatted argument. *)

(*****************************************************************************)

type virtual_hosts = (string * Re.Pcre.regexp * int option) list
(** Type of the result of parsing the field [hostfiler] in the configuration
    file. Inside the list, the first argument is the host itself
    (which is a glob-like pattern that can contains [*]), a regexp
    parsing this pattern, and optionally a port.
*)

val hash_virtual_hosts : virtual_hosts -> int
val equal_virtual_hosts : virtual_hosts -> virtual_hosts -> bool

val host_match :
   virtual_hosts:virtual_hosts
  -> host:string option
  -> port:int
  -> bool

(*****************************************************************************)

type do_not_serve =
  { do_not_serve_regexps : string list
  ; do_not_serve_files : string list
  ; do_not_serve_extensions : string list }
(** Configuration to hide/forbid local files *)

val serve_everything : do_not_serve

exception IncorrectRegexpes of do_not_serve

val do_not_serve_to_regexp : do_not_serve -> Re.Pcre.regexp
(** Compile a do_not_serve structure into a regexp. Raises
    [IncorrectRegexpes] if the compilation fails. The result is
    memoized for subsequent calls with the same argument *)

val join_do_not_serve : do_not_serve -> do_not_serve -> do_not_serve

type config_info =
  { default_hostname : string
  ; default_httpport : int
  ; default_httpsport : int
  ; default_protocol_is_https : bool
  ; mime_assoc : Ocsigen_charset_mime.mime_assoc
  ; charset_assoc : Ocsigen_charset_mime.charset_assoc
  ; default_directory_index : string list
    (** Default name to use as index file when a directory is requested.
      Use [None] if no index should be tried. The various indexes are
      tried in the given order. If no index is specified, or the index
      does not exists, the content of the directory might be listed,
      according to [list_directory_content] *)
  ; list_directory_content : bool
    (** Should the list of files in a directory be displayed if there is
      no index in this directory ? *)
  ; follow_symlinks : [`No | `Owner_match | `Always]
    (** Should symlinks be followed when accessing a local file? *)
  ; do_not_serve_404 : do_not_serve
  ; do_not_serve_403 : do_not_serve
  ; uploaddir : string option
  ; maxuploadfilesize : int64 option }
(** Configuration options, passed to (and modified by) extensions *)

val default_config_info : unit -> config_info

type request = {request_info : Ocsigen_request.t; request_config : config_info}

exception Ocsigen_is_dir of (Ocsigen_request.t -> Uri.t)

type answer =
  | Ext_do_nothing  (** I don't want to do anything *)
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
  | Ext_continue_with of (request * Ocsigen_cookie_map.t * Cohttp.Code.status)
  (** Used to modify the request before giving it to next extension.
      The extension returns the request (possibly modified) and a set
      of cookies if it wants to set or cookies
      ({!Ocsigen_cookie_map.empty} for no cookies).  You must add
      these cookies yourself in request if you want them to be seen by
      subsequent extensions, for example using
      {!Ocsigen_http_frame.compute_new_ri_cookies}.  The status is
      usually equal to the one received from preceding extension (but
      you may want to modify it). *)
  | Ext_retry_with of request * Ocsigen_cookie_map.t
  (** Used to retry all the extensions with a new request.  The
      extension returns the request (possibly modified) and a set of
      cookies if it wants to set or cookies
      ({!Ocsigen_cookie_map.empty} for no cookies).  You must add
      these cookies yourself in request if you want them to be seen by
      subsequent extensions, for example using
      {!Ocsigen_http_frame.compute_new_ri_cookies}. *)
  | Ext_sub_result of extension_composite
  (** Used if your extension want to define option that may contain
      other options from other extensions.  In that case, while
      parsing the configuration file, call the parsing function (of
      type [parse_fun]), that will return something of type
      [extension_composite]. *)
  | Ext_found_continue_with of (unit -> (Ocsigen_response.t * request) Lwt.t)
  (** Same as [Ext_found] but may modify the request. *)
  | Ext_found_continue_with' of (Ocsigen_response.t * request)
  (** Same as [Ext_found_continue_with] but does not allow to delay
      the computation of the page. You should probably not use it, but
      for output filters. *)

and request_state =
  | Req_not_found of (Cohttp.Code.status * request)
  | Req_found of (request * Ocsigen_response.t)

and extension_composite =
  Ocsigen_cookie_map.t -> request_state -> (answer * Ocsigen_cookie_map.t) Lwt.t

type extension = request_state -> answer Lwt.t
(** For each <site> tag in the configuration file,
    you can set the extensions you want.
    Each extension is implemented as a function, taking
    the charset found in configuration file,
    the current state of the request,
    and returning an answer.
    If no page has been generated so far ([Req_not_found]), it receive
    the error code given by the previous extension (default 404),
    and the request information.
    If a page has been generated by previous extensions (case [Req_found]),
    the extension may want to modify the result (filters).
*)

type parse_fun = Xml.xml list -> extension_composite

type parse_host
(** Type of the functions parsing the content of a <host> tag *)

type userconf_info = {localfiles_root : string}
(** Information received by extensions accepting userconf files.

    The parameter [localfiles_root] is an absolute path to the
    directory that the user is allowed to serve. This is used
    by staticmod, to disallow the user from allowing access to
    outside of this directory
*)

type parse_config =
  userconf_info option -> virtual_hosts -> config_info -> parse_config_aux
(** [parse_config] is the type of the functions parsing a <site> tag
    (and returning an extension).  Those are functions taking
    {ul
     {- the name of the virtual <host>}}
     that will be called for each <host>,
     and that will generate a function taking:
    {ul
     {- the path attribute of a <site> tag}}
     that will be called for each <site>,
     and that will generate a function taking:
    {ul
     {- an item of the config file}}
     that will be called on each tag inside <site> and:
    {ul
     {- raise [Bad_config_tag_for_extension] if it does not recognize that tag}
     {- return something of type [extension] (filter or page generator)}}

    [parse_config_user] is the type of functions parsing a site tag
    inside an userconf file. They take one more parameter, of type userconf_info
*)

and parse_config_aux =
  Ocsigen_lib.Url.path -> parse_host -> parse_fun -> Xml.xml -> extension

val register :
   name:string
  -> ?fun_site:parse_config
  -> ?end_init:(unit -> unit)
  -> ?init_fun:(Xml.xml list -> unit)
  -> ?exn_handler:(exn -> string)
  -> ?respect_pipeline:bool
  -> unit
  -> unit
(** For each extension generating pages, we register its name and six functions:
    - a function [fun_site] of type [parse_config]. This function
    will be responsible for handling the options of the configuration
    files that are recognized by the extension, and potentially generating
    a page.
    - a function [end_init] that will be called at the end of the initialisation
    phase of each site
    - a function [init_fun] that will be called just before registering the
    extension, taking as parameter the configuration options between
    [<extension>] and [</extension>]. This allows to give configuration options
    to extensions. If no function is supplied, the extension is supposed to
    accept no option (and loading will fail if an option is supplied)
    See {!Configuration.process_elements} for
    the easy construction of such a function.
    - a function [exn_handler] that will create an error message from the
    exceptions that may be raised during the initialisation phase, and raise again
    all other exceptions

    Moreover, if the optional parameter [?respect_pipeline] is [true],
    the extension will ask the server to respect the order of the
    pipeline. That means that it will wait to be sure that the previous
    request from the same connection has been taken by an extension
    before giving a request to an extension.  Use this to write proxies
    extensions, when you want to be able to pipeline the requests you
    to another server. It is false by default.

*)

(** This modules contains types and constructor for the description of XML
    configurations and the accordingly parsing. *)
module Configuration : sig
  type element
  (** Specification of a XML element. *)

  type attribute
  (** Specification of a XML attribute. *)

  val element :
     name:string
    -> ?obligatory:bool
    -> ?init:(unit -> unit)
    -> ?elements:element list
    -> ?attributes:attribute list
    -> ?pcdata:(string -> unit)
    -> ?other_elements:
         (string -> (string * string) list -> Xml.xml list -> unit)
    -> ?other_attributes:(string -> string -> unit)
    -> unit
    -> element
  (** Create the specification of a XML element.
      @param name Name of the XML tag
      @param init A function to be executed before processing the child elements and attributes
      @param obligatory Whether the element is obligatory ([false] by default)
      @param elements Specifications of the child elements
      @param attribute Specifications of the attributes
      @param pcdata Function to be applied on the pcdata ([ignore_blank_pcdata] by default)
      @param other_elements Optional function to be applied on the content of unspecified tags
      @param other_attributes Optional function to be applied on the unspecfied attributes
  *)

  val attribute :
     name:string
    -> ?obligatory:bool
    -> (string -> unit)
    -> attribute
  (** [attribute ~name f] create a specification of a XML attribute.
      @param name The name of the XML attribute
      @param obligatory Whether the attribute is obligatory ([false] by default)
      @param f Function to be applied on the value string of the attribute
  *)

  val process_element :
     in_tag:string
    -> elements:element list
    -> ?pcdata:(string -> unit)
    -> ?other_elements:
         (string -> (string * string) list -> Xml.xml list -> unit)
    -> Xml.xml
    -> unit
  (** Process an XML element by the specifications.
      @param in_tag Name of the enclosing XML tag (just for generating error messages)
      @param elements Specifications of the child elements
      @param pcdata Function to be applied on the PCDATA ([ignore_blank_pcdata] by default)
      @param other_elements Optional function to be applied on unexpected child elements
      @raise Error_in_config_file If an element (resp. attribute) occurs
        which is not specified by the [element] (resp. [attribute]) parameter
        and no function [other_elements] (resp. other_attributes) is provided
  *)

  val process_elements :
     in_tag:string
    -> elements:element list
    -> ?pcdata:(string -> unit)
    -> ?other_elements:
         (string -> (string * string) list -> Xml.xml list -> unit)
    -> ?init:(unit -> unit)
    -> Xml.xml list
    -> unit
  (** Application of [process_element] on a list of XML elements. *)

  val ignore_blank_pcdata : in_tag:string -> string -> unit
  (** The specification for ignoring blank PCDATA ('\n', '\r', ' ', '\t') and failing
      otherwise (a reasonable default). *)

  val refuse_pcdata : in_tag:string -> string -> unit
end

val get_hostname : request -> string
(** Returns the hostname to be used for absolute links or redirections.
    It is either the Host header or the hostname set in
    the configuration file. *)

val get_port : request -> int
(** Returns the port to be used for absolute links or redirections.
    It is either:
    - the port the server is listening at
    - or the port in the Host header
    - or the default port set in the configuration file. *)

val new_url_of_directory_request : request -> Ocsigen_request.t -> Uri.t
(** new_url_of_directory_request create a redirection and generating a new url
    for the client (depending on the server configuration and request)
    @param request configuration of the server
    @param ri request *)

(** {3 User directories} *)

(** Exception raised when an non-existing user is found *)

exception NoSuchUser

type ud_string
(** The type for string that may contain a $u(...) *)

val parse_user_dir : string -> ud_string

val replace_user_dir : Re.Pcre.regexp -> ud_string -> string -> string
(** raises [Not_found] is the directory does not exist *)

exception Not_concerned
(** {3 Regular expressions for redirections} *)

val find_redirection :
   Re.Pcre.regexp
  -> bool
  -> string
  -> Ocsigen_request.t
  -> string

(**/**)

val preprocess_site_path : Ocsigen_lib.Url.path -> Ocsigen_lib.Url.path
val compose : extension list -> extension_composite
val make_parse_config : Ocsigen_lib.Url.path -> parse_config_aux -> parse_fun
val parse_config_item : parse_config

val site_ext :
   extension_composite
  -> Ocsigen_charset_mime.charset option
  -> Ocsigen_lib.Url.path
  -> extension

type host_config = virtual_hosts * config_info * extension_composite

val set_hosts : host_config list -> unit
val get_hosts : unit -> (virtual_hosts * config_info * extension_composite) list

val compute_result :
   ?previous_cookies:Ocsigen_cookie_map.t
  -> Ocsigen_request.t
  -> Ocsigen_response.t Lwt.t
(** Compute the answer to be sent to the client, by trying all
    extensions according the configuration file. *)

(** {3 Profiling} *)

val get_number_of_connected : unit -> int
val during_initialisation : unit -> bool
val start_initialisation : unit -> unit
val end_initialisation : unit -> unit
val get_numberofreloads : unit -> int
val get_init_exn_handler : unit -> exn -> string
val set_config : Xml.xml list -> unit
