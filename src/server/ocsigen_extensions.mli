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
(*****************************************************************************)
(*****************************************************************************)
(* Tables of services (global and session tables)                            *)
(* Store and load dynamic pages                                              *)
(*****************************************************************************)
(*****************************************************************************)

(** Writing extensions for Ocsigen                                           *)

open Lwt
open Ocsigen_lib
open Ocsigen_cookies

module Ocsigen_request_info : (module type of Ocsigen_request_info
                                with type request_info = Ocsigen_request_info.request_info
                                 and type file_info = Ocsigen_request_info.file_info
                                 and type ifrange = Ocsigen_request_info.ifrange)

exception Ocsigen_http_error of (Ocsigen_cookies.cookieset * int)

(** Xml tag not recognized by an extension (usually not a real error) *)
exception Bad_config_tag_for_extension of string

(** Error in a <site> tag inside the main ocsigen.conf file *)
exception Error_in_config_file of string

(** Option incorrect in a userconf file *)
exception Error_in_user_config_file of string



val badconfig : ('a, unit, string, 'b) format4 -> 'a
(** Convenient function for raising Error_in_config_file exceptions with
    a sprintf-formatted argument. *)

(*****************************************************************************)

(** Type of the result of parsing the field [hostfiler] in the configuration
    file. Inside the list, the first argument is the host itself
    (which is a glob-like pattern that can contains [*]), a regexp
    parsing this pattern, and optionnaly a port.
*)

module VirtualHost : sig
  type t

  (** Function to make new virtual host *)
  val make :
    host:string ->
    pattern:Netstring_pcre.regexp ->
    ?port:int -> unit -> t

  (** Hash virtual host (only hash host and port, no pattern) *)
  val hash : t -> int
  (** Equal of virtual host (only compare host and port, no pattern) *)
  val equal : t -> t -> bool

  (** Host accessor for virtual host *)
  val host : t -> string
  (** Pattern accessor for virtual host *)
  val pattern : t -> Netstring_pcre.regexp
  (** Port accessor for virtual host *)
  val port : t -> int option
end

type virtual_hosts = VirtualHost.t list

val hash_virtual_hosts : virtual_hosts -> int
val equal_virtual_hosts : virtual_hosts -> virtual_hosts -> bool

val host_match:
  virtual_hosts:virtual_hosts -> host:string option -> port:int -> bool

(*****************************************************************************)

(** Configuration to hide/forbid local files *)
type do_not_serve = {
  do_not_serve_regexps: string list;
  do_not_serve_files: string list;
  do_not_serve_extensions: string list;
}

(** Default configuration to hide/forbid local files.
    For this, no file is filtered *)
val serve_everything : do_not_serve

exception IncorrectRegexpes of do_not_serve

(** Compile a do_not_serve structure into a regexp. Raises
    [IncorrectRegexpes] if the compilation fails. The result is
    memoized for subsequent calls with the same argument *)
val do_not_serve_to_regexp: do_not_serve -> Netstring_pcre.regexp

val join_do_not_serve : do_not_serve -> do_not_serve -> do_not_serve



(** Configuration options, passed to (and modified by) extensions *)
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
      might be listed, according to [list_directory_content] *)
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


(*****************************************************)


type client = Ocsigen_http_com.connection
(** A value of this type represents the client who did the request. *)

val client_id : client -> int
(** Returns the id number of the connection *)

val client_connection : client -> Ocsigen_http_com.connection
(** Returns the connection *)

type ifrange = Ocsigen_request_info.ifrange =
  | IR_No
  | IR_Ifunmodsince of float
  | IR_ifmatch of string
type file_info = Ocsigen_request_info.file_info = {
  tmp_filename: string;
  filesize: int64;
  raw_original_filename: string;
  original_basename: string ;
  file_content_type: ((string * string) * (string * string) list) option;
}
type request_info = Ocsigen_request_info.request_info
and request = {
  request_info: request_info;
  request_config: config_info;
}

exception Ocsigen_Is_a_directory of request


type answer =
  | Ext_do_nothing
  (** I don't want to do anything *)
  | Ext_found of (unit -> Ocsigen_http_frame.result Lwt.t)
  (** "OK stop! I will take the page.
      You can start the following request of the same pipelined connection.
      Here is the function to generate the page".
      The extension must return Ext_found as soon as possible
      when it is sure it is safe to start next request.
      Usually as soon as you know that the result will be Ext_found.
      But in some case, for example proxies, you don't want the request of
      one connection to be handled in different order.
      In that case, wait to be sure that the new request will not
      overtake this one.
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
  (** Error. Do not try next extension (even filters),
      do not try next site,
      do not try next host,
      do not .
      The integer is the HTTP error code, usally 403.
  *)
  | Ext_continue_with of (request * Ocsigen_cookies.cookieset * int)
  (** Used to modify the request before giving it to next extension.
      The extension returns the request_info (possibly modified)
      and a set of cookies if it wants to set or cookies
      ([!Ocsigen_cookies.Cookies.empty] for no cookies).
      You must add these cookies yourself in request_info if you
      want them to be seen by subsequent extensions,
      for example using {!Ocsigen_http_frame.compute_new_ri_cookies}.
      The integer is usually equal to the error code received
      from preceding extension (but you may want to modify it).
  *)
  | Ext_retry_with of request * Ocsigen_cookies.cookieset
  (** Used to retry all the extensions with a new request_info.
      The extension returns the request_info (possibly modified)
      and a set of cookies if it wants to set or cookies
      ([!Ocsigen_cookies.Cookies.empty] for no cookies).
      You must add these cookies yourself in request_info if you
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

type parse_fun = Simplexmlparser.xml list -> extension2

(** Type of the functions parsing the content of a <host> tag *)
type parse_host


(** Information received by extensions accepting userconf files.

    The parameter [localfiles_root] is an absolute path to the
    directory that the user is allowed to serve. This is used
    by staticmod, to disallow the user from allowing access to
    outside of this directory
*)
type userconf_info = {
  localfiles_root : string;
}

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
type parse_config =
  virtual_hosts -> config_info -> parse_config_aux
and parse_config_user =
  userconf_info -> parse_config
and parse_config_aux =
    Url.path -> parse_host ->
    (parse_fun -> Simplexmlparser.xml ->
     extension
    )


(** For each extension generating pages, we register its name and six functions:
    - a function [fun_site] of type [parse_config]. This function
    will be responsible for handling the options of the configuration
    files that are recognized by the extension, and potentially generating
    a page.
    - a function [user_fun_site] of type [parse_user_config] which has the
    same role as [fun_site], but inside userconf files. Specify nothing
    if your extension is disallowed in userconf files. Otherwise, compared
    to [fun_site], you can selectively disallow some options,
    as [user_fun_site] must define only safe options (for example it is not
    safe to allow such options to load a cmo specified by a user, or to
    execute a program, as this program will be executed by ocsigen's user).
    Note that [user_fun_site] will be called for every request, whereas the
    [fun_site] is called only when starting or reloading the server.
    - a function [begin_init] that will be called at the beginning
    of the initialisation phase of each site, and each time the config file is
    reloaded.
    - a function [end_init] that will be called at the end of the initialisation
    phase of each site
    - a function [init_fun] that will be called just before registering the
    extension, taking as parameter the configuration options between
    [<extension>] and [</extension>]. This allows to give configuration options
    to extensions. If no function is supplied, the extension is supposed to
    accept no option (and loading will fail if an option is supplied)
    See <<a_api module="Ocsigen_extensions.Configuration" | val process_elements >> for
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
val register_extension :
  name:string ->
  ?fun_site:parse_config ->
  ?user_fun_site:parse_config_user ->
  ?begin_init:(unit -> unit) ->
  ?end_init:(unit -> unit) ->
  ?init_fun:(Simplexmlparser.xml list -> unit) ->
  ?exn_handler:(exn -> string) ->
  ?respect_pipeline:bool ->
  unit -> unit


(** This modules contains types and constructor for the description of XML
    configurations and the accordingly parsing. *)
module Configuration : sig

  (** Specification of a XML element. *)
  type element

  (** Specification of a XML attribute. *)
  type attribute

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
  val element :
    name:string ->
    ?obligatory:bool ->
    ?init:(unit -> unit) ->
    ?elements:element list ->
    ?attributes:attribute list ->
    ?pcdata:(string -> unit) ->
    ?other_elements:(string -> (string * string) list -> Simplexmlparser.xml list -> unit) ->
    ?other_attributes:(string -> string -> unit) ->
    unit -> element

  (** [attribute ~name f] create a specification of a XML attribute.
      @param name The name of the XML attribute
      @param obligatory Whether the attribute is obligatory ([false] by default)
      @param f Function to be applied on the value string of the attribute
  *)
  val attribute :
    name:string ->
    ?obligatory:bool ->
    (string -> unit) ->
    attribute

  (** Process an XML element by the specifications.
      @param in_tag Name of the enclosing XML tag (just for generating error messages)
      @param elements Specifications of the child elements
      @param pcdata Function to be applied on the PCDATA ([ignore_blank_pcdata] by default)
      @param other_elements Optional function to be applied on unexpected child elements
      @raise Error_in_config_file If an element (resp. attribute) occurs
        which is not specified by the [element] (resp. [attribute]) parameter
        and no function [other_elements] (resp. other_attributes) is provided
  *)
  val process_element :
    in_tag:string ->
    elements:element list ->
    ?pcdata:(string -> unit) ->
    ?other_elements:(string -> (string * string) list -> Simplexmlparser.xml list -> unit) ->
    Simplexmlparser.xml -> unit

  (** Application of [process_element] on a list of XML elements. *)
  val process_elements :
    in_tag:string ->
    elements:element list ->
    ?pcdata:(string -> unit) ->
    ?other_elements:(string -> (string * string) list -> Simplexmlparser.xml list -> unit) ->
    ?init:(unit -> unit) ->
    Simplexmlparser.xml list -> unit

  (** The specification for ignoring blank PCDATA ('\n', '\r', ' ', '\t') and failing
      otherwise (a reasonable default). *)
  val ignore_blank_pcdata : in_tag:string -> string -> unit

end

(** Returns the hostname to be used for absolute links or redirections.
    It is either the Host header or the hostname set in
    the configuration file. *)
val get_hostname : request -> string

(** Returns the port to be used for absolute links or redirections.
    It is either the port the server is listening at or the default port set in
    the configuration file. *)
val get_port : request -> int

(** Parsing URLs.
    This allows to modify the URL in the request_info.
    (to be used for example with Ext_retry_with or Ext_continue_with)
*)
val ri_of_url : ?full_rewrite:bool -> string -> request_info -> request_info


(** {3 User directories} *)

(** Exception raised when an non-existing user is found *)

exception NoSuchUser

(** The type for string that may contain a $u(...) *)
type ud_string

val parse_user_dir : string -> ud_string

val replace_user_dir : Netstring_pcre.regexp -> ud_string -> string -> string
(** raises [Not_found] is the directory does not exist *)



(** {3 Regular expressions for redirections} *)
exception Not_concerned

val find_redirection :
  Netstring_pcre.regexp ->
  bool ->
  string ->
  bool ->
  string option -> int -> string option -> string -> string -> string


(** {3 Extending server commands} *)
exception Unknown_command

(** Use a prefix for all your commands when you want to create
    extension-specific commands.
    For example if the prefix is "myextension" and the commande "blah",
    the actual command to be written by the user is "myextension:blah".
    Give as parameter the function that will parse the command and do an action.
    Its first parameter is the full command as a string.
    The second one is the command without prefix, split by word.
    It must raise [ocsigen_extensions.Unknown_command] if it does
    not recognize the command.
*)
val register_command_function :
  ?prefix:string -> (string -> string list -> unit Lwt.t) -> unit

(** Equivalent to a suit of <site></site> in XML configuration file *)
val make_site :
  path:Url.path -> ?charset:Ocsigen_charset_mime.charset -> ?closure:extension list -> extension2

(**/**)
val get_command_function :
  unit -> (?prefix:string -> string -> string list -> unit Lwt.t)

(**/**)
(**/**)

val make_parse_config : Url.path -> parse_config_aux -> parse_fun

val parse_config_item : parse_config
val parse_user_site_item : parse_config_user

val set_hosts : (virtual_hosts * config_info * extension2) list -> unit
val get_hosts : unit -> (virtual_hosts * config_info * extension2) list

(** Compute the result to be sent to the client,
    by trying all extensions according the configuration file.
*)
val compute_result :
  ?previous_cookies:Ocsigen_cookies.cookieset ->
  ?awake_next_request:bool ->
  request_info -> Ocsigen_http_frame.result Lwt.t

(** Profiling *)
val get_number_of_connected : unit -> int
val get_number_of_connected : unit -> int


(** Server internal functions: *)
val incr_connected : int -> unit
val decr_connected : unit -> unit Lwt.t
val wait_fewer_connected : int -> unit Lwt.t

val during_initialisation : unit -> bool
val start_initialisation : unit -> unit
val end_initialisation : unit -> unit
val get_numberofreloads : unit -> int

val get_init_exn_handler : unit -> exn -> string

val set_config : Simplexmlparser.xml list -> unit

val client_of_connection : Ocsigen_http_com.connection -> client

val get_server_address : request_info -> Unix.inet_addr * int

val sockets : Lwt_unix.file_descr list ref
val sslsockets : Lwt_unix.file_descr list ref
