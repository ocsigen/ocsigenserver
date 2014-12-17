open Lwt

(** Global variables should be initialized before extensions.

    For example, if the mime file is specified after the configuration
    of Staticmod, the server delivered a binary file (even if the file
    is HTML).
*)
let global_initialization root =
  Ocsigen_config.set_logdir (root ^ "/local/var/log");
  Ocsigen_config.set_datadir (root ^ "/local/var/lib");
  Ocsigen_config.set_command_pipe
    (root ^ "/local/var/run/ocsigenserver_command");
  Ocsigen_config.set_mimefile
    (root ^ "/src/files/mime.types")

(** Default configuration of the server. This configuration is moved from
    extensions to extensions by dispatch (see default [~connector] of
    {!Ocsigen_server.start_server}), and extension can modify this config.

    same as:
    <host charset="utf-8" hostfilter="*"></host>
*)
let default_config () : Ocsigen_extensions.config_info =
  let open Ocsigen_extensions in
  {
    default_hostname = "localhost";
    default_httpport = 8080;
    default_httpsport = 4242;
    default_protocol_is_https = false;
    mime_assoc = Ocsigen_charset_mime.default_mime_assoc ();
    charset_assoc =
      Ocsigen_charset_mime.empty_charset_assoc ~default:"utf-8" ();
    default_directory_index = ["index.html"];
    list_directory_content = false;
    follow_symlinks = Ocsigen_extensions.FollowSymlinksIfOwnerMatch;
    do_not_serve_404 = Ocsigen_extensions.serve_everything;
    do_not_serve_403 = Ocsigen_extensions.serve_everything;
    uploaddir = Ocsigen_config.get_uploaddir ();
    maxuploadfilesize = Ocsigen_config.get_maxuploadfilesize ();
  }

(** An extension is composed of a list of virtual hosts, an initial configuration
    and a function that handle requests.

    When the request matches one of the virtual host, we execute the function.
    In this example, the function matches with all the hosts.

    same as:
    <host>
      <static dir="$root/local/var/www" />
    </host>
*)
let make_virtualhost1 root
  : Ocsigen_extensions.virtual_hosts
    * Ocsigen_extensions.config_info
    * Ocsigen_extensions.extension2
  =
  ( [Ocsigen_extensions.VirtualHost.make
       ~host:"*"
       ~pattern:(Netstring_pcre.regexp ".*$") ()]
  , default_config ()
  , fun awake cookies request_state ->
    Printf.printf "My Staticmod\n%!";
    Staticmod.gen
      ~usermode:None
      (Staticmod.Dir (root ^ "/local/var/www/"))
      request_state
    >>= fun res ->
    Lwt.return (res, cookies)
  )


(** same as:
    <host>
      <site path="ocsigenstuff" charset="utf-8">
        <static dir="$root/local/var/www/ocsigenstuff" />
      </site>
    </host>
*)
let make_virtualhost2 root
  : Ocsigen_extensions.virtual_hosts
    * Ocsigen_extensions.config_info
    * Ocsigen_extensions.extension2
  =
  ( [Ocsigen_extensions.VirtualHost.make
       ~host:"*"
       ~pattern:(Netstring_pcre.regexp ".*$") ()]
  , default_config ()
  , Ocsigen_extensions.make_site
      ~path:["ocsigenstuff"]
      ~charset:"utf-8"
      ~closure:[
        (fun request_state ->
           Printf.printf "My Staticmod Ocsigenstuff\n%!";
           Staticmod.gen
             ~usermode:None
             (Staticmod.Dir
                (root ^ "/local/var/www/ocsigenstuff/"))
             request_state)])

let condition_header name pattern ri =
  let open Ocsigen_http_frame in
  List.exists
    (fun a -> Netstring_pcre.string_match pattern a 0 <> None)
    (try Http_headers.find_all
           (Http_headers.name name)
           (Ocsigen_request_info.http_frame ri)
           .Ocsigen_http_frame.frame_header
           .Ocsigen_http_frame.Http_header.headers
     with Not_found -> [])

(** same as:
    <redirect fullurl=".+" dest="http://www.mozilla.org/fr/firefox/new/"
*)
let redirectmod_for_firefox request_state =
  Printf.printf "My RedirectMod\n%!";
  Redirectmod.gen
    (Redirectmod.Regexp
       (Netstring_pcre.regexp ".+",
        "https://www.mozilla.org/fr/firefox/new/",
        Ocsigen_lib_base.Yes, false))
    request_state

(** same as:
    <host>
      <site path="restricted-area" charset="utf-8">
        <if>
          <header name="User-Agent" regexp=".*Firefox.*" />
          <static dir="$root/local/var/www/firefox/" />
        </if>
        <else>
          <redirect fullurl=".+" dest="http://www.mozilla.org/fr/firefox/new/" />
        </else>
      </site>
    </host>
*)
let make_complex_example root
  : Ocsigen_extensions.virtual_hosts
    * Ocsigen_extensions.config_info
    * Ocsigen_extensions.extension2
  =
  (** same as:
      <static dir="$root/local/var/www/firefox/">
  *)
  let staticmod_for_firefox request_state =
    Printf.printf "My Staticmod for firefox\n%!";
    Staticmod.gen
      ~usermode:None
      (Staticmod.Dir (root ^ "/local/var/www/firefox/"))
      request_state
  in
  ( [Ocsigen_extensions.VirtualHost.make
       ~host:"*"
       ~pattern:(Netstring_pcre.regexp ".*$") ()]
  , default_config ()
  , Ocsigen_extensions.make_site
      ~path:["restricted-area"]
      ~charset:"utf-8"
      ~closure:[(fun request_state ->
        Printf.printf "My AccessControl\n%!";
        let open Ocsigen_extensions in
        match request_state with
        | Ocsigen_extensions.Req_found (ri, _)
        | Ocsigen_extensions.Req_not_found (_, ri) ->
          if (condition_header
                "User-Agent"
                (Netstring_pcre.regexp ".*Firefox.*"))
              ri.request_info
          then
            begin Printf.printf "> firefox\n%!";
              staticmod_for_firefox request_state end
          else
            begin Printf.printf "< firefox\n%!";
              redirectmod_for_firefox request_state end
      )])

let server_conf =
  Ocsigen_server_configuration.make
    [(Ocsigen_socket.All, 8080)]
    []

let () =
  let root =
    if Array.length Sys.argv >= 2
    then Sys.argv.(1)
    else Sys.getenv "HOME"
  in
  global_initialization root;
  Cgimod.init ~timeout:5 ();
  Ocsigen_extensions.set_hosts
    [ make_complex_example root;
      make_virtualhost2 root;
      make_virtualhost1 root ];
  Ocsigen_server.start_server ~configuration:[ server_conf ] ()
