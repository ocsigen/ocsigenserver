open Lwt

(** This is an initialization of global variable before configuration of
    extension. For example, if the mime file is specified after the
    configuration of Staticmod, the server delivered a binary file (even if the
    file is HTML)
*)
let () =
  Ocsigen_config.set_logdir "/home/dinosaure/bin/ocsigenserver/local/var/log";
  Ocsigen_config.set_datadir "/home/dinosaure/bin/ocsigenserver/local/var/lib";
  Ocsigen_config.set_command_pipe
    "/home/dinosaure/bin/ocsigenserver/local/var/run/ocsigenserver_command";
  Ocsigen_config.set_mimefile
    "/home/dinosaure/bin/ocsigenserver/src/files/mime.types"

(** An extension has a default configuration of server. This
    configuration is moved from extensions to extensions by dispatch (see
    default `~connector` of `Ocsigen_server.start_server`), and extension can
    modify this config.

    same as:
    <host charset="utf-8" hostfilter="*"></host>
*)
let staticmod_conf : Ocsigen_extensions.config_info =
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

let site_match request (site_path : string list) url =
  (* We are sure that there is no / at the end or beginning of site_path *)
  (* and no / at the beginning of url *)
  (* and no // or ../ inside both of them *)
  (* We return the subpath without / at beginning *)
  let open Ocsigen_extensions in
  let rec aux site_path url =
    match site_path, url with
    | [], [] -> raise (Ocsigen_Is_a_directory request)
    | [], p -> Some p
    | a::l, aa::ll when a = aa -> aux l ll
    | _ -> None
  in
  match site_path, url with
  | [], [] -> Some []
  | _ -> aux site_path url

let endpoint_of_extension awake cookies =
  let open Ocsigen_extensions in
  function
  | Req_found (ri, res) ->
    Lwt.return (Ext_found_continue_with' (res, ri), cookies)
  | Req_not_found (e, ri) ->
    Lwt.return
      (Ext_continue_with
         (ri, Ocsigen_cookies.Cookies.empty, e), cookies)

let make_site ~path ?charset ?(closure = endpoint_of_extension) =
  let open Ocsigen_extensions in
  let open Ocsigen_lib in
  fun awake cookies ->
    function
    | Req_found (ri, res) ->
      Lwt.return (Ext_found_continue_with' (res, ri), cookies)
    | Req_not_found (e, oldri) ->
      let oldri = match charset with
        | None -> oldri
        | Some charset ->
          { oldri
            with request_config =
                   { oldri.request_config
                     with charset_assoc =
                            Ocsigen_charset_mime.set_default_charset
                              oldri.request_config.charset_assoc charset } }
      in
      match site_match oldri path
              (Ocsigen_request_info.full_path oldri.request_info) with
      | None ->
        Ocsigen_messages.debug (fun () ->
            "site \""^
            (Url.string_of_url_path ~encode:true path)^
            "\" does not match url \""^
            (Url.string_of_url_path ~encode:true
               (Ocsigen_request_info.full_path oldri.request_info))^
            "\".");
        Lwt.return (Ext_next e, cookies)
      | Some sub_path ->
        Ocsigen_messages.debug (fun () ->
            "-------- site found: url \""^
            (Url.string_of_url_path ~encode:true
               (Ocsigen_request_info.full_path oldri.request_info))^
            "\" matches \""^
            (Url.string_of_url_path ~encode:true path)^"\".");
        let ri = {oldri with
                  request_info =
                    (Ocsigen_request_info.update oldri.request_info
                       ~sub_path:sub_path
                       ~sub_path_string:
                         (Url.string_of_url_path
                            ~encode:true sub_path) ()) }
        in
        closure awake cookies (Req_not_found (e, ri))
        >>= function
          (* After a site, we turn back to old ri *)
        | (Ext_stop_site (cs, err), cookies)
        | (Ext_continue_with (_, cs, err), cookies) ->
          Lwt.return
            (Ext_continue_with (oldri, cs, err), cookies)
        | (Ext_found_continue_with r, cookies) ->
          awake ();
          r () >>= fun (r', req) ->
          Lwt.return
            (Ext_found_continue_with' (r', oldri), cookies)
        | (Ext_found_continue_with' (r, req), cookies) ->
          Lwt.return
            (Ext_found_continue_with' (r, oldri), cookies)
        | (Ext_do_nothing, cookies) ->
          Lwt.return
            (Ext_continue_with (oldri,
                                Ocsigen_cookies.Cookies.empty,
                                e), cookies)
        | r -> Lwt.return r

(** same as:
    <host>
      <static dir="/home/dinosaure/bin/ocsigenserver/local/var/www" />
    </host>

    An extension has a compute function for request. This function is associated
    with an initial `config_info` and a virtual host. When the request matches
    the virtual host, we execute the function.
    In this example, the compute function matches with all the hosts.
*)
let staticmod : (Ocsigen_extensions.virtual_hosts
                 * Ocsigen_extensions.config_info
                 * Ocsigen_extensions.extension2) =
  ([Ocsigen_extensions.VirtualHost.make
      ~host:"*"
      ~pattern:(Netstring_pcre.regexp ".*$") ()], staticmod_conf,
   (fun _ _ request_state ->
     Printf.printf "My Staticmod\n%!";
     Staticmod.gen
       ~usermode:None
       (Staticmod.Dir "/home/dinosaure/bin/ocsigenserver/local/var/www/")
       request_state
     >>= fun res ->
     Lwt.return (res, Ocsigen_cookies.Cookies.empty)))

(** same as:
    <host>
      <site path="ocsigenstuff" charset="utf-8">
        <static dir="/home/dinosaure/bin/ocsigenserver/local/var/www/ocsigenstuff" />
      </site>
    </host>
*)
let ocsigenstuff : (Ocsigen_extensions.virtual_hosts
                    * Ocsigen_extensions.config_info
                    * Ocsigen_extensions.extension2) =
  ([Ocsigen_extensions.VirtualHost.make
      ~host:"*"
      ~pattern:(Netstring_pcre.regexp ".*$") ()], staticmod_conf,
   make_site
     ~path:["ocsigenstuff"]
     ~charset:"utf-8"
     ~closure:(fun _ _ request_state ->
         Printf.printf "My Staticmod Ocsigenstuff\n%!";
         Staticmod.gen
           ~usermode:None
           (Staticmod.Dir
              "/home/dinosaure/bin/ocsigenserver/local/var/www/ocsigenstuff/")
           request_state
         >>= fun res ->
         Lwt.return (res, Ocsigen_cookies.Cookies.empty)))

let condition_header name pattern =
  let open Ocsigen_http_frame in
  (fun ri ->
     List.exists
       (fun a -> Netstring_pcre.string_match pattern a 0 <> None)
       (try Http_headers.find_all
              (Http_headers.name name)
              (Ocsigen_request_info.http_frame ri)
              .Ocsigen_http_frame.frame_header
              .Ocsigen_http_frame.Http_header.headers
        with Not_found -> []))

(** same as:
    <redirect fullurl=".+" dest="http://www.mozilla.org/fr/firefox/new/"
*)
let redirectmod_for_firefox =
  (fun _ _ request_state ->
    Printf.printf "My RedirectMod\n%!";
    Redirectmod.gen
      (Redirectmod.Regexp
        (Netstring_pcre.regexp ".+",
         "https://www.mozilla.org/fr/firefox/new/",
         Ocsigen_lib_base.Yes, false))
      request_state
    >>= fun res ->
    Lwt.return (res, Ocsigen_cookies.Cookies.empty))

(** same as:
    <static dir="/home/dinosaure/bin/ocsigenserver/local/var/www/firefox/">
*)
let staticmod_for_firefox =
  (fun _ _ request_state ->
    Printf.printf "My Staticmod for firefox\n%!";
    Staticmod.gen
      ~usermode:None
      (Staticmod.Dir "/home/dinosaure/bin/ocsigenserver/local/var/www/firefox/")
      request_state
    >>= fun res ->
    Lwt.return (res, Ocsigen_cookies.Cookies.empty))

(** same as:
    <host>
      <site path="restricted-area" charset="utf-8">
        <if>
          <header name="User-Agent" regexp=".*Firefox.*" />
          <static dir="/home/dinosaure/bin/ocsigenserver/local/var/www/firefox/" />
        </if>
        <else>
          <redirect fullurl=".+" dest="http://www.mozilla.org/fr/firefox/new/" />
        </else>
      </site>
    </host>
*)
let accesscontrol : (Ocsigen_extensions.virtual_hosts
                     * Ocsigen_extensions.config_info
                     * Ocsigen_extensions.extension2) =
  ([Ocsigen_extensions.VirtualHost.make
      ~host:"*"
      ~pattern:(Netstring_pcre.regexp ".*$") ()], staticmod_conf,
   make_site
     ~path:["restricted-area"]
     ~charset:"utf-8"
     ~closure:(fun awake cookies request_state ->
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
             staticmod_for_firefox awake cookies request_state end
           else
             begin Printf.printf "< firefox\n%!";
             redirectmod_for_firefox awake cookies request_state end
      ))

let server_conf =
  Ocsigen_server_configuration.make
    [(Ocsigen_socket.All, 8080)]
    []

let () =
  Cgimod.init ~timeout:5 ();
  Deflatemod.init ~level:5 ~size:1024 ();
  Ocsigen_extensions.set_hosts [ accesscontrol; ocsigenstuff; staticmod ];
  Ocsigen_server.start_server ~configuration:[ server_conf ] ()
