open Lwt

let serve_everything : Ocsigen_extensions.do_not_serve =
  let open Ocsigen_extensions in
  {
    do_not_serve_regexps = [];
    do_not_serve_files = [];
    do_not_serve_extensions = [];
  }

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
    do_not_serve_404 = serve_everything;
    do_not_serve_403 = serve_everything;
    uploaddir = Ocsigen_config.get_uploaddir ();
    maxuploadfilesize = Ocsigen_config.get_maxuploadfilesize ();
  }

let staticmod_compute : (Ocsigen_extensions.virtual_hosts
                         * Ocsigen_extensions.config_info
                         * Ocsigen_extensions.extension2) =
  ([("*", Netstring_pcre.regexp ".*$", None)], staticmod_conf,
   (* first argument is `awake` for pipeline
    * and second argument is cookies *)
   (fun _ _ ri -> Printf.printf "My Staticmod\n%!";
     Staticmod.gen ~usermode:None (Staticmod.Dir "/home/dinosaure") ri
     >>= fun res -> Lwt.return (res, Ocsigen_cookies.Cookies.empty)))

let server_conf () =

  Ocsigen_config.set_logdir "/home/dinosaure/bin/ocsigenserver/local/var/log";
  Ocsigen_config.set_datadir "/home/dinosaure/bin/ocsigenserver/local/var/lib";
  Ocsigen_config.set_command_pipe
    "/home/dinosaure/bin/ocsigenserver/local/var/run/ocsigenserver_command";
  Ocsigen_config.set_mimefile
    "/home/dinosaure/bin/ocsigenserver/src/files/mime.types";

  ( (* User, Group *)
    (None, None),
    (* SSL, ports, SSL ports *)
    (None, [ (Ocsigen_parseconfig.All, 8080) ], []),
    (* minimum thread, maximum thread *)
    (Ocsigen_config.get_minthreads (), Ocsigen_config.get_maxthreads ()))

let () =
  Cgimod.init ~timeout:5 ();
  Deflatemod.init ~level:5 ~size:1024 ();
  Ocsigen_extensions.set_hosts [ staticmod_compute ];
  Ocsigen_server.start_server ~configuration:[ server_conf () ] ()
