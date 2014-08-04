open Lwt

(* This is an initialization of global variable before configuration of
 * extension. For example, if the mime file is specified after the configuration
 * of Staticmod, the server delivered a binary file (even if the file is HTML)
 *)

let () =
  Ocsigen_config.set_logdir "/home/dinosaure/bin/ocsigenserver/local/var/log";
  Ocsigen_config.set_datadir "/home/dinosaure/bin/ocsigenserver/local/var/lib";
  Ocsigen_config.set_command_pipe
    "/home/dinosaure/bin/ocsigenserver/local/var/run/ocsigenserver_command";
  Ocsigen_config.set_mimefile
    "/home/dinosaure/bin/ocsigenserver/src/files/mime.types"

(* An extension has a default configuration of server. This
 * configuration is moved from extensions to extensions by dispatch (see
 * default `~connector` of `Ocsigen_server.start_server`), and extension can
 * modify this config.
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

(* An extension has a compute function for request. This function is associated
 * with an initial `config_info` and a virtual host. When the request matches
 * the virtual host, we execute the function.
 * In this example, the compute function matches with all the hosts.
 *)

let staticmod_compute : (Ocsigen_extensions.virtual_hosts
                         * Ocsigen_extensions.config_info
                         * Ocsigen_extensions.extension2) =
  ([Ocsigen_extensions.VirtualHost.make
      ~host:"*"
      ~pattern:(Netstring_pcre.regexp ".*$") ()], staticmod_conf,
   (* first argument is `awake` for pipeline
    * and second argument is cookies *)
   (fun _ _ ri -> Printf.printf "My Staticmod\n%!";
     Staticmod.gen ~usermode:None (Staticmod.Dir "/home/dinosaure/bin/ocsigenserver/local/var/www/") ri
     >>= fun res -> Lwt.return (res, Ocsigen_cookies.Cookies.empty)))

(* This function has the same way of `Ocsigen_parseconfig.extract_info`.
 * Indeed, it's now optional to use the configuration file specifying the
 * `~configuration` parameter of `Ocsigen_server.start_server`.
 *
 * The structure is not explicit (since it's a tuple). Comments are there to say
 * what it's but it should be something more readable.
 *)

let server_conf =
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
  Ocsigen_server.start_server ~configuration:[ server_conf ] ()
