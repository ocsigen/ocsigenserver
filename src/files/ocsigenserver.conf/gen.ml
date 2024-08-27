let conf_in =
  {|<!-- -*- Mode: Xml -*- -->
<!-- This file is intentionnaly very basic. See http://ocsigen.org/ocsigenserver/manual/config
     a more elaborate one, as well as for comments on the various options -->
<!-- DO NOT MODIFY. This a generated file.
     Additional configurations sit in conf.d/ -->
<ocsigen>


  <server>

    <port>_PORT_</port>

    <logdir>_LOGDIR_</logdir>
    <datadir>_DATADIR_</datadir>
    _OCSIGENUSER_
    _OCSIGENGROUP_
    _COMMANDPIPE_
    _MIMEFILE_

    <charset>utf-8</charset>

    <findlib path="_METADIR_"/>_FINDLIBEXTRA_

    <extension findlib-package="_EXTPACKAGENAME_.staticmod"/>

    <!-- Inclusion of all external configuration files matching *.conf
         from the directory 'dir' (in alphabetical order): -->
    <extconf dir="_CONFIGDIR_/conf.d" />

    <host charset="utf-8" hostfilter="*">

      <site path="ocsigenstuff" charset="utf-8">
        <static dir="_STATICPAGESDIR_/ocsigenstuff" />
      </site>

      <static dir="_STATICPAGESDIR_" />

    </host>

  </server>

</ocsigen>|}

let conf_ml =
  {|
let version_number = "_VERSION_"
let config_file = ref "_CONFIGDIR_/ocsigenserver.conf"
let is_native = Sys.backend_type = Sys.Native
let logdir = ref (Some "_LOGDIR_")
let default_user = ref "_OCSIGENUSER_"
let default_group = ref "_OCSIGENGROUP_"
let mimefile = ref "_CONFIGDIR_/mime.types"
let datadir = ref "_DATADIR_"
let bindir = ref "_BINDIR_"
let extdir = ref "_EXTDIR_"
let command_pipe = ref "_COMMANDPIPE_"
let builtin_packages =
  List.fold_left
    (fun a s -> Ocsigen_lib.String.Set.add s a)
    Ocsigen_lib.String.Set.empty
    [_DEPS_]|}

let interpolate f s =
  let regexp = Str.regexp "_\\([A-Z]+\\)_" in
  let f s = f (Str.matched_group 1 s) in
  Str.global_substitute regexp f s

open Options

let libdir () =
  if libdir_set = 0
  then (
    let inp = Unix.open_process_in "ocamlfind printconf destdir" in
    let libdir = input_line inp in
    ignore (Unix.close_process_in inp);
    libdir)
  else libdir

let deps () =
  let extra_deps =
    [ "ocsigenserver.polytables"
    ; "ocsigenserver.cookies"
    ; "ocsigenserver.baselib.base"
    ; "ocsigenserver.baselib"
    ; "ocsigenserver.http"
    ; "ocsigenserver" ]
  in
  let packages =
    "lwt_ssl,bytes,lwt.unix,lwt_log,ipaddr,findlib,cryptokit,re,str,xml-light,dynlink,cohttp-lwt-unix,hmap"
  in
  let inp =
    Unix.open_process_in ("ocamlfind query -p-format -recursive " ^ packages)
  in
  let deps = ref [] in
  (try
     while true do
       deps := input_line inp :: !deps
     done
   with End_of_file -> ());
  ignore (Unix.close_process_in inp);
  !deps @ extra_deps

let static_options = function
  | "VERSION" -> version
  | "WARNING" -> "Warning: this file has been generated - DO NOT MODIFY!"
  | "LOGDIR" -> logdir
  | "DATADIR" -> datadir
  | "BINDIR" -> bindir
  | "EXTDIR" -> libdir () ^ "/ocsigenserver/extensions"
  | "STATICPAGESDIR" -> staticpagesdir
  | "UP" -> uploaddir
  | "OCSIGENUSER" -> ocsigenuser
  | "OCSIGENGROUP" -> ocsigengroup
  | "COMMANDPIPE" -> commandpipe
  | "CONFIGDIR" -> configdir
  | "DEPS" -> String.concat ";" (List.map (Format.asprintf "%S") (deps ()))
  | _ as s -> failwith s

let sample_options = function
  | "PORT" -> string_of_int port
  | "LOGDIR" -> logdir
  | "DATADIR" -> datadir
  | "OCSIGENUSER" -> "<user>" ^ ocsigenuser ^ "</user>"
  | "OCSIGENGROUP" -> "<group>" ^ ocsigengroup ^ "</group>"
  | "COMMANDPIPE" -> "<!-- <commandpipe>" ^ commandpipe ^ "</commandpipe> -->"
  | "MIMEFILE" -> "<!-- <mimefile>" ^ mimefile ^ "</mimefile> -->"
  | "LIBDIR" | "METADIR" -> libdir ()
  | "EXTPACKAGENAME" -> "ocsigenserver.ext"
  | "CONFIGDIR" -> configdir
  | "STATICPAGESDIR" -> staticpagesdir
  | "FINDLIBEXTRA" -> ""
  | _ as s -> failwith s

let local_options = function
  | "PORT" -> "8080"
  | "LOGDIR" -> src ^ "/local/var/log"
  | "DATADIR" -> src ^ "/local/var/lib"
  | "OCSIGENUSER" -> ""
  | "OCSIGENGROUP" -> ""
  | "COMMANDPIPE" ->
      "<commandpipe>" ^ src
      ^ "/local/var/run/ocsigenserver_command</commandpipe>"
  | "MIMEFILE" -> "<mimefile>" ^ src ^ "/src/files/mime.types</mimefile>"
  | "LIBDIR" | "METADIR" -> libdir ()
  | "EXTPACKAGENAME" -> "ocsigenserver.ext"
  | "CONFIGDIR" -> src ^ "/local/etc/ocsigenserver"
  | "STATICPAGESDIR" -> src ^ "/local/var/www"
  | "FINDLIBEXTRA" ->
      "<findlib path=\"" ^ src ^ "/src/files/\"/><findlib path=\"" ^ src
      ^ "/src/extensions/files/\"/>"
  | _ as s -> failwith s

let () =
  let arg = if Array.length Sys.argv > 1 then Sys.argv.(1) else "" in
  print_endline
  @@
  match arg with
  | "static.ml" -> interpolate static_options conf_ml
  | "local" -> interpolate local_options conf_in
  | "sample" -> interpolate sample_options conf_in
  | _ -> failwith arg
