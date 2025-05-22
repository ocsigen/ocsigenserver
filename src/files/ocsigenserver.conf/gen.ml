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
  {|(* Warning: this file has been generated - DO NOT MODIFY! *)

let version_number = _VERSION_
let config_file = ref _CONFIGFILE_
let is_native = Sys.backend_type = Sys.Native
let logdir = ref (Some _LOGDIR_)
let mimefile = ref _MIMEFILE_
let datadir = ref _DATADIR_
let bindir = ref _BINDIR_
let extdir = ref _EXTDIR_
let command_pipe = ref _COMMANDPIPE_
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
    "lwt_ssl,bytes,lwt.unix,logs,logs-syslog.unix,syslog-message,ipaddr,findlib,cryptokit,re,str,xml-light,dynlink,cohttp-eio,http"
  in
  let deps = ref [] in
  let cmd = "ocamlfind query -p-format -recursive " ^ packages in
  let inp = Unix.open_process_in cmd in
  (try
     while true do
       deps := input_line inp :: !deps
     done
   with End_of_file -> ());
  match Unix.close_process_in inp with
  | WEXITED 0 -> !deps @ extra_deps
  | _ -> failwith ("Command failed: " ^ cmd)

(* Encode a string as a string literal that can be included in an ocaml file. *)
let str = Printf.sprintf "%S"
let ( // ) = Filename.concat

let static_options = function
  | "VERSION" -> str version
  | "LOGDIR" -> str logdir
  | "DATADIR" -> str datadir
  | "BINDIR" -> str bindir
  | "EXTDIR" -> str (libdir () // "ocsigenserver" // "extensions")
  | "STATICPAGESDIR" -> str staticpagesdir
  | "UP" -> str uploaddir
  | "COMMANDPIPE" -> str commandpipe
  | "CONFIGDIR" -> str configdir
  | "CONFIGFILE" -> str (configdir // "ocsigenserver.conf")
  | "MIMEFILE" -> str (configdir // "mime.types")
  | "DEPS" -> String.concat ";" (List.map (Format.asprintf "%S") (deps ()))
  | _ as s -> failwith s

let sample_options = function
  | "PORT" -> string_of_int port
  | "LOGDIR" -> logdir
  | "DATADIR" -> datadir
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
