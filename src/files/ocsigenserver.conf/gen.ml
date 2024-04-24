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
  | s -> "_" ^ s ^ "_"

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
  | s -> "_" ^ s ^ "_"

let () =
  let arg = if Array.length Sys.argv > 1 then Sys.argv.(1) else "" in
  let options =
    match arg with "local" -> local_options | _ -> sample_options
  in
  print_endline (interpolate options conf_in)
