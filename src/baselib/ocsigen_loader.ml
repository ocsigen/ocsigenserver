(* Ocsigen
 * http://www.ocsigen.org
 * File ocsigen_loader.ml
 * Copyright (C) 2008 Stéphane Glondu
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

open Ocsigen_lib

exception Dynlink_error of string * exn
exception Findlib_error of string * exn

let section = Logs.Src.create "ocsigen:dynlink"

(************************************************************************)

(* Translate .cmo/.cma extensions to .cmxs in native mode, and .cmxs
   to .cmo (.cma if the file exists) in bytecode mode. *)
let translate =
  if Ocsigen_config_static.is_native
  then
    fun filename ->
      if
        Filename.check_suffix filename ".cmo"
        || Filename.check_suffix filename ".cma"
      then Filename.chop_extension filename ^ ".cmxs"
      else filename
  else
    fun filename ->
      if Filename.check_suffix filename ".cmxs"
      then
        let filename = Filename.chop_extension filename in
        let cma = filename ^ ".cma" in
        if Sys.file_exists cma then cma else filename ^ ".cmo"
      else filename

(************************************************************************)
(* Loading files *)

let isloaded, addloaded =
  let set = ref String.Set.empty in
  (fun s -> String.Set.mem s !set), fun s -> set := String.Set.add s !set

module M = Map.Make (String)

let init_functions = ref M.empty

let get_init_on_load, set_init_on_load =
  let init_on_load = ref false in
  (fun () -> !init_on_load), fun b -> init_on_load := b

let loadfile pre post force file =
  let file = translate file in
  try
    if force
    then (
      pre ();
      Logs.info ~src:section (fun fmt ->
        fmt "Loading %s (will be reloaded every times)" file);
      try
        Dynlink_wrapper.loadfile file;
        post ()
      with e -> post (); raise e)
    else if not (isloaded file)
    then (
      pre ();
      Logs.info ~src:section (fun fmt -> fmt "Loading extension %s" file);
      (try
         Dynlink_wrapper.loadfile file;
         post ()
       with e -> post (); raise e);
      addloaded file)
    else
      Logs.info ~src:section (fun fmt -> fmt "Extension %s already loaded" file)
  with e -> raise (Dynlink_error (file, e))

let id () = ()

let loadfiles pre post force modules =
  let rec aux = function
    | [] -> ()
    | [m] -> loadfile pre post force m
    | m :: q -> loadfile id id false m; aux q
  in
  aux modules

let set_module_init_function name f =
  init_functions := M.add name [f] !init_functions;
  if get_init_on_load () then f ()

let add_module_init_function name f =
  let update = function
    | None (* no binding so far *) -> Some [f]
    | Some old -> Some (f :: old)
  in
  init_functions := M.update name update !init_functions;
  if get_init_on_load () then f ()

let init_module pre post force name =
  let f =
    try
      let l = List.rev @@ M.find name !init_functions in
      fun () -> List.iter (fun f -> f ()) l
    with Not_found ->
      Logs.info ~src:section (fun fmt ->
        fmt "No init function for named module %s." name);
      fun () -> ()
  in
  if force
  then (
    pre ();
    Logs.info ~src:section (fun fmt ->
      fmt "Initializing %s (will be initialized every time)" name);
    try f (); post () with e -> post (); raise e)
  else if not (isloaded name)
  then (
    pre ();
    Logs.info ~src:section (fun fmt -> fmt "Initializing module %s " name);
    (try f (); post () with e -> post (); raise e);
    addloaded name)
  else
    Logs.info ~src:section (fun fmt ->
      fmt "Module %s already initialized." name)

(************************************************************************)
(* Manipulating Findlib's search path *)

let () = Findlib.init ()
let ocsigen_search_path = ref []

let update_search_path () =
  match !ocsigen_search_path with
  | [] -> Findlib.init ()
  | x -> Findlib.init ~env_ocamlpath:(String.concat ":" x) ()

let get_ocamlpath = Findlib.search_path

let set_ocamlpath lp =
  ocsigen_search_path := lp;
  update_search_path ()

let add_ocamlpath p =
  ocsigen_search_path := p :: !ocsigen_search_path;
  update_search_path ()

(************************************************************************)
(* Using Findlib to locate files *)

let findfiles =
  let cmx = Re.Pcre.regexp ~flags:[`MULTILINE; `CASELESS] "\\.cmx($| |a)" in
  fun package ->
    try
      let preds =
        [ (if Ocsigen_config_static.is_native then "native" else "byte")
        ; "plugin"
        ; "mt" ]
      in
      let deps =
        List.filter
          (fun a ->
             not @@ String.Set.mem a Ocsigen_config_static.builtin_packages)
          (Findlib.package_deep_ancestors preds [package])
      in
      Logs.info ~src:section (fun fmt ->
        fmt "Dependencies of %s: %s" package (String.concat ", " deps));
      let rec aux = function
        | [] -> []
        | a :: q ->
            let mods =
              try
                let raw = Findlib.package_property preds a "archive" in
                (* Replacing .cmx/.cmxa by .cmxs *)
                let raw =
                  Ocsigen_lib.Netstring_pcre.global_replace cmx ".cmxs " raw
                in
                List.filter (( <> ) "") (String.split ~multisep:true ' ' raw)
              with Not_found -> []
            in
            let base = Findlib.package_directory a in
            List.map (Findlib.resolve_path ~base) mods @ aux q
      in
      let res = aux deps in
      Logs.info ~src:section (fun fmt ->
        fmt "Needed: %s" (String.concat ", " res));
      res
    with e -> raise (Findlib_error (package, e))

(************************************************************************)
(* Error formatting *)

open Printf

let () =
  Printexc.register_printer @@ function
  | Dynlink_wrapper.Error e -> Some (Dynlink_wrapper.error_message e)
  | Dynlink_error (s, e) ->
      Some
        (sprintf "Dynlink error while loading %s: %s" s (Printexc.to_string e))
  | Findlib_error (s, Fl_package_base.No_such_package (s', msg)) ->
      let pkg =
        if s = s' then s else sprintf "%s [while trying to load %s]" s' s
      in
      let additional = if msg = "" then "" else sprintf " (%s)" msg in
      let msg =
        sprintf
          "Findlib package %s not found%s: maybe you forgot <findlib path=\"...\"/>?"
          pkg additional
      in
      Some msg
  | Findlib_error (s, e) ->
      Some
        (sprintf "Findlib error while handling %s: %s" s (Printexc.to_string e))
  | _ -> None
