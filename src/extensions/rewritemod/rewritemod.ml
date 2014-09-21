(* Ocsigen
 * http://www.ocsigen.org
 * Module rewritemod.ml
 * Copyright (C) 2008 Vincent Balat
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
(* Ocsigen extension for rewriteing URLs                                     *)
(* in the configuration file                                                 *)
(*****************************************************************************)
(*****************************************************************************)

(* IMPORTANT WARNING
   It is really basic for now:
    - rewrites only subpaths (and do not change get parameters)
    - changes only ri_sub_path and ri_sub_path_tring
   not ri_full_path and ri_full_path_string and ri_url_string and ri_url
   This is probably NOT what we want ...
*)



(* To compile it:
   ocamlfind ocamlc  -thread -package netstring-pcre,ocsigen -c extensiontemplate.ml

   Then load it dynamically from Ocsigen's config file:
   <extension module=".../rewritemod.cmo"/>

*)

open Lwt
open Ocsigen_extensions
open Simplexmlparser


exception Not_concerned


(*****************************************************************************)
(* The table of rewrites for each virtual server                             *)
type assockind =
  | Regexp of Netstring_pcre.regexp * string * bool



(*****************************************************************************)
(* Finding rewrites *)

let find_rewrite (Regexp (regexp, dest, fullrewrite)) suburl =
  (match Netstring_pcre.string_match regexp suburl 0 with
   | None -> raise Not_concerned
   | Some _ -> (* Matching regexp found! *)
     Netstring_pcre.global_replace regexp dest suburl), fullrewrite






(*****************************************************************************)
(** The function that will generate the pages from the request. *)
let gen regexp continue = function
  | Ocsigen_extensions.Req_found _ ->
    Lwt.return Ocsigen_extensions.Ext_do_nothing
  | Ocsigen_extensions.Req_not_found (err, ri) ->
    catch
      (* Is it a rewrite? *)
      (fun () ->
         Ocsigen_messages.debug2 "--Rewritemod: Is it a rewrite?";
         let redir, fullrewrite =
           let ri = ri.request_info in
           find_rewrite regexp
             (match Ocsigen_request_info.get_params_string ri with
              | None -> Ocsigen_request_info.sub_path_string ri
              | Some g -> (Ocsigen_request_info.sub_path_string ri) ^ "?" ^ g)
         in
         Ocsigen_messages.debug (fun () ->
             "--Rewritemod: YES! rewrite to: "^redir
             ^(if continue then " (and continue)" else " (and restart)"));
         if continue
         then
           return
             (Ext_continue_with
                ({ ri with request_info =
                             Ocsigen_extensions.ri_of_url
                               ~full_rewrite:fullrewrite
                               redir ri.request_info },
                 Ocsigen_cookies.Cookies.empty,
                 err)
             )
         else
           return
             (Ext_retry_with
                ({ ri with request_info =
                             Ocsigen_extensions.ri_of_url
                               ~full_rewrite:fullrewrite
                               redir ri.request_info },
                 Ocsigen_cookies.Cookies.empty)
             )
      )
      (function
        | Not_concerned -> return (Ext_next err)
        | e -> fail e)




(*****************************************************************************)

let parse_config element =
  let regexp = ref "" in
  let dest = ref None in
  let fullrewrite = ref false in
  let continue = ref false in
  Ocsigen_extensions.(
    Configuration.process_element
      ~in_tag:"host"
      ~elements:[Configuration.element
                   ~name:"rewrite"
                   ~attributes:[Configuration.attribute
                                  ~name:"regexp"
                                  ~obligatory:true
                                  (fun s -> regexp := s);
                                Configuration.attribute
                                  ~name:"url"
                                  (fun s -> dest := Some s);
                                Configuration.attribute
                                  ~name:"dest"
                                  (fun s -> dest := Some s);
                                Configuration.attribute
                                  ~name:"fullrewrite"
                                  (fun s -> fullrewrite := (s = "fullrewrite"
                                                            || s = "true"));
                                Configuration.attribute
                                  ~name:"continue"
                                  (fun s -> continue := (s = "continue"
                                                         || s = "true"));
                               ]
                   ()]
      element
  );
  match !dest with
  | None -> raise (Error_in_config_file "url attribute expected for <rewrite>")
  | Some dest ->
    gen (Regexp ((Netstring_pcre.regexp ("^"^ !regexp^"$")),
                 dest, !fullrewrite))
      !continue


(*****************************************************************************)
(** Registration of the extension *)
let () = register_extension
    ~name:"rewritemod"
    ~fun_site:(fun _ _ _ _ _ -> parse_config)
    ~user_fun_site:(fun _ _ _ _ _ _ -> parse_config)
    ()
