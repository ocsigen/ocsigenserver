(* OASIS_START *)
(* DO NOT EDIT (digest: a49834815f8e58268967a57d39f43b6d) *)
module OASISGettext = struct
(* # 22 "src/oasis/OASISGettext.ml" *)


  let ns_ str =
    str


  let s_ str =
    str


  let f_ (str: ('a, 'b, 'c, 'd) format4) =
    str


  let fn_ fmt1 fmt2 n =
    if n = 1 then
      fmt1^^""
    else
      fmt2^^""


  let init =
    []


end

module OASISExpr = struct
(* # 22 "src/oasis/OASISExpr.ml" *)





  open OASISGettext


  type test = string


  type flag = string


  type t =
    | EBool of bool
    | ENot of t
    | EAnd of t * t
    | EOr of t * t
    | EFlag of flag
    | ETest of test * string



  type 'a choices = (t * 'a) list


  let eval var_get t =
    let rec eval' =
      function
        | EBool b ->
            b

        | ENot e ->
            not (eval' e)

        | EAnd (e1, e2) ->
            (eval' e1) && (eval' e2)

        | EOr (e1, e2) ->
            (eval' e1) || (eval' e2)

        | EFlag nm ->
            let v =
              var_get nm
            in
              assert(v = "true" || v = "false");
              (v = "true")

        | ETest (nm, vl) ->
            let v =
              var_get nm
            in
              (v = vl)
    in
      eval' t


  let choose ?printer ?name var_get lst =
    let rec choose_aux =
      function
        | (cond, vl) :: tl ->
            if eval var_get cond then
              vl
            else
              choose_aux tl
        | [] ->
            let str_lst =
              if lst = [] then
                s_ "<empty>"
              else
                String.concat
                  (s_ ", ")
                  (List.map
                     (fun (cond, vl) ->
                        match printer with
                          | Some p -> p vl
                          | None -> s_ "<no printer>")
                     lst)
            in
              match name with
                | Some nm ->
                    failwith
                      (Printf.sprintf
                         (f_ "No result for the choice list '%s': %s")
                         nm str_lst)
                | None ->
                    failwith
                      (Printf.sprintf
                         (f_ "No result for a choice list: %s")
                         str_lst)
    in
      choose_aux (List.rev lst)


end


# 132 "myocamlbuild.ml"
module BaseEnvLight = struct
(* # 22 "src/base/BaseEnvLight.ml" *)


  module MapString = Map.Make(String)


  type t = string MapString.t


  let default_filename =
    Filename.concat
      (Sys.getcwd ())
      "setup.data"


  let load ?(allow_empty=false) ?(filename=default_filename) () =
    if Sys.file_exists filename then
      begin
        let chn =
          open_in_bin filename
        in
        let st =
          Stream.of_channel chn
        in
        let line =
          ref 1
        in
        let st_line =
          Stream.from
            (fun _ ->
               try
                 match Stream.next st with
                   | '\n' -> incr line; Some '\n'
                   | c -> Some c
               with Stream.Failure -> None)
        in
        let lexer =
          Genlex.make_lexer ["="] st_line
        in
        let rec read_file mp =
          match Stream.npeek 3 lexer with
            | [Genlex.Ident nm; Genlex.Kwd "="; Genlex.String value] ->
                Stream.junk lexer;
                Stream.junk lexer;
                Stream.junk lexer;
                read_file (MapString.add nm value mp)
            | [] ->
                mp
            | _ ->
                failwith
                  (Printf.sprintf
                     "Malformed data file '%s' line %d"
                     filename !line)
        in
        let mp =
          read_file MapString.empty
        in
          close_in chn;
          mp
      end
    else if allow_empty then
      begin
        MapString.empty
      end
    else
      begin
        failwith
          (Printf.sprintf
             "Unable to load environment, the file '%s' doesn't exist."
             filename)
      end


  let rec var_expand str env =
    let buff =
      Buffer.create ((String.length str) * 2)
    in
      Buffer.add_substitute
        buff
        (fun var ->
           try
             var_expand (MapString.find var env) env
           with Not_found ->
             failwith
               (Printf.sprintf
                  "No variable %s defined when trying to expand %S."
                  var
                  str))
        str;
      Buffer.contents buff


  let var_get name env =
    var_expand (MapString.find name env) env


  let var_choose lst env =
    OASISExpr.choose
      (fun nm -> var_get nm env)
      lst
end


# 237 "myocamlbuild.ml"
module MyOCamlbuildFindlib = struct
(* # 22 "src/plugins/ocamlbuild/MyOCamlbuildFindlib.ml" *)


  (** OCamlbuild extension, copied from
    * http://brion.inria.fr/gallium/index.php/Using_ocamlfind_with_ocamlbuild
    * by N. Pouillard and others
    *
    * Updated on 2009/02/28
    *
    * Modified by Sylvain Le Gall
    *)
  open Ocamlbuild_plugin


  (* these functions are not really officially exported *)
  let run_and_read =
    Ocamlbuild_pack.My_unix.run_and_read


  let blank_sep_strings =
    Ocamlbuild_pack.Lexers.blank_sep_strings


  let exec_from_conf exec =
    let exec =
      let env_filename = Pathname.basename BaseEnvLight.default_filename in
      let env = BaseEnvLight.load ~filename:env_filename ~allow_empty:true () in
      try
        BaseEnvLight.var_get exec env
      with Not_found ->
        Printf.eprintf "W: Cannot get variable %s\n" exec;
        exec
    in
    let fix_win32 str =
      if Sys.os_type = "Win32" then begin
        let buff = Buffer.create (String.length str) in
        (* Adapt for windowsi, ocamlbuild + win32 has a hard time to handle '\\'.
         *)
        String.iter
          (fun c -> Buffer.add_char buff (if c = '\\' then '/' else c))
          str;
        Buffer.contents buff
      end else begin
        str
      end
    in
      fix_win32 exec

  let split s ch =
    let buf = Buffer.create 13 in
    let x = ref [] in
    let flush () =
      x := (Buffer.contents buf) :: !x;
      Buffer.clear buf
    in
      String.iter
        (fun c ->
           if c = ch then
             flush ()
           else
             Buffer.add_char buf c)
        s;
      flush ();
      List.rev !x


  let split_nl s = split s '\n'


  let before_space s =
    try
      String.before s (String.index s ' ')
    with Not_found -> s

  (* ocamlfind command *)
  let ocamlfind x = S[Sh (exec_from_conf "ocamlfind"); x]

  (* This lists all supported packages. *)
  let find_packages () =
    List.map before_space (split_nl & run_and_read "ocamlfind list")


  (* Mock to list available syntaxes. *)
  let find_syntaxes () = ["camlp4o"; "camlp4r"]


  let well_known_syntax = [
    "camlp4.quotations.o";
    "camlp4.quotations.r";
    "camlp4.exceptiontracer";
    "camlp4.extend";
    "camlp4.foldgenerator";
    "camlp4.listcomprehension";
    "camlp4.locationstripper";
    "camlp4.macro";
    "camlp4.mapgenerator";
    "camlp4.metagenerator";
    "camlp4.profiler";
    "camlp4.tracer"
  ]


  let dispatch =
    function
      | After_options ->
          (* By using Before_options one let command line options have an higher
           * priority on the contrary using After_options will guarantee to have
           * the higher priority override default commands by ocamlfind ones *)
          Options.ocamlc     := ocamlfind & A"ocamlc";
          Options.ocamlopt   := ocamlfind & A"ocamlopt";
          Options.ocamldep   := ocamlfind & A"ocamldep";
          Options.ocamldoc   := ocamlfind & A"ocamldoc";
          Options.ocamlmktop := ocamlfind & A"ocamlmktop";
          Options.ocamlmklib := ocamlfind & A"ocamlmklib"

      | After_rules ->

          (* When one link an OCaml library/binary/package, one should use
           * -linkpkg *)
          flag ["ocaml"; "link"; "program"] & A"-linkpkg";

          (* For each ocamlfind package one inject the -package option when
           * compiling, computing dependencies, generating documentation and
           * linking. *)
          List.iter
            begin fun pkg ->
              let base_args = [A"-package"; A pkg] in
              (* TODO: consider how to really choose camlp4o or camlp4r. *)
              let syn_args = [A"-syntax"; A "camlp4o"] in
              let args =
              (* Heuristic to identify syntax extensions: whether they end in
                 ".syntax"; some might not.
               *)
                if Filename.check_suffix pkg "syntax" ||
                   List.mem pkg well_known_syntax then
                  syn_args @ base_args
                else
                  base_args
              in
              flag ["ocaml"; "compile";  "pkg_"^pkg] & S args;
              flag ["ocaml"; "ocamldep"; "pkg_"^pkg] & S args;
              flag ["ocaml"; "doc";      "pkg_"^pkg] & S args;
              flag ["ocaml"; "link";     "pkg_"^pkg] & S base_args;
              flag ["ocaml"; "infer_interface"; "pkg_"^pkg] & S args;
            end
            (find_packages ());

          (* Like -package but for extensions syntax. Morover -syntax is useless
           * when linking. *)
          List.iter begin fun syntax ->
          flag ["ocaml"; "compile";  "syntax_"^syntax] & S[A"-syntax"; A syntax];
          flag ["ocaml"; "ocamldep"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
          flag ["ocaml"; "doc";      "syntax_"^syntax] & S[A"-syntax"; A syntax];
          flag ["ocaml"; "infer_interface"; "syntax_"^syntax] &
                S[A"-syntax"; A syntax];
          end (find_syntaxes ());

          (* The default "thread" tag is not compatible with ocamlfind.
           * Indeed, the default rules add the "threads.cma" or "threads.cmxa"
           * options when using this tag. When using the "-linkpkg" option with
           * ocamlfind, this module will then be added twice on the command line.
           *
           * To solve this, one approach is to add the "-thread" option when using
           * the "threads" package using the previous plugin.
           *)
          flag ["ocaml"; "pkg_threads"; "compile"] (S[A "-thread"]);
          flag ["ocaml"; "pkg_threads"; "doc"] (S[A "-I"; A "+threads"]);
          flag ["ocaml"; "pkg_threads"; "link"] (S[A "-thread"]);
          flag ["ocaml"; "pkg_threads"; "infer_interface"] (S[A "-thread"]);
          flag ["ocaml"; "package(threads)"; "compile"] (S[A "-thread"]);
          flag ["ocaml"; "package(threads)"; "doc"] (S[A "-I"; A "+threads"]);
          flag ["ocaml"; "package(threads)"; "link"] (S[A "-thread"]);
          flag ["ocaml"; "package(threads)"; "infer_interface"] (S[A "-thread"]);

      | _ ->
          ()
end

module MyOCamlbuildBase = struct
(* # 22 "src/plugins/ocamlbuild/MyOCamlbuildBase.ml" *)


  (** Base functions for writing myocamlbuild.ml
      @author Sylvain Le Gall
    *)





  open Ocamlbuild_plugin
  module OC = Ocamlbuild_pack.Ocaml_compiler


  type dir = string
  type file = string
  type name = string
  type tag = string


(* # 62 "src/plugins/ocamlbuild/MyOCamlbuildBase.ml" *)


  type t =
      {
        lib_ocaml: (name * dir list * string list) list;
        lib_c:     (name * dir * file list) list;
        flags:     (tag list * (spec OASISExpr.choices)) list;
        (* Replace the 'dir: include' from _tags by a precise interdepends in
         * directory.
         *)
        includes:  (dir * dir list) list;
      }


  let env_filename =
    Pathname.basename
      BaseEnvLight.default_filename


  let dispatch_combine lst =
    fun e ->
      List.iter
        (fun dispatch -> dispatch e)
        lst


  let tag_libstubs nm =
    "use_lib"^nm^"_stubs"


  let nm_libstubs nm =
    nm^"_stubs"


  let dispatch t e =
    let env =
      BaseEnvLight.load
        ~filename:env_filename
        ~allow_empty:true
        ()
    in
      match e with
        | Before_options ->
            let no_trailing_dot s =
              if String.length s >= 1 && s.[0] = '.' then
                String.sub s 1 ((String.length s) - 1)
              else
                s
            in
              List.iter
                (fun (opt, var) ->
                   try
                     opt := no_trailing_dot (BaseEnvLight.var_get var env)
                   with Not_found ->
                     Printf.eprintf "W: Cannot get variable %s\n" var)
                [
                  Options.ext_obj, "ext_obj";
                  Options.ext_lib, "ext_lib";
                  Options.ext_dll, "ext_dll";
                ]

        | After_rules ->
            (* Declare OCaml libraries *)
            List.iter
              (function
                 | nm, [], intf_modules ->
                     ocaml_lib nm;
                     let cmis =
                       List.map (fun m -> (String.uncapitalize m) ^ ".cmi")
                                intf_modules in
                     dep ["ocaml"; "link"; "library"; "file:"^nm^".cma"] cmis
                 | nm, dir :: tl, intf_modules ->
                     ocaml_lib ~dir:dir (dir^"/"^nm);
                     List.iter
                       (fun dir ->
                          List.iter
                            (fun str ->
                               flag ["ocaml"; "use_"^nm; str] (S[A"-I"; P dir]))
                            ["compile"; "infer_interface"; "doc"])
                       tl;
                     let cmis =
                       List.map (fun m -> dir^"/"^(String.uncapitalize m)^".cmi")
                                intf_modules in
                     dep ["ocaml"; "link"; "library"; "file:"^dir^"/"^nm^".cma"]
                         cmis)
              t.lib_ocaml;

            (* Declare directories dependencies, replace "include" in _tags. *)
            List.iter
              (fun (dir, include_dirs) ->
                 Pathname.define_context dir include_dirs)
              t.includes;

            (* Declare C libraries *)
            List.iter
              (fun (lib, dir, headers) ->
                   (* Handle C part of library *)
                   flag ["link"; "library"; "ocaml"; "byte"; tag_libstubs lib]
                     (S[A"-dllib"; A("-l"^(nm_libstubs lib)); A"-cclib";
                        A("-l"^(nm_libstubs lib))]);

                   flag ["link"; "library"; "ocaml"; "native"; tag_libstubs lib]
                     (S[A"-cclib"; A("-l"^(nm_libstubs lib))]);

                   flag ["link"; "program"; "ocaml"; "byte"; tag_libstubs lib]
                     (S[A"-dllib"; A("dll"^(nm_libstubs lib))]);

                   (* When ocaml link something that use the C library, then one
                      need that file to be up to date.
                    *)
                   dep ["link"; "ocaml"; "program"; tag_libstubs lib]
                     [dir/"lib"^(nm_libstubs lib)^"."^(!Options.ext_lib)];

                   dep  ["compile"; "ocaml"; "program"; tag_libstubs lib]
                     [dir/"lib"^(nm_libstubs lib)^"."^(!Options.ext_lib)];

                   (* TODO: be more specific about what depends on headers *)
                   (* Depends on .h files *)
                   dep ["compile"; "c"]
                     headers;

                   (* Setup search path for lib *)
                   flag ["link"; "ocaml"; "use_"^lib]
                     (S[A"-I"; P(dir)]);
              )
              t.lib_c;

              (* Add flags *)
              List.iter
              (fun (tags, cond_specs) ->
                 let spec = BaseEnvLight.var_choose cond_specs env in
                 let rec eval_specs =
                   function
                     | S lst -> S (List.map eval_specs lst)
                     | A str -> A (BaseEnvLight.var_expand str env)
                     | spec -> spec
                 in
                   flag tags & (eval_specs spec))
              t.flags
        | _ ->
            ()


  let dispatch_default t =
    dispatch_combine
      [
        dispatch t;
        MyOCamlbuildFindlib.dispatch;
      ]


end


# 594 "myocamlbuild.ml"
open Ocamlbuild_plugin;;
let package_default =
  {
     MyOCamlbuildBase.lib_ocaml =
       [
          ("polytables", ["src/polytables"], []);
          ("commandline", ["src/commandline/yes"], []);
          ("nocommandline", ["src/commandline/no"], []);
          ("baselib", ["src/baselib"], []);
          ("base", ["src/baselib"], []);
          ("http", ["src/http"], []);
          ("cookies", ["src/http"], []);
          ("server", ["src/server"], []);
          ("ext", ["src/extensions"], []);
          ("ocsigenserver", ["src"], []);
          ("ocsipersist-sqlite",
            ["src/extensions/ocsipersist/ocsipersist-sqlite"],
            []);
          ("ocsipersist-dbm",
            ["src/extensions/ocsipersist/ocsipersist-dbm"],
            []);
          ("userconf", ["src/extensions/userconf"], []);
          ("rewritemod", ["src/extensions/rewritemod"], []);
          ("revproxy", ["src/extensions/revproxy"], []);
          ("redirectmod", ["src/extensions/redirectmod"], []);
          ("outputfilter", ["src/extensions/outputfilter"], []);
          ("ocsigen_comet", ["src/extensions/ocsigen_comet"], []);
          ("extensiontemplate", ["src/extensions/extensiontemplate"], []);
          ("extendconfiguration", ["src/extensions/extendconfiguration"], []);
          ("deflatemod", ["src/extensions/deflatemod"], []);
          ("cors", ["src/extensions/cors"], []);
          ("cgimod", ["src/extensions/cgimod"], []);
          ("authbasic", ["src/extensions/authbasic"], []);
          ("accesscontrol", ["src/extensions/accesscontrol"], []);
          ("staticmod", ["src/extensions/staticmod"], [])
       ];
     lib_c = [];
     flags = [];
     includes =
       [
          ("src/server", ["src/baselib"; "src/http"; "src/polytables"]);
          ("src/http", ["src/baselib"]);
          ("src/extensions/userconf", ["src"]);
          ("src/extensions/staticmod", ["src"]);
          ("src/extensions/rewritemod", ["src"]);
          ("src/extensions/revproxy", ["src"]);
          ("src/extensions/redirectmod", ["src"]);
          ("src/extensions/outputfilter", ["src"]);
          ("src/extensions/ocsipersist/ocsipersist-sqlite", ["src"]);
          ("src/extensions/ocsipersist/ocsipersist-dbm", ["src"]);
          ("src/extensions/ocsigen_comet", ["src"]);
          ("src/extensions/extensiontemplate", ["src"]);
          ("src/extensions/extendconfiguration", ["src"]);
          ("src/extensions/deflatemod", ["src"]);
          ("src/extensions/cors", ["src"]);
          ("src/extensions/cgimod", ["src"]);
          ("src/extensions/authbasic", ["src"]);
          ("src/extensions/accesscontrol", ["src"]);
          ("src",
            [
               "src/baselib";
               "src/extensions";
               "src/http";
               "src/polytables";
               "src/server"
            ]);
          ("bin", ["src/server"])
       ]
  }
  ;;

let dispatch_default = MyOCamlbuildBase.dispatch_default package_default;;

# 669 "myocamlbuild.ml"
(* OASIS_STOP *)

(* Substitution *)

let subst vars s =
  let buf = Buffer.create (String.length s) in
  let start = ref 0 in
  let last = ref 0 in
  let len = String.length s in
  while (!last < len - 4) do
    if not (s.[!last] = '%' && s.[!last + 1] = '%')
    then incr last
    else
      begin
        let start_subst = !last in
        let last_id = ref (!last + 2) in
        let stop = ref false in
        while (!last_id < len - 1 && not !stop) do
          if not (s.[!last_id] = '%' && s.[!last_id + 1] = '%') then begin
            if s.[!last_id] <> ' ' then (incr last_id) else
              (stop := true; last := !last_id)
          end else begin
            let id_start = start_subst + 2 in
            let id = String.sub s (id_start) (!last_id - id_start) in
            try
              let subst = List.assoc id vars in
              Buffer.add_substring buf s !start (start_subst - !start);
              Buffer.add_string buf subst;
              stop := true;
              start := !last_id + 2;
              last := !last_id + 2;
            with Not_found ->
              stop := true;
              last := !last_id
          end;
        done;
      end
  done;
  Buffer.add_substring buf s !start (len - !start);
  Buffer.contents buf;;

let subst_rule file args =
  rule file ~dep:(file^".in") ~prod:file
      (fun env build ->
         let ifile = env (file^".in") in
         let ic = open_in ifile in
         let ilen = in_channel_length ic in
         let content = String.create ilen in

         really_input ic content 0 ilen;

         let res = subst args content in
         Echo( [ res ], env file));;

let load_file file =
  let ic = open_in file in
  let n = in_channel_length ic in
  let s = String.create n in
  really_input ic s 0 n; close_in ic; s;;

let has_lwt_preemptive, preemptive =
  try let _ = Ocamlbuild_pack.Findlib.query "lwt.preemptive" in
      true, "Lwt_preemptive"
  with _ -> false, "Fake_preempt";;

let env_filename = Pathname.basename BaseEnvLight.default_filename
let env = BaseEnvLight.load ~filename:env_filename ~allow_empty:true ()

let commandline = bool_of_string (BaseEnvLight.var_get "commandline" env)
let is_native = bool_of_string (BaseEnvLight.var_get "is_native" env)
let native_dynlink = bool_of_string (BaseEnvLight.var_get "native_dynlink" env)
let preemptive = bool_of_string (BaseEnvLight.var_get "preemptive" env)
let libdir = BaseEnvLight.var_get "libdir" env
let pkg_name = BaseEnvLight.var_get "pkg_name" env
let sqlite3 = bool_of_string (BaseEnvLight.var_get "sqlite3" env)
let dbm = bool_of_string (BaseEnvLight.var_get "dbm" env)

let choose_rule ifiles ofile func =
  rule ofile
      ~deps:ifiles
      ~prod:ofile
      (fun env build ->
         let file = func ifiles in
         let content = load_file file in
         Echo( [ content ], env ofile));;

let link_rule source dest =
  rule (Printf.sprintf "%s -> %s" source dest) ~dep:source ~prod:dest
    (fun env _ -> Cmd (S [A"ln"; A"-f"; P (env source); P (env dest)]));;

let version =
  let fd = open_in "VERSION" in
  let bf = input_line fd in
  close_in fd; bf;;

(* Or
 * let version = BaseEnvLight.var_get "pkg_version" env
*)

let configuration = [
  ("_VERSION_", version);
  ("_WARNING_",
   "Warning: this file has been generated from ocsigen_config.ml.in");
  ("_LOG_DIR_", BaseEnvLight.var_get "logdir" env);
  ("_DATA_DIR_", BaseEnvLight.var_get "datadir" env);
  ("_BIN_DIR_", BaseEnvLight.var_get "bindir" env);
  ("_LIB_DIR_", libdir);
  ("_EXT_DIR_", libdir ^ "/" ^ pkg_name ^ "/extensions");
  ("_STATIC_PAGES_DIR_", BaseEnvLight.var_get "staticpagesdir" env);
  ("_UPLOAD_DIR_", "/tmp/");
  ("_OCSIGEN_USER_", BaseEnvLight.var_get "ocsigen_user" env);
  ("_OCSIGEN_GROUP_", BaseEnvLight.var_get "ocsigen_group" env);
  ("_PROJECT_NAME_", pkg_name);
  ("_COMMAND_PIPE_", BaseEnvLight.var_get "commandpipe" env);
  ("_CONFIG_DIR_", BaseEnvLight.var_get "sysconfdir" env);
  ("_PREEMPTIVE_", if preemptive then "Lwt_preemptive" else "Fake_preempt");
  ("_IS_NATIVE_", string_of_bool is_native);
  ("_NATIVE_DYNLINK_", string_of_bool native_dynlink);
];;

(* Parametric compilation *)

Ocamlbuild_plugin.dispatch (function
    | After_hygiene ->
      dispatch_default After_hygiene;
      subst_rule "src/baselib/ocsigen_config.ml" configuration;

      if native_dynlink
      then tag_file "src/baselib/dynlink_wrapper.ml"
          ["native_dynlink( " ^ (string_of_bool native_dynlink) ^ ")"];

      if commandline
      then
        begin
          tag_file "src/baselib/ocsigen_commandline.ml" ["use_commandline"];
          tag_file "bin/server_main.native" ["use_commandline"];
          tag_file "bin/server_main.byte" ["use_commandline"];
        end
      else
        begin
          tag_file "src/baselib/ocsigen_commandline.ml" ["use_nocommandline"];
          tag_file "bin/server_main.native" ["use_nocommandline"];
          tag_file "bin/server_main.byte" ["use_nocommandline"];
        end;

      if preemptive
      then
        begin
          flag ["ocaml"; "ocamldep"] (S [A "-package";
                                         A "lwt.preemptive"]);
          flag ["ocaml"; "compile"] (S [A "-thread";
                                        A "-package";
                                        A "lwt.preemptive"]);
          flag ["link"] (S [A "-thread";
                            A "-package";
                            A "lwt.preemptive"]);
        end;

      if sqlite3 then
        link_rule
          "src/extensions/ocsipersist/ocsipersist.mli"
          "src/extensions/ocsipersist/ocsipersist-sqlite/ocsipersist.mli";

      if dbm then
        link_rule
          "src/extensions/ocsipersist/ocsipersist.mli"
          "src/extensions/ocsipersist/ocsipersist-dbm/ocsipersist.mli";

      pflag ["ocaml"; "ocamldep"] "native_dynlink"
        (fun value ->
           S [A "-ppopt"; A "-let"; A "-ppopt"; A ("native_dynlink=" ^ value)]);
      pflag ["ocaml"; "compile"] "native_dynlink"
        (fun value ->
           S [A "-ppopt"; A "-let"; A "-ppopt"; A ("native_dynlink=" ^ value)]);
      pflag ["ocaml"; "doc"] "native_dynlink"
        (fun value ->
           S [A "-ppopt"; A "-let"; A "-ppopt"; A ("native_dynlink=" ^ value)]);

      ()
    | x -> dispatch_default x);;
