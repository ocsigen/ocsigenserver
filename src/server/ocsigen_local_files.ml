(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2009 Boris Yakobowski
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

(* Display of a local file or directory. Currently used in staticmod
   and eliom_predefmod *)

let section = Logs.Src.create "ocsigen:local-file"

exception Failed_403
exception Failed_404
exception NotReadableDirectory

(* Policies for following symlinks *)
type symlink_policy =
  stat:Unix.LargeFile.stats -> lstat:Unix.LargeFile.stats -> bool

let never_follow_symlinks : symlink_policy = fun ~stat:_ ~lstat:_ -> false

let follow_symlinks_if_owner_match : symlink_policy =
 fun ~stat ~lstat -> stat.Unix.LargeFile.st_uid = lstat.Unix.LargeFile.st_uid

(* checks that [filename] can be followed depending on the predicate
   [policy] which must receives as argument both the results
   of calling [stat] and [lstat] on filenam.
   If supplied, [stat] must be the result of calling [Unix.stat] on
   [filename] *)
let check_symlinks_aux
      filename
      ?(stat = Unix.LargeFile.stat filename)
      (policy : symlink_policy)
  =
  let lstat = Unix.LargeFile.lstat filename in
  if lstat.Unix.LargeFile.st_kind = Unix.S_LNK
  then policy ~stat ~lstat
  else true

(* Check that there are no invalid symlinks in the directories leading to
   [filename]. Paths upwards [no_check_for] are not checked. *)
let rec check_symlinks_parent_directories
          ~filename
          ~no_check_for
          (policy : symlink_policy)
  =
  if filename = "/" || filename = "." || Some filename = no_check_for
  then true
  else
    let dirname = Filename.dirname filename in
    check_symlinks_aux dirname policy
    && check_symlinks_parent_directories ~filename:dirname ~no_check_for policy

(* Check that [filename] can be reached according to the given
   symlink policy  *)
let check_symlinks ~no_check_for ~filename policy =
  let aux policy =
    if filename = "/"
    then
      (* The root cannot be a symlink, and this avoids some degenerate
         cases later on *)
      true
    else
      let filename =
        (* [filename] should start by at least a slash, as
           [Filename.is_relative filename] should be false. Hence the length
           should be at least 1 *)
        (* We remove an eventual trailing slash, in order to avoid a
           needless recursion in check_symlinks_parent_directories, and so
           that Unix.lstat returns the correct result (Unix.lstat "foo/" and
           Unix.lstat "foo" return two different results...)  *)
        let len = String.length filename - 1 in
        if filename.[len] = '/' then String.sub filename 0 len else filename
      in
      check_symlinks_aux filename policy
      && check_symlinks_parent_directories ~filename ~no_check_for policy
  in
  match policy with
  | `Always -> true
  | `No -> aux never_follow_symlinks
  | `Owner_match -> aux follow_symlinks_if_owner_match

let check_dotdot =
  let regexp = Ocsigen_lib.Netstring_pcre.regexp "(/\\.\\./)|(/\\.\\.$)" in
  fun ~filename ->
    (* We always reject .. in filenames.
       In URLs, .. have already been removed by the server,
       but the filename may come from somewhere else than URLs ... *)
    try
      ignore
        (Ocsigen_lib.Netstring_pcre.search_forward regexp filename 0
         : int * 'groups);
      false
    with Not_found -> true

let can_send filename request =
  let filename =
    Ocsigen_lib.Url.split_path filename
    |> Ocsigen_lib.Url.norm_path |> Ocsigen_lib.Url.join_path
  in
  Logs.info ~src:section (fun fmt ->
    fmt "checking if file %s can be sent" filename);
  let matches arg =
    Ocsigen_lib.Netstring_pcre.string_match
      (Ocsigen_extensions.do_not_serve_to_regexp arg)
      filename 0
    <> None
  in
  if matches request.Ocsigen_extensions.do_not_serve_403
  then (
    Logs.info ~src:section (fun fmt -> fmt "this file is forbidden");
    raise Failed_403)
  else if matches request.Ocsigen_extensions.do_not_serve_404
  then (
    Logs.info ~src:section (fun fmt -> fmt "this file must be hidden");
    raise Failed_404)

(* Return type of a request for a local file. The string argument
   represents the real file/directory to serve, eg. foo/index.html
   instead of foo *)
type resolved = RFile of string | RDir of string

(* given [filename], we search for it in the local filesystem and
   - we return ["filename/index.html"] if [filename] corresponds to
   a directory, ["filename/index.html"] is valid, and ["index.html"]
   is one possible index (trying all possible indexes in order)
   - we raise [Failed_404] if [filename] corresponds to a directory,
   no index exists and [list_dir_content] is false.
   Warning: this behaviour is not the same as Apache's but it corresponds
   to a missing service in Eliom (answers 404). This also allows to have
   an Eliom service after a "forbidden" directory
   - we raise [Failed_403] if [filename] is a symlink that must
   not be followed
   - raises [Failed_404] if [filename] does not exist, or is a special file
   - otherwise returns [filename]
*)
(* See also module Files in eliom.ml *)
let resolve
      ?no_check_for
      ~request:({Ocsigen_extensions.request_config; _} as request)
      ~filename
      ()
  =
  (* We only accept absolute filenames in daemon mode,
     as we do not really know what is the current directory *)
  let filename =
    if Filename.is_relative filename && Ocsigen_config.get_daemon ()
    then "/" ^ filename
    else filename
  in
  try
    Logs.info ~src:section (fun fmt -> fmt "Testing \"%s\"." filename);
    let stat = Unix.LargeFile.stat filename in
    let filename, stat =
      if stat.Unix.LargeFile.st_kind = Unix.S_DIR
      then
        if filename.[String.length filename - 1] <> '/'
        then (
          Logs.info
            ~src:
              (* In this case, [filename] is a directory but this is not visible in
             its name as there is no final slash. We signal this fact to
             Ocsigen, which will then issue a 301 redirection to "filename/" *)
              section (fun fmt -> fmt "LocalFiles: %s is a directory" filename);
          raise
            (Ocsigen_extensions.Ocsigen_is_dir
               (Ocsigen_extensions.new_url_of_directory_request request)))
        else
          let rec find_index = function
            | [] ->
                (* No suitable index, we try to list the directory *)
                if request_config.Ocsigen_extensions.list_directory_content
                then (
                  Logs.info ~src:section (fun fmt ->
                    fmt "Displaying directory content");
                  filename, stat)
                else (
                  Logs.info
                    ~src:
                      (* No suitable index *)
                      section (fun fmt -> fmt "No index and no listing");
                  raise NotReadableDirectory)
            | e :: q -> (
                let index = filename ^ e in
                Logs.info ~src:section (fun fmt ->
                  fmt "Testing \"%s\" as possible index." index);
                try index, Unix.LargeFile.stat index
                with Unix.Unix_error (Unix.ENOENT, _, _) -> find_index q)
          in
          find_index request_config.Ocsigen_extensions.default_directory_index
      else filename, stat
    in
    if not (check_dotdot ~filename)
    then (
      Logs.info ~src:section (fun fmt ->
        fmt "Filenames cannot contain .. as in \"%s\"." filename);
      raise Failed_403)
    else if
      check_symlinks ~filename ~no_check_for
        request_config.Ocsigen_extensions.follow_symlinks
    then (
      can_send filename request_config;
      (* If the previous function did not fail, we are authorized to
         send this file *)
      Logs.info ~src:section (fun fmt -> fmt "Returning \"%s\"." filename);
      if stat.Unix.LargeFile.st_kind = Unix.S_REG
      then RFile filename
      else if stat.Unix.LargeFile.st_kind = Unix.S_DIR
      then RDir filename
      else raise Failed_404)
    else (
      Logs.info
        ~src:
          (* [filename] is accessed through as symlink which we should not
         follow according to the current policy *)
          section (fun fmt -> fmt "Failed symlink check for \"%s\"." filename);
      raise Failed_403)
  with
  (* We can get an EACCESS here, if are missing some rights on a directory *)
  | Unix.Unix_error (Unix.EACCES, _, _) -> raise Failed_403
  | Unix.Unix_error (Unix.ENOENT, _, _) -> raise Failed_404
