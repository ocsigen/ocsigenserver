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
open Ocsigen_extensions

(* Displaying of a local file or directory. Currently used in
   staticmod and eliom_predefmod*)

let section = Lwt_log.Section.make "ocsigen:local-file"
exception Failed_403
exception Failed_404
exception NotReadableDirectory


(* Policies for following symlinks *)
type symlink_policy =
  stat:Unix.LargeFile.stats -> lstat:Unix.LargeFile.stats -> bool

let never_follow_symlinks : symlink_policy =
  fun ~stat ~lstat -> false

let follow_symlinks_if_owner_match : symlink_policy =
  fun ~stat ~lstat ->
    stat.Unix.LargeFile.st_uid = lstat.Unix.LargeFile.st_uid


(* checks that [filename] can be followed depending on the predicate
   [policy] which must receives as argument both the results
   of calling [stat] and [lstat] on filenam.
   If supplied, [stat] must be the result of calling [Unix.stat] on
   [filename] *)
let check_symlinks_aux
    filename ?(stat=Unix.LargeFile.stat filename) (policy : symlink_policy) =
  let lstat = Unix.LargeFile.lstat filename in
  if lstat.Unix.LargeFile.st_kind = Unix.S_LNK then
    policy ~stat ~lstat
  else
    true

(* Check that there are no invalid symlinks in the directories leading to
   [filename]. Paths upwards [no_check_for] are not checked. *)
let rec check_symlinks_parent_directories ~filename ~no_check_for (policy : symlink_policy) =
  if filename = "/" || filename = "." || Some filename = no_check_for then
    true
  else
    let dirname = Filename.dirname filename in
    check_symlinks_aux dirname policy &&
    check_symlinks_parent_directories ~filename:dirname ~no_check_for policy


(* Check that [filename] can be reached according to the given
   symlink policy  *)
let check_symlinks ~no_check_for ~filename policy =
  let aux policy =
    if filename = "/" then
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
        if filename.[len] = '/' then
          String.sub filename 0 len
        else
          filename
      in
      check_symlinks_aux filename policy &&
      check_symlinks_parent_directories filename no_check_for policy
  in
  match policy with
  | AlwaysFollowSymlinks -> true
  | DoNotFollowSymlinks -> aux never_follow_symlinks
  | FollowSymlinksIfOwnerMatch -> aux follow_symlinks_if_owner_match

let check_dotdot =
  let regexp = Netstring_pcre.regexp "(/\\.\\./)|(/\\.\\.$)" in
  fun ~filename ->
    (* We always reject .. in filenames.
       In URLs, .. have already been removed by the server,
       but the filename may come from somewhere else than URLs ... *)
    try ignore (Netstring_pcre.search_forward regexp filename 0); false
    with Not_found -> true

let can_send filename request =
  let filename =
    Neturl.join_path (Neturl.norm_path (Neturl.split_path filename)) in
  Lwt_log.ign_info_f ~section "checking if file %s can be sent" filename;
  let matches arg =
    Netstring_pcre.string_match (Ocsigen_extensions.do_not_serve_to_regexp arg)
      filename 0 <> None
  in
  if matches request.do_not_serve_403 then (
    Lwt_log.ign_info ~section "this file is forbidden";
    raise Failed_403)
  else
    if matches request.do_not_serve_404 then (
      Lwt_log.ign_info ~section "this file must be hidden";
      raise Failed_404)


(* Return type of a request for a local file. The string argument
   represents the real file/directory to serve, eg. foo/index.html
   instead of foo *)
type resolved =
  | RFile of string
  | RDir of string


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
let resolve ?no_check_for ~request ~filename () =
  (* We only accept absolute filenames in daemon mode,
     as we do not really know what is the current directory *)
  let filename =
    if Filename.is_relative filename && Ocsigen_config.get_daemon () then
      "/"^filename
    else
      filename
  in
  try
    Lwt_log.ign_info_f ~section "Testing \"%s\"." filename;
    let stat = Unix.LargeFile.stat filename in
    let (filename, stat) =
      if stat.Unix.LargeFile.st_kind = Unix.S_DIR then
        if filename.[String.length filename - 1] <> '/' then begin
          (* In this case, [filename] is a directory but this is not visible in
             its name as there is no final slash. We signal this fact to
             Ocsigen, which will then issue a 301 redirection to "filename/" *)
          Lwt_log.ign_info_f ~section "LocalFiles: %s is a directory" filename;
          raise (Ocsigen_extensions.Ocsigen_Is_a_directory
                   (Ocsigen_extensions.new_url_of_directory_request request))
        end

        else
          let rec find_index = function
            | [] ->
                (* No suitable index, we try to list the directory *)
                if request.request_config.list_directory_content then (
                  Lwt_log.ign_info ~section "Displaying directory content";
                  (filename, stat))
                else (
                  (* No suitable index *)
                  Lwt_log.ign_info ~section "No index and no listing";
                  raise NotReadableDirectory)
            | e :: q ->
                let index = filename ^ e in
                Lwt_log.ign_info_f ~section "Testing \"%s\" as possible index." index;
                try
                  (index, Unix.LargeFile.stat index)
                with
                  | Unix.Unix_error (Unix.ENOENT, _, _) -> find_index q
          in find_index request.request_config.default_directory_index

      else (filename, stat)
    in
    if not (check_dotdot ~filename)
    then
      (Lwt_log.ign_info_f ~section "Filenames cannot contain .. as in \"%s\"." filename;
       raise Failed_403)
    else if check_symlinks ~filename ~no_check_for
        request.request_config.follow_symlinks
    then (
      can_send filename request.request_config;
      (* If the previous function did not fail, we are authorized to
         send this file *)
        Lwt_log.ign_info_f ~section "Returning \"%s\"." filename;
      if stat.Unix.LargeFile.st_kind = Unix.S_REG then
        RFile filename
      else if stat.Unix.LargeFile.st_kind = Unix.S_DIR then
        RDir filename
      else raise Failed_404
    )
    else (
      (* [filename] is accessed through as symlink which we should not
         follow according to the current policy *)
      Lwt_log.ign_info_f ~section "Failed symlink check for \"%s\"." filename;
      raise Failed_403)
  with
  (* We can get an EACCESS here, if are missing some rights on a directory *)
  | Unix.Unix_error (Unix.EACCES,_,_) -> raise Failed_403
  | Unix.Unix_error (Unix.ENOENT,_,_) -> raise Failed_404


(* Given a local file or directory, we retrieve its content *)
let content ~request ~file =
  try
    match file with
    | RDir dirname ->
      Ocsigen_senders.Directory_content.result_of_content
        (dirname, Ocsigen_request_info.full_path request.request_info)
    | RFile filename ->
      Ocsigen_senders.File_content.result_of_content
        (filename,
         request.request_config.charset_assoc,
         request.request_config.mime_assoc
        )

  with
  | Unix.Unix_error (Unix.EACCES,_,_) -> raise Failed_403
