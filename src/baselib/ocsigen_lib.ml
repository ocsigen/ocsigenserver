(* Ocsigen
 * Copyright (C) 2005 Vincent Balat
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

include Ocsigen_lib_base

module String = String_base

(*****************************************************************************)

module Ip_address = struct
  exception No_such_host

  let get_inet_addr ?(v6=false) host =
    let rec aux = function
      | [] -> Lwt.fail No_such_host
      | {Unix.ai_addr=Unix.ADDR_INET (inet_addr, _)}::_ -> Lwt.return inet_addr
      | _::l -> aux l
    in
    let options = [if v6 then Lwt_unix.AI_FAMILY Lwt_unix.PF_INET6 else Lwt_unix.AI_FAMILY Lwt_unix.PF_INET] in
    Lwt.bind
      (Lwt_unix.getaddrinfo host "" options)
      aux

end

(*****************************************************************************)

module Filename = struct

  include Filename

  let basename f =
    let n = String.length f in
    let i = try String.rindex f '\\' + 1 with Not_found -> 0 in
    let j = try String.rindex f '/' + 1 with Not_found -> 0 in
    let k = max i j in
    if k < n then
      String.sub f k (n-k)
    else
      "none"

  let extension_no_directory filename =
    try
      let pos = String.rindex filename '.' in
      String.sub filename (pos+1) ((String.length filename) - pos - 1)
    with Not_found ->
      raise Not_found

  let extension filename =
    try
      let pos = String.rindex filename '.'
      and slash =
        try String.rindex filename '/'
        with Not_found -> -1
      in
      if pos > slash then
        String.sub filename (pos+1) ((String.length filename) - pos - 1)
      else (* Dot before a directory separator *)
        raise Not_found
    with Not_found -> (* No dot in filename *)
      raise Not_found

end

(*****************************************************************************)

let make_cryptographic_safe_string =
  let rng = Cryptokit.Random.device_rng "/dev/urandom" in
  fun () ->
    let random_part =
      let random_number = Cryptokit.Random.string rng 20 in
      let to_b64 = Cryptokit.Base64.encode_compact () in
      Cryptokit.transform_string to_b64 random_number
    and sequential_part =
      (*VVV Use base 64 also here *)
      Printf.sprintf "%Lx" (Int64.bits_of_float (Unix.gettimeofday ())) in
    random_part ^ sequential_part

(* The string is produced from the concatenation of two components:
   a 160-bit random sequence obtained from /dev/urandom, and a
   64-bit sequential component derived from the system clock.  The
   former is supposed to prevent session spoofing.  The assumption
   is that given the high cryptographic quality of /dev/urandom, it
   is impossible for an attacker to deduce the sequence of random
   numbers produced.  As for the latter component, it exists to
   prevent a theoretical (though infinitesimally unlikely) session
   ID collision if the server were to be restarted.
*)


module Url = struct

  include Url_base

  (* Taken from Neturl version 1.1.2 *)
  let problem_re1 = Netstring_pcre.regexp "[ <>\"{}|\\\\^\\[\\]`]"

  let fixup_url_string1 =
    Netstring_pcre.global_substitute
      problem_re1
      (fun m s ->
         Printf.sprintf "%%%02x"
           (Char.code s.[Netstring_pcre.match_beginning m]))

  (* I add this fixup to handle %uxxxx sent by browsers.
     Translated to %xx%xx *)
  let problem_re2 = Netstring_pcre.regexp "\\%u(..)(..)"

  let fixup_url_string s =
    fixup_url_string1
      (Netstring_pcre.global_substitute
         problem_re2
         (fun m s ->
            String.concat "" ["%"; Netstring_pcre.matched_group m 1 s;
                              "%"; Netstring_pcre.matched_group m 2 s]
         )
         s)

  (*VVV This is in Netencoding but we have a problem with ~
        (not encoded by browsers). Here is a patch that does not encode '~': *)
  module MyUrl = struct

    let hex_digits =
      [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7';
         '8'; '9'; 'A'; 'B'; 'C'; 'D'; 'E'; 'F' |]

    let to_hex2 k =
      (* Converts k to a 2-digit hex string *)
      let s = Bytes.create 2 in
      Bytes.set s 0 hex_digits.( (k lsr 4) land 15 );
      Bytes.set s 1 hex_digits.( k land 15 );
      s

    let url_encoding_re =
      Netstring_pcre.regexp "[^A-Za-z0-9~_.!*\\-]"

    let encode ?(plus = true) s =
      Netstring_pcre.global_substitute
        url_encoding_re
        (fun r _ ->
           match Netstring_pcre.matched_string r s with
           | " " when plus -> "+"
           | x ->
             let k = Char.code(x.[0]) in
             "%" ^ to_hex2 k
        )
        s

  end

  let encode = MyUrl.encode
  let decode ?plus a = Netencoding.Url.decode ?plus a

  let make_encoded_parameters params =
    String.concat "&"
      (List.map (fun (name, value) -> encode name ^ "=" ^ encode value) params)

  let string_of_url_path ~encode l =
    if encode
    then
      fixup_url_string (String.concat "/"
                          (List.map (*Netencoding.Url.encode*)
                             (MyUrl.encode ~plus:false) l))
      (* ' ' are not encoded to '+' in paths *)
    else String.concat "/" l (* BYXXX : check illicit characters *)


  let parse =

    (* We do not accept http://login:pwd@host:port (should we?). *)
    let url_re = Netstring_pcre.regexp "^([Hh][Tt][Tt][Pp][Ss]?)://([0-9a-zA-Z.-]+|\\[[0-9A-Fa-f:.]+\\])(:([0-9]+))?/([^\\?]*)(\\?(.*))?$" in
    let short_url_re = Netstring_pcre.regexp "^/([^\\?]*)(\\?(.*))?$" in
    (*  let url_relax_re = Netstring_pcre.regexp "^[Hh][Tt][Tt][Pp][Ss]?://[^/]+" in
    *)
    fun url ->

      let match_re = Netstring_pcre.string_match url_re url 0 in

      let (https, host, port, pathstring, query) =
        match match_re with
        | None ->
          (match Netstring_pcre.string_match short_url_re url 0 with
           | None -> raise Ocsigen_Bad_Request
           | Some m ->
             let path =
               fixup_url_string (Netstring_pcre.matched_group m 1 url)
             in
             let query =
               try
                 Some (fixup_url_string (Netstring_pcre.matched_group m 3 url))
               with Not_found -> None
             in
             (None, None, None, path, query))
        | Some m ->
          let path = fixup_url_string (Netstring_pcre.matched_group m 5 url) in
          let query =
            try Some (fixup_url_string (Netstring_pcre.matched_group m 7 url))
            with Not_found -> None
          in
          let https =
            try (match Netstring_pcre.matched_group m 1 url with
                | "http" -> Some false
                | "https" -> Some true
                | _ -> None)
            with Not_found -> None in
          let host =
            try Some (Netstring_pcre.matched_group m 2 url)
            with Not_found -> None in
          let port =
            try Some (int_of_string (Netstring_pcre.matched_group m 4 url))
            with Not_found -> None in
          (https, host, port, path, query)
      in

      (* Note that the fragment (string after #) is not sent by browsers *)

      (*20110707 ' ' is encoded to '+' in queries, but not in paths.
        Warning: if we write the URL manually, we must encode ' ' to '+' manually
        (not done by the browser).
        --Vincent
      *)

      let get_params =
        lazy begin
          let params_string = match query with None -> "" | Some s -> s in
          try
            Netencoding.Url.dest_url_encoded_parameters params_string
          with Failure _ -> raise Ocsigen_Bad_Request
        end
      in

      let path = List.map (Netencoding.Url.decode ~plus:false) (Neturl.split_path pathstring) in
      let path = remove_dotdot path (* and remove "//" *)
      (* here we remove .. from paths, as it is dangerous.
         But in some very particular cases, we may want them?
         I prefer forbid that. *)
      in
      let uri_string = match query with
        | None -> pathstring
        | Some s -> String.concat "?" [pathstring; s]
      in

      (https, host, port, uri_string, path, query, get_params)

end
