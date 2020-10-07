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
      | {Unix.ai_addr=Unix.ADDR_INET (inet_addr, _); _}::_ ->
         Lwt.return inet_addr
      | _::l -> aux l
    in
    let options = [if v6 then Lwt_unix.AI_FAMILY Lwt_unix.PF_INET6 else Lwt_unix.AI_FAMILY Lwt_unix.PF_INET] in
    Lwt.bind
      (Lwt_unix.getaddrinfo host "" options)
      aux

  let of_sockaddr = function
    | Unix.ADDR_INET (ip, _port) ->
      ip
    | _ ->
      raise (Ocsigen_Internal_Error "ip of unix socket")

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

module Netstring_pcre = struct

  let regexp s = Pcre.regexp ~flags:[`MULTILINE] s

  let templ_re = Pcre.regexp "(?:\\\\\\d)|[\\$\\\\]" ;;

  let tr_templ s =
    (* Convert \n to $n etc. *)
    (* Unfortunately we cannot just replace \ by $. *)
    let rec tr l =
      match l with
        Pcre.Delim "$" :: l' -> "$$" :: tr l'
      | Pcre.Delim "\\" :: Pcre.Delim "$" :: l'  -> "$$" :: tr l'
      | Pcre.Delim "\\" :: Pcre.Delim s :: l' -> s :: tr l'
      | Pcre.Delim "\\" :: Pcre.Text s :: l' -> s :: tr l'
      | [ Pcre.Delim "\\" ] -> failwith "trailing backslash"
      | Pcre.Delim d :: l' ->
        assert(d.[0] = '\\');
        let n = Char.code d.[1] - Char.code '0' in
        if n = 0 then
          "$&" :: tr l'
        else
          ("$" ^ string_of_int n ^ "$!") :: tr l'
      | Pcre.Text t :: l' -> t :: tr l'
      | Pcre.Group(_,_) :: _ -> assert false
      | Pcre.NoGroup :: _ -> assert false
      | [] -> []
    in
    let l = Pcre.full_split ~rex:templ_re ~max:(-1) s in
    String.concat "" (tr l)

  let matched_group result n _ =
    if n < 0 || n >= Pcre.num_of_subs result then raise Not_found;
    ignore (Pcre.get_substring_ofs result n);
    Pcre.get_substring result n

  let matched_string result _ =
    ignore (Pcre.get_substring_ofs result 0);
    Pcre.get_substring result 0

  let global_replace pat templ s =
    Pcre.replace ~rex:pat ~itempl:(Pcre.subst (tr_templ templ)) s

  let global_substitute pat subst s =
    Pcre.substitute_substrings ~rex:pat ~subst:(fun r -> subst r s) s

  let search_forward pat s pos =
    let result = Pcre.exec ~rex:pat ~pos s in
    fst (Pcre.get_substring_ofs result 0), result

  let string_after s n =
    String.sub s n (String.length s - n)

  let bounded_split expr text num =
    let start =
      try
        let start_substrs = Pcre.exec ~rex:expr ~flags:[`ANCHORED] text in
        (* or Not_found *)
        let (_,match_end) = Pcre.get_substring_ofs start_substrs 0 in
        match_end
      with
        Not_found -> 0
    in
    let rec split start n =
      if start >= String.length text then [] else
      if n = 1 then [string_after text start] else
        try
          let next_substrs = Pcre.exec ~rex:expr ~pos:start text
          in (* or Not_found *)
          let pos, match_end = Pcre.get_substring_ofs next_substrs 0 in
          String.sub text start (pos-start) :: split match_end (n-1)
        with Not_found ->
          [string_after text start] in
    split start num

  let split sep s = bounded_split sep s 0

  let string_match pat s pos =
    try
      let result = Pcre.exec ~rex:pat ~flags:[`ANCHORED] ~pos s in
      Some result
    with Not_found -> None

end

module Url = struct

  include Url_base

  (* Taken from Neturl version 1.1.2 *)
  let problem_re1 = Netstring_pcre.regexp "[ <>\"{}|\\\\^\\[\\]`]"

  let fixup_url_string1 =
    Netstring_pcre.global_substitute
      problem_re1
      (fun m s ->
         Printf.sprintf "%%%02x"
           (Char.code s.[fst (Pcre.get_substring_ofs m 0)]))

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

    let percent_encode =
      let lengths =
        let l = Array.make 256 3 in
        String.iter (fun c -> l.(Char.code c) <- 1)
          (* Unreserved Characters (section 2.3 of RFC 3986) *)
          "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_.~";
        l
      in
      fun s ->
        let l = String.length s in
        let l' = ref 0 in
        for i = 0 to l - 1 do
          l' := !l' + lengths.(Char.code s.[i])
        done;
        if l = !l' then
           s
        else
          let s' = Bytes.create !l' in
          let j = ref 0 in
          let hex = "0123456789ABCDEF" in
          for i = 0 to l - 1 do
            let c = s.[i] in
            let n = Char.code s.[i] in
            let d = lengths.(n) in
            if d = 1 then
              Bytes.set s' !j c
            else begin
              Bytes.set s' !j '%';
              Bytes.set s' (!j + 1) hex.[n lsr 4];
              Bytes.set s' (!j + 2) hex.[n land 0xf]
            end;
            j := !j + d
          done;
          Bytes.unsafe_to_string s'

    let encode_plus =
      let lengths =
        let l = Array.make 256 3 in
        String.iter (fun c -> l.(Char.code c) <- 1)
          (* Unchanged characters + space (HTML spec) *)
          "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_.* ";
        l
      in
      fun s ->
        let l = String.length s in
        let l' = ref 0 in
        for i = 0 to l - 1 do
          l' := !l' + lengths.(Char.code s.[i])
        done;
        let s' = Bytes.create !l' in
        let j = ref 0 in
        let hex = "0123456789ABCDEF" in
        for i = 0 to l - 1 do
          let c = s.[i] in
          let n = Char.code s.[i] in
          let d = lengths.(n) in
          if d = 1 then
            Bytes.set s' !j (if c =  ' ' then '+' else c)
          else begin
            Bytes.set s' !j '%';
            Bytes.set s' (!j + 1) hex.[n lsr 4];
            Bytes.set s' (!j + 2) hex.[n land 0xf]
          end;
          j := !j + d
        done;
        Bytes.unsafe_to_string s'

    let encode ?(plus = true) s =
      if plus then encode_plus s else percent_encode s

  end

  let url_decoding_re =
    Netstring_pcre.regexp "\\+\\|%..\\|%.\\|%";;

  let of_hex1 c =
    match c with
    | ('0'..'9') -> Char.code c - Char.code '0'
    | ('A'..'F') -> Char.code c - Char.code 'A' + 10
    | ('a'..'f') -> Char.code c - Char.code 'a' + 10
    | _ ->
      raise Not_found

  let encode = MyUrl.encode
  let decode ?(plus = true) s =
    let pos = 0 and len = None in
    let s_l = String.length s in
    let s1 =
      if pos = 0 && len=None then s else
        let len = match len with Some n -> n | None -> s_l in
        String.sub s pos len in
    let l = String.length s1 in
    Netstring_pcre.global_substitute
      url_decoding_re
      (fun r _ ->
         match Netstring_pcre.matched_string r s1 with
         | "+" -> if plus then " " else "+"
         | _ ->
           let i = fst (Pcre.get_substring_ofs r 0) in
           (* Assertion: s1.[i] = '%' *)
           if i+2 >= l then failwith "decode";
           let c1 = s1.[i+1] in
           let c2 = s1.[i+2] in
           begin
             try
               let k1 = of_hex1 c1 in
               let k2 = of_hex1 c2 in
               String.make 1 (Char.chr((k1 lsl 4) lor k2))
             with
               Not_found ->
               failwith "decode"
           end
      )
      s1

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

  let url_split_re = Str.regexp "[&=]";;

  (* taken from Ocamlnet 4.1.2 *)
  let dest_url_encoded_parameters parstr =
    let rec parse_after_amp tl =
      match tl with
      | Str.Text name :: Str.Delim "=" :: Str.Text value :: tl' ->
        (decode name, decode value) :: parse_next tl'
      | Str.Text name :: Str.Delim "=" :: Str.Delim "&" :: tl' ->
        (decode name, "") :: parse_after_amp tl'
      | Str.Text name :: Str.Delim "=" :: [] ->
        [decode name, ""]
      | _ ->
        failwith "dest_url_encoded_parameters"
    and parse_next tl =
      match tl with
      | [] -> []
      | Str.Delim "&" :: tl' ->
        parse_after_amp tl'
      | _ ->
        failwith "dest_url_encoded_parameters"
    in
    let toklist = Str.full_split url_split_re parstr in
    match toklist with
    | [] -> []
    | _ -> parse_after_amp toklist

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
            dest_url_encoded_parameters params_string
          with Failure _ -> raise Ocsigen_Bad_Request
        end
      in

      let path = List.map (decode ~plus:false) (split_path pathstring) in
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

  let prefix_and_path_of_t url =
    let (https, host, port, _, path, _, _) = parse url in
    let https_str = match https with
    | None -> ""
    | Some x -> if x then "https://" else "http://"
    in
    let host_str = match host with
    | None -> ""
    | Some x -> x
    in
    let port_str = match port with
    | None -> ""
    | Some x -> string_of_int x
    in
    (https_str ^ host_str ^ ":" ^ port_str, path)

end

module Date = struct

  let name_of_day = function
    | 0 -> "Sun"
    | 1 -> "Mon"
    | 2 -> "Tue"
    | 3 -> "Wed"
    | 4 -> "Thu"
    | 5 -> "Fri"
    | 6 -> "Sat"
    | _ -> failwith "name_of_day"

  let name_of_month = function
    | 0  -> "Jan"
    | 1  -> "Feb"
    | 2  -> "Mar"
    | 3  -> "Apr"
    | 4  -> "May"
    | 5  -> "Jun"
    | 6  -> "Jul"
    | 7  -> "Aug"
    | 8  -> "Sep"
    | 9  -> "Oct"
    | 10 -> "Nov"
    | 11 -> "Dec"
    | _  -> failwith "name_of_month"

  let to_string d =
    let {
      Unix.tm_wday ;
      tm_mday ; tm_mon ; tm_year ;
      tm_hour ; tm_min ; tm_sec ; _
    } = Unix.gmtime d in
    Printf.sprintf "%s, %02d %s %d %02d:%02d:%02d GMT"
      (name_of_day tm_wday)
      tm_mday (name_of_month tm_mon) (tm_year + 1900)
      tm_hour tm_min tm_sec

end
