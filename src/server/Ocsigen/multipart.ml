(* This code is inspired by mimestring.ml from OcamlNet *)
(* Copyright Gerd Stolpmann, Patrick Doane *)
(* Modified for Ocsigen/Lwt by Nataliya Guts and Vincent Balat *)

(*VVV Check wether we should support int64 for large files? *)

open Lwt.Infix
module S = Ocsigen_base.Lib.Netstring_pcre
module Pcre = Re.Pcre

let section = Logs.Src.create "ocsigen:server:multipart"

(* [Multipart_error reason]: the body is malformed. It becomes a bad request
   in [scan_multipart_body_from_stream], which logs [reason]. *)
exception Multipart_error of string
exception Ocsigen_upload_forbidden

let malformed_header = Multipart_error "a part has a malformed header"
let match_end result = snd (Pcre.get_substring_ofs result 0)
let cr_or_lf_re = S.regexp "[\013\n]"

let header_stripped_re =
  S.regexp
    "([^ \t\r\n:]+):[ \t]*((.*[^ \t\r\n])?([ \t\r]*\n[ \t](.*[^ \t\r\n])?)*)[ \t\r]*\n"

let header_unstripped_re = S.regexp "([^ \t\r\n:]+):([ \t]*.*\n([ \t].*\n)*)"
(* This much simpler expression returns the name and the unstripped
   value. *)

let empty_line_re = S.regexp "\013?\n"
let end_of_header_re = S.regexp "\n\013?\n"

let scan_header
      ?(downcase = true)
      ?(unfold = true)
      ?(strip = false)
      parstr
      ~start_pos
      ~end_pos
  =
  let header_re =
    if unfold || strip then header_stripped_re else header_unstripped_re
  in
  let rec parse_header i l =
    match S.string_match header_re parstr i with
    | Some r ->
        let i' = match_end r in
        if i' > end_pos then raise malformed_header;
        let name =
          if downcase
          then String.lowercase_ascii (S.matched_group r 1 parstr)
          else S.matched_group r 1 parstr
        in
        let value_with_crlf = S.matched_group r 2 parstr in
        let value =
          if unfold
          then S.global_replace cr_or_lf_re "" value_with_crlf
          else value_with_crlf
        in
        parse_header i' ((name, value) :: l)
    | None -> (
      (* The header must end with an empty line *)
      match S.string_match empty_line_re parstr i with
      | Some r' -> List.rev l, match_end r'
      | None -> raise malformed_header)
  in
  parse_header start_pos []

let read_header ?downcase ?unfold ?strip s =
  let rec find_end_of_header s =
    Lwt.catch
      (fun () ->
         let b = Ocsigen_base.Ocsigen_stream.current_buffer s in
         (* Maybe the header is empty. In this case, there is an empty
            line right at the beginning *)
         match S.string_match empty_line_re b 0 with
         | Some r -> Lwt.return (s, match_end r)
         | None ->
             (* Search for an empty line *)
             Lwt.return
               (s, match_end (snd (S.search_forward end_of_header_re b 0))))
      (function
        | Not_found -> (
            Ocsigen_base.Ocsigen_stream.enlarge_stream s >>= function
            | Ocsigen_base.Ocsigen_stream.Finished _ ->
                Lwt.fail Ocsigen_base.Ocsigen_stream.Stream_too_small
            | Ocsigen_base.Ocsigen_stream.Cont _ as s -> find_end_of_header s)
        | e -> Lwt.fail e)
  in
  find_end_of_header s >>= fun (s, end_pos) ->
  let b = Ocsigen_base.Ocsigen_stream.current_buffer s in
  let h, (_ : int) =
    scan_header ?downcase ?unfold ?strip b ~start_pos:0 ~end_pos
  in
  Ocsigen_base.Ocsigen_stream.skip s (Int64.of_int end_pos) >>= fun s ->
  Lwt.return (s, h)

let lf_re = S.regexp "[\n]"

let rec search_window s re start =
  try
    Lwt.return
      ( s
      , snd
          (S.search_forward re
             (Ocsigen_base.Ocsigen_stream.current_buffer s)
             start) )
  with Not_found -> (
    Ocsigen_base.Ocsigen_stream.enlarge_stream s >>= function
    | Ocsigen_base.Ocsigen_stream.Finished _ ->
        Lwt.fail Ocsigen_base.Ocsigen_stream.Stream_too_small
    | Ocsigen_base.Ocsigen_stream.Cont _ as s -> search_window s re start)

let search_end_of_line s k =
  (* Search LF beginning at position k. Without one, as after a boundary
     without line end, it fails with [Stream_too_small]. *)
  search_window s lf_re k >>= fun (s, x) -> Lwt.return (s, match_end x)

let search_first_boundary ~boundary s =
  (* Search boundary per regexp; return the position of the
     character immediately following the boundary (on the same
     line). Without a boundary, it fails with [Stream_too_small]. *)
  let re = S.regexp ("\n--" ^ Pcre.quote boundary) in
  search_window s re 0 >>= fun (s, x) -> Lwt.return (s, match_end x)

let check_beginning_is_boundary ~boundary s =
  let del = "--" ^ boundary in
  let ldel = String.length del in
  Ocsigen_base.Ocsigen_stream.stream_want s (ldel + 2) >>= function
  | Ocsigen_base.Ocsigen_stream.Finished _ as str2 ->
      Lwt.return (str2, false, false)
  | Ocsigen_base.Ocsigen_stream.Cont (ss, _f) as str2 ->
      let long = String.length ss in
      let isdelim = long >= ldel && String.sub ss 0 ldel = del in
      let islast = isdelim && long >= ldel + 2 && String.sub ss ldel 2 = "--" in
      Lwt.return (str2, isdelim, islast)

let rec parse_parts ~boundary ~decode_part s uses_crlf =
  (* PRE: [s] is at the beginning of the next part.  [uses_crlf] must
     be true if CRLF is used as EOL sequence, and false if only LF is
     used as EOL sequence.  *)
  let delimiter = (if uses_crlf then "\r" else "") ^ "\n--" ^ boundary in
  Ocsigen_base.Ocsigen_stream.substream delimiter s >>= fun a ->
  decode_part a >>= fun (y, s) ->
  (* Now the position of [s] is at the beginning of the delimiter.
     Check if there is a "--" after the delimiter (==> last part) *)
  let l_delimiter = String.length delimiter in
  Ocsigen_base.Ocsigen_stream.next s >>= fun s ->
  Ocsigen_base.Ocsigen_stream.stream_want s (l_delimiter + 2) >>= fun s ->
  let last_part =
    match s with
    | Ocsigen_base.Ocsigen_stream.Finished _ -> false
    | Ocsigen_base.Ocsigen_stream.Cont (ss, _f) ->
        let long = String.length ss in
        long >= l_delimiter + 2
        && ss.[l_delimiter] = '-'
        && ss.[l_delimiter + 1] = '-'
  in
  if last_part
  then Lwt.return [y]
  else
    search_end_of_line s 2 >>= fun (s, k) ->
    (* [k]: Beginning of next part *)
    Ocsigen_base.Ocsigen_stream.skip s (Int64.of_int k) >>= fun s ->
    parse_parts ~boundary ~decode_part s uses_crlf >>= fun l ->
    Lwt.return (y :: l)

let read_multipart_body ~boundary ~decode_part s =
  (* Check whether s directly begins with a boundary *)
  check_beginning_is_boundary ~boundary s >>= fun (s, b, islast) ->
  if islast
  then Lwt.return []
  else if b
  then
    (* Move to the beginning of the next line *)
    search_end_of_line s 0 >>= fun (s, k_eol) ->
    let uses_crlf =
      (Ocsigen_base.Ocsigen_stream.current_buffer s).[k_eol - 2] = '\r'
    in
    Ocsigen_base.Ocsigen_stream.skip s (Int64.of_int k_eol) >>= fun s ->
    (* Begin with first part: *)
    parse_parts ~boundary ~decode_part s uses_crlf
  else
    (* Look for the first boundary *)
    search_first_boundary ~boundary s >>= fun (s, k_eob) ->
    search_end_of_line s k_eob >>= fun (s, k_eol) ->
    let uses_crlf =
      (Ocsigen_base.Ocsigen_stream.current_buffer s).[k_eol - 2] = '\r'
    in
    Ocsigen_base.Ocsigen_stream.skip s (Int64.of_int k_eol) >>= fun s ->
    (* Begin with first part: *)
    parse_parts ~boundary ~decode_part s uses_crlf

let empty_stream =
  Ocsigen_base.Ocsigen_stream.get
    (Ocsigen_base.Ocsigen_stream.make (fun () ->
       Ocsigen_base.Ocsigen_stream.empty None))

let decode_part ~max_size ~create ~add ~stop stream =
  read_header stream >>= fun (s, header) ->
  let p = create header in
  let rec while_stream size = function
    | Ocsigen_base.Ocsigen_stream.Finished None ->
        Lwt.return (size, empty_stream)
    | Ocsigen_base.Ocsigen_stream.Finished (Some ss) -> Lwt.return (size, ss)
    | Ocsigen_base.Ocsigen_stream.Cont (stri, f) ->
        let long = String.length stri in
        let size2 = Int64.add size (Int64.of_int long) in
        if
          match max_size with
          | None -> false
          | Some m -> Int64.compare size2 m > 0
        then Lwt.fail Ocsigen_base.Lib.Ocsigen_Request_too_long
        else if stri = ""
        then Ocsigen_base.Ocsigen_stream.next f >>= while_stream size
        else
          add p stri >>= fun () ->
          Ocsigen_base.Ocsigen_stream.next f >>= while_stream size2
  in
  (* [stop] closes the file of the part, if any. It runs once: when it fails
     after the body was read, its error is reported without a second call,
     which could close a descriptor reused in the meantime. *)
  Lwt.try_bind
    (fun () -> while_stream Int64.zero s)
    (fun (size, s) -> stop size p >>= fun r -> Lwt.return (r, s))
    (fun error ->
       Lwt.catch
         (fun () -> stop Int64.zero p >|= ignore)
         (fun e ->
            Logs.warn ~src:section (fun fmt ->
              fmt "Error while closing a part, after another error: %s"
                (Printexc.to_string e));
            Lwt.return ())
       >>= fun () -> Lwt.fail error)

(* [reject_body reason] logs why a multipart body is rejected, and fails with
   [Ocsigen_Bad_Request], which the client gets as a 400. *)
let reject_body reason =
  Logs.info ~src:section (fun fmt -> fmt "Multipart body rejected: %s" reason);
  Lwt.fail Ocsigen_base.Lib.Ocsigen_Bad_Request

let scan_multipart_body_from_stream ?max_size ~boundary ~create ~add ~stop s =
  let decode_part = decode_part ~max_size ~create ~add ~stop in
  Lwt.catch
    (fun () ->
       (* read the multipart body: *)
       Ocsigen_base.Ocsigen_stream.next s >>= fun s ->
       read_multipart_body ~boundary ~decode_part s >>= fun _ -> Lwt.return ())
    (function
      | Ocsigen_base.Ocsigen_stream.Stream_too_small ->
          reject_body "the body or one of its parts ends too early"
      | Multipart_error reason -> reject_body reason
      | e -> Lwt.fail e)

(* [get_boundary ctparams] is the boundary in the parameters [ctparams] of a
   multipart Content-Type. It has 1 to 70 characters (RFC 2046). *)
let get_boundary ctparams =
  match List.assoc_opt "boundary" ctparams with
  | None -> reject_body "its Content-Type has no boundary"
  | Some "" -> reject_body "its boundary is empty"
  | Some boundary when String.length boundary > 70 ->
      reject_body "its boundary is longer than 70 characters"
  | Some boundary -> Lwt.return boundary

let counter =
  let c = ref (Random.int 1000000) in
  fun () ->
    c := !c + 1;
    !c

(* [header_params s] is [(v, params)], where [v] is the value at the start of
   the header value [s], such as a media type or a disposition type, and
   [params] the list of the parameters that follow it, with their names in
   lowercase, or [None] if they are malformed. It reads the parameters of the
   Content-Type (RFC 2045) and Content-Disposition (RFC 6266, RFC 7578)
   headers. The names are matched whole, so that [filename] is never taken
   for [name]. Spaces may surround "=" and ";", a trailing ";" is ignored,
   and a value is a token or a quoted string. An unquoted value may also
   contain "=", although RFC 2045 requires quotes then, so that a boundary
   such as ==a=b is accepted as it always was. In a quoted string, \" and \\
   stand for " and \, as some clients escape them (Go, curl --form-escape);
   any other backslash is kept. Browsers do not escape backslashes (they
   percent-encode double quotes), so a value ending with a backslash, such as
   a file name, makes the parameters malformed when read this way: they are
   then read again with every backslash kept. Two backslashes in a row sent
   by a browser are still read as one, as nothing tells them from an escaped
   backslash. *)
let header_params s =
  let len = String.length s in
  (* [scan p i] is the first position from [i] whose character does not
     satisfy [p], or [len]. *)
  let rec scan p i = if i < len && p s.[i] then scan p (i + 1) else i in
  let is_space c = c = ' ' || c = '\t' in
  let is_token c = not (is_space c || c = '=' || c = ';' || c = '"') in
  (* [quoted ~escapes i] reads a quoted string whose first character is at
     [i]. With [escapes], \" and \\ stand for " and \. *)
  let quoted ~escapes i =
    let b = Buffer.create 16 in
    let rec loop j =
      if j >= len
      then None
      else
        match s.[j] with
        | '"' -> Some (Buffer.contents b, j + 1)
        | '\\'
          when escapes && j + 1 < len && (s.[j + 1] = '"' || s.[j + 1] = '\\')
          ->
            Buffer.add_char b s.[j + 1];
            loop (j + 2)
        | c ->
            Buffer.add_char b c;
            loop (j + 1)
    in
    loop i
  in
  let value ~escapes i =
    if i < len && s.[i] = '"'
    then quoted ~escapes (i + 1)
    else
      let j = scan (fun c -> is_token c || c = '=') i in
      if j = i then None else Some (String.sub s i (j - i), j)
  in
  let rec params ~escapes acc i =
    let i = scan is_space i in
    if i = len
    then Some (List.rev acc)
    else if s.[i] <> ';'
    then None
    else
      let i = scan is_space (i + 1) in
      if i = len
      then Some (List.rev acc)
      else
        let j = scan is_token i in
        let k = scan is_space j in
        if j = i || k = len || s.[k] <> '='
        then None
        else
          let name = String.lowercase_ascii (String.sub s i (j - i)) in
          match value ~escapes (scan is_space (k + 1)) with
          | Some (v, next) -> params ~escapes ((name, v) :: acc) next
          | None -> None
  in
  let start = scan is_space 0 in
  let stop = scan is_token start in
  ( String.sub s start (stop - start)
  , match params ~escapes:true [] stop with
    | Some _ as ps -> ps
    | None -> params ~escapes:false [] stop )

let parse_content_type s =
  let media_type, params = header_params s in
  match String.index_opt media_type '/' with
  | None -> None
  | Some i ->
      let typ = String.sub media_type 0 i
      and subtype =
        String.sub media_type (i + 1) (String.length media_type - i - 1)
      in
      (* Malformed parameters are ignored *)
      Some ((typ, subtype), Option.value params ~default:[])

type content_type = (string * string) * (string * string) list

type file_info =
  { tmp_filename : string
  ; filesize : int64
  ; raw_original_filename : string
  ; file_content_type : ((string * string) * (string * string) list) option }

type post_data = (string * string) list * (string * file_info) list

let post_params_form_urlencoded body_gen _ _ =
  Lwt.catch
    (fun () ->
       let body = Ocsigen_base.Ocsigen_stream.get body_gen in
       (* BY, adapted from a previous comment. Should this stream be
          consumed in case of error? *)
       Ocsigen_base.Ocsigen_stream.string_of_stream
         (Config.get_maxrequestbodysizeinmemory ())
         body
       >>= fun r ->
       let r = Ocsigen_base.Lib.Url.fixup_url_string r in
       let l =
         Uri.query_of_encoded r
         |> List.map (fun (s, l) -> List.map (fun v -> s, v) l)
         |> List.concat
       in
       Lwt.return (l, []))
    (function
      | Ocsigen_base.Ocsigen_stream.String_too_large ->
          Lwt.fail Ocsigen_base.Lib.Input_is_too_large
      | e -> Lwt.fail e)

let post_params_multipart_form_data
      ~on_file
      ctparams
      body_gen
      upload_dir
      max_size
  =
  (* Same question here, should this stream be consumed after an
     error? *)
  get_boundary ctparams >>= fun boundary ->
  let body = Ocsigen_base.Ocsigen_stream.get body_gen
  and params = ref []
  and files = ref [] in
  let rec add p s =
    match p with
    | _, `No_file to_buf -> Buffer.add_string to_buf s; Lwt.return ()
    | _, `Some_file (_, _, wh, _) ->
        let len = String.length s in
        let r = Unix.write_substring wh s 0 len in
        if r < len
        then (*XXXX Inefficient if s is long *)
          add p (String.sub s r (len - r))
        else Lwt.pause ()
  in
  let create hs =
    let content_type =
      try
        let ct = List.assoc "content-type" hs in
        parse_content_type ct
      with _ -> None
    in
    let malformed reason = raise (Multipart_error ("a part has " ^ reason)) in
    let cd =
      match List.assoc_opt "content-disposition" hs with
      | Some cd -> cd
      | None -> malformed "no Content-Disposition header"
    in
    let cd_params =
      match header_params cd with
      | _, Some cd_params -> cd_params
      | _, None -> malformed "a malformed Content-Disposition"
    in
    let p_name =
      match List.assoc_opt "name" cd_params with
      | Some p_name -> p_name
      | None -> malformed "no name in its Content-Disposition"
    in
    match List.assoc_opt "filename" cd_params with
    | None -> p_name, `No_file (Buffer.create 1024)
    | Some store -> (
      match upload_dir with
      | Some dname ->
          let fname =
            Printf.sprintf "%s/%f-%d" dname (Unix.gettimeofday ()) (counter ())
          in
          let fd =
            Unix.openfile fname
              [Unix.O_CREAT; Unix.O_TRUNC; Unix.O_WRONLY; Unix.O_NONBLOCK]
              0o666
          in
          Logs.info ~src:section (fun fmt ->
            fmt "%s" ("Upload file opened: " ^ fname));
          on_file fname;
          p_name, `Some_file (fname, store, fd, content_type)
      | None -> raise Ocsigen_upload_forbidden)
  and stop filesize = function
    | p_name, `No_file to_buf ->
        params := !params @ [p_name, Buffer.contents to_buf];
        Lwt.return ()
    (* in the end ? *)
    | ( p_name
      , `Some_file (tmp_filename, raw_original_filename, wh, file_content_type)
      ) ->
        let file_info =
          {tmp_filename; filesize; raw_original_filename; file_content_type}
        in
        files := !files @ [p_name, file_info];
        Unix.close wh;
        Lwt.return ()
  in
  scan_multipart_body_from_stream ?max_size ~boundary ~create ~add ~stop body
  >>= fun () ->
  (*VVV Does scan_multipart_body_from_stream read until the end or
    only what it needs?  If we do not consume here, the following
    request will be read only when this one is finished ...  *)
  Ocsigen_base.Ocsigen_stream.consume body_gen >>= fun () ->
  Lwt.return (!params, !files)

let post_params ~content_type ~on_file body_gen =
  let (ct, cst), ctparams = content_type in
  match String.lowercase_ascii ct, String.lowercase_ascii cst with
  | "application", "x-www-form-urlencoded" ->
      Some
        (body_gen |> Ocsigen_base.Ocsigen_stream.of_cohttp_body
       |> post_params_form_urlencoded)
  | "multipart", "form-data" ->
      Some
        (body_gen |> Ocsigen_base.Ocsigen_stream.of_cohttp_body
        |> post_params_multipart_form_data ~on_file ctparams)
  | _ -> None
