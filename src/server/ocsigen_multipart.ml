open Eio.Std

(* This code is inspired by mimestring.ml from OcamlNet *)
(* Copyright Gerd Stolpmann, Patrick Doane *)
(* Modified for Ocsigen/Lwt by Nataliya Guts and Vincent Balat *)

(*VVV Check wether we should support int64 for large files? *)

module S = Ocsigen_lib.Netstring_pcre
module Pcre = Re.Pcre

let section = Logs.Src.create "ocsigen:server:multipart"

exception Multipart_error of string
exception Ocsigen_upload_forbidden

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
        if i' > end_pos then raise (Multipart_error "Mimestring.scan_header");
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
      | None -> raise (Multipart_error "Mimestring.scan_header"))
  in
  parse_header start_pos []

let read_header ?downcase ?unfold ?strip s =
  let rec find_end_of_header s =
    try
      let b = Ocsigen_stream.current_buffer s in
      (* Maybe the header is empty. In this case, there is an empty
            line right at the beginning *)
      match S.string_match empty_line_re b 0 with
      | Some r -> s, match_end r
      | None ->
          (* Search for an empty line *)
          s, match_end (snd (S.search_forward end_of_header_re b 0))
    with
    | Not_found -> (
      match Ocsigen_stream.enlarge_stream s with
      | Ocsigen_stream.Finished _ -> raise Ocsigen_stream.Stream_too_small
      | Ocsigen_stream.Cont _ as s -> find_end_of_header s)
    | e -> raise e
  in
  let s, end_pos = find_end_of_header s in
  let b = Ocsigen_stream.current_buffer s in
  let h, (_ : int) =
    scan_header ?downcase ?unfold ?strip b ~start_pos:0 ~end_pos
  in
  let s = Ocsigen_stream.skip s (Int64.of_int end_pos) in
  s, h

let lf_re = S.regexp "[\n]"

let rec search_window s re start =
  try s, snd (S.search_forward re (Ocsigen_stream.current_buffer s) start)
  with Not_found -> (
    match Ocsigen_stream.enlarge_stream s with
    | Ocsigen_stream.Finished _ -> raise Ocsigen_stream.Stream_too_small
    | Ocsigen_stream.Cont _ as s -> search_window s re start)

let search_end_of_line s k =
  try
    let s, x =
      (* Search LF beginning at position k *)
      search_window s lf_re k
    in
    s, match_end x
  with
  | Not_found ->
      raise
        (Multipart_error "read_multipart_body: MIME boundary without line end")
  | e -> raise e

let search_first_boundary ~boundary s =
  (* Search boundary per regexp; return the position of the
     character immediately following the boundary (on the same
     line), or raise Not_found. *)
  let re = S.regexp ("\n--" ^ Pcre.quote boundary) in
  let s, x = search_window s re 0 in
  s, match_end x

let check_beginning_is_boundary ~boundary s =
  let del = "--" ^ boundary in
  let ldel = String.length del in
  match Ocsigen_stream.stream_want s (ldel + 2) with
  | Ocsigen_stream.Finished _ as str2 -> str2, false, false
  | Ocsigen_stream.Cont (ss, _f) as str2 ->
      let long = String.length ss in
      let isdelim = long >= ldel && String.sub ss 0 ldel = del in
      let islast = isdelim && String.sub ss ldel 2 = "--" in
      str2, isdelim, islast

let rec parse_parts ~boundary ~decode_part s uses_crlf =
  (* PRE: [s] is at the beginning of the next part.  [uses_crlf] must
     be true if CRLF is used as EOL sequence, and false if only LF is
     used as EOL sequence.  *)
  let delimiter = (if uses_crlf then "\r" else "") ^ "\n--" ^ boundary in
  let a = Ocsigen_stream.substream delimiter s in
  let y, s = decode_part a in
  (* Now the position of [s] is at the beginning of the delimiter.
     Check if there is a "--" after the delimiter (==> last part) *)
  let l_delimiter = String.length delimiter in
  let s = Ocsigen_stream.next s in
  let s = Ocsigen_stream.stream_want s (l_delimiter + 2) in
  let last_part =
    match s with
    | Ocsigen_stream.Finished _ -> false
    | Ocsigen_stream.Cont (ss, _f) ->
        let long = String.length ss in
        long >= l_delimiter + 2
        && ss.[l_delimiter] = '-'
        && ss.[l_delimiter + 1] = '-'
  in
  if last_part
  then [y]
  else
    let s, k = search_end_of_line s 2 in
    let s =
      (* [k]: Beginning of next part *)
      Ocsigen_stream.skip s (Int64.of_int k)
    in
    let l = parse_parts ~boundary ~decode_part s uses_crlf in
    y :: l

let read_multipart_body ~boundary ~decode_part s =
  let s, b, islast =
    (* Check whether s directly begins with a boundary *)
    check_beginning_is_boundary ~boundary s
  in
  if islast
  then []
  else if b
  then
    let s, k_eol =
      (* Move to the beginning of the next line *)
      search_end_of_line s 0
    in
    let uses_crlf = (Ocsigen_stream.current_buffer s).[k_eol - 2] = '\r' in
    let s = Ocsigen_stream.skip s (Int64.of_int k_eol) in
    (* Begin with first part: *)
    parse_parts ~boundary ~decode_part s uses_crlf
  else (* Look for the first boundary *)
    try
      let s, k_eob = search_first_boundary ~boundary s in
      let s, k_eol = search_end_of_line s k_eob in
      let uses_crlf = (Ocsigen_stream.current_buffer s).[k_eol - 2] = '\r' in
      let s =
        (* Printf.printf "k_eol=%d\n" k_eol; *)
        Ocsigen_stream.skip s (Int64.of_int k_eol)
      in
      (* Begin with first part: *)
      parse_parts ~boundary ~decode_part s uses_crlf
    with
    | Not_found ->
        (* No boundary at all, empty body *)
        []
    | e -> raise e

let empty_stream =
  Ocsigen_stream.get (Ocsigen_stream.make (fun () -> Ocsigen_stream.empty None))

let decode_part ~max_size ~create ~add ~stop stream =
  let s, header = read_header stream in
  let p = create header in
  let rec while_stream size = function
    | Ocsigen_stream.Finished None -> size, empty_stream
    | Ocsigen_stream.Finished (Some ss) -> size, ss
    | Ocsigen_stream.Cont (stri, f) ->
        let long = String.length stri in
        let size2 = Int64.add size (Int64.of_int long) in
        if
          match max_size with
          | None -> false
          | Some m -> Int64.compare size2 m > 0
        then raise Ocsigen_lib.Ocsigen_Request_too_long
        else if stri = ""
        then (while_stream size) (Ocsigen_stream.next f)
        else (
          add p stri;
          (while_stream size2) (Ocsigen_stream.next f))
  in
  try
    let size, s = while_stream Int64.zero s in
    let r = stop size p in
    r, s
  with error ->
    let _ = stop Int64.zero p in
    raise error

let scan_multipart_body_from_stream ?max_size ~boundary ~create ~add ~stop s =
  let decode_part = decode_part ~max_size ~create ~add ~stop in
  try
    let s =
      (* read the multipart body: *)
      Ocsigen_stream.next s
    in
    let _ = read_multipart_body ~boundary ~decode_part s in
    ()
  with
  | Ocsigen_stream.Stream_too_small -> raise Ocsigen_lib.Ocsigen_Bad_Request
  | e -> raise e

let get_boundary ctparams = List.assoc "boundary" ctparams

let counter =
  let c = ref (Random.int 1000000) in
  fun () ->
    c := !c + 1;
    !c

let field field content_disp =
  let (_ : int), res =
    S.search_forward (S.regexp (field ^ "=.([^\"]*).;?")) content_disp 0
  in
  S.matched_group res 1 content_disp

let parse_content_type s =
  match Ocsigen_lib.String.split ';' s with
  | [] -> None
  | a :: l -> (
    try
      let typ, subtype = Ocsigen_lib.String.sep '/' a in
      let params =
        try List.map (Ocsigen_lib.String.sep '=') l with Not_found -> []
      in
      (*VVV If syntax error, we return no parameter at all *)
      Some ((typ, subtype), params)
      (*VVV If syntax error in type, we return None *)
    with Not_found -> None)

type content_type = (string * string) * (string * string) list

type file_info =
  { tmp_filename : string
  ; filesize : int64
  ; raw_original_filename : string
  ; file_content_type : ((string * string) * (string * string) list) option }

type post_data = (string * string) list * (string * file_info) list

let post_params_form_urlencoded body_gen _ _ =
  try
    let body = Ocsigen_stream.get body_gen in
    let r =
      (* BY, adapted from a previous comment. Should this stream be
          consumed in case of error? *)
      Ocsigen_stream.string_of_stream
        (Ocsigen_config.get_maxrequestbodysizeinmemory ())
        body
    in
    let r = Ocsigen_lib.Url.fixup_url_string r in
    let l =
      Uri.query_of_encoded r
      |> List.map (fun (s, l) -> List.map (fun v -> s, v) l)
      |> List.concat
    in
    l, []
  with
  | Ocsigen_stream.String_too_large -> raise Ocsigen_lib.Input_is_too_large
  | e -> raise e

let post_params_multipart_form_data ctparams body_gen upload_dir max_size =
  (* Same question here, should this stream be consumed after an
     error? *)
  let body = Ocsigen_stream.get body_gen
  and boundary = get_boundary ctparams
  and params = ref []
  and files = ref []
  and filenames = ref [] in
  let rec add p s =
    match p with
    | _, `No_file to_buf -> Buffer.add_string to_buf s
    | _, `Some_file (_, _, wh, _) ->
        let len = String.length s in
        let r = Unix.write_substring wh s 0 len in
        if r < len
        then (*XXXX Inefficient if s is long *)
          add p (String.sub s r (len - r))
        else Fiber.yield ()
  in
  let create hs =
    let content_type =
      try
        let ct = List.assoc "content-type" hs in
        parse_content_type ct
      with _ -> None
    in
    let cd = List.assoc "content-disposition" hs in
    let p_name = field "name" cd in
    try
      let store = field "filename" cd in
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
          filenames := fname :: !filenames;
          p_name, `Some_file (fname, store, fd, content_type)
      | None -> raise Ocsigen_upload_forbidden
    with Not_found -> p_name, `No_file (Buffer.create 1024)
  and stop filesize = function
    | p_name, `No_file to_buf ->
        params := !params @ [p_name, Buffer.contents to_buf]
    (* in the end ? *)
    | ( p_name
      , `Some_file (tmp_filename, raw_original_filename, wh, file_content_type)
      ) ->
        let file_info =
          {tmp_filename; filesize; raw_original_filename; file_content_type}
        in
        files := !files @ [p_name, file_info];
        Unix.close wh
  in
  scan_multipart_body_from_stream ?max_size ~boundary ~create ~add ~stop body;
  (*VVV Does scan_multipart_body_from_stream read until the end or
    only what it needs?  If we do not consume here, the following
    request will be read only when this one is finished ...  *)
  Ocsigen_stream.consume body_gen;
  !params, !files

let post_params ~content_type body_gen =
  let (ct, cst), ctparams = content_type in
  match String.lowercase_ascii ct, String.lowercase_ascii cst with
  | "application", "x-www-form-urlencoded" ->
      Some
        (body_gen |> Ocsigen_stream.of_cohttp_body
       |> post_params_form_urlencoded)
  | "multipart", "form-data" ->
      Some
        (body_gen |> Ocsigen_stream.of_cohttp_body
        |> post_params_multipart_form_data ctparams)
  | _ -> None
