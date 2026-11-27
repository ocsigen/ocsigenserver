(* Property tests of the parsers of HTTP messages, with Crowbar: they run as
   random tests in dune test, and can be run with AFL for a long search (see
   README.md). *)

open Crowbar

(* {1 Generators} *)

(* One of the characters of [chars], all equally likely. *)
let one_of chars = map [range (String.length chars)] (String.get chars)
let chars_between lo hi = String.init (hi - lo + 1) (fun i -> Char.chr (lo + i))
let string_of char = map [list char] (fun l -> String.of_seq (List.to_seq l))
let string1_of char = map [list1 char] (fun l -> String.of_seq (List.to_seq l))
let digits = chars_between 0x30 0x39
let letters = chars_between 0x41 0x5a ^ chars_between 0x61 0x7a

(* tchar (RFC 9110 section 5.6.2). *)
let tchars = "!#$%&'*+-.^_`|~" ^ digits ^ letters
let token = string1_of (one_of tchars)

(* {1 Entity tags} *)

module Entity_tag = Ocsigen_http.Header.Entity_tag

(* etagc (RFC 9110 section 8.8.3): any visible byte but the double quote, and
   obs-text. *)
let etagc = "!" ^ chars_between 0x23 0x7e ^ chars_between 0x80 0xff

(* The comma, which separates entity tags but may also appear in them, is
   made more frequent. *)
let opaque = string_of (choose [one_of etagc; const ','])

let entity_tag =
  map [bool; opaque] (fun weak opaque -> Entity_tag.make ~weak opaque)

let pp_tags ppf tags =
  pp ppf "%s" (String.concat ", " (List.map Entity_tag.to_string tags))

(* Whitespace around list elements (RFC 9110 section 5.6.1). *)
let ows = choose [const ""; const " "; const "\t"; const " \t "]

(* List elements that are not entity tags: no quote, comma or star. *)
let junk =
  string1_of
    (one_of
       (String.of_seq
          (Seq.filter
             (fun c -> not (String.contains "\",*" c))
             (String.to_seq etagc)
          )
       )
    )

type element = Tag of Entity_tag.t | Junk of string | Empty

let element =
  choose
    [ map [entity_tag] (fun t -> Tag t)
    ; map [junk] (fun j -> Junk j)
    ; const Empty
    ]

let () =
  add_test ~name:"Entity_tag.parse_list never fails"
    [list bytes]
    (fun values -> ignore (Entity_tag.parse_list values));
  (* The entity tags of a list are read, in order, whatever the whitespace,
     the empty elements, the elements that are not entity tags, and the lines
     the list is written on. *)
  add_test ~name:"Entity_tag.parse_list reads the tags of a list"
    [list (map [ows; element; ows] (fun a e b -> a, e, b)); bool]
    (fun elements one_per_line ->
      let text (before, element, after) =
        before
        ^ ( match element with
          | Tag t -> Entity_tag.to_string t
          | Junk j -> j
          | Empty -> ""
          )
        ^ after
      in
      let texts = List.map text elements in
      let values = if one_per_line then texts else [String.concat "," texts] in
      let expected =
        List.filter_map
          (function _, Tag t, _ -> Some t | _, (Junk _ | Empty), _ -> None)
          elements
      in
      match Entity_tag.parse_list values with
      | `Tags parsed -> check_eq ~pp:pp_tags expected parsed
      | `Any -> fail "*"
    );
  add_test ~name:"Entity_tag.make accepts exactly etagc" [bool; bytes]
    (fun weak s ->
      let valid = String.for_all (String.contains etagc) s in
      match Entity_tag.make ~weak s with
      | tag ->
          check_eq ~pp:pp_bool true valid;
          check_eq ~pp:pp_string s tag.opaque
      | exception Invalid_argument _ -> check_eq ~pp:pp_bool false valid
  );
  (* Equal opaque tags are made frequent. *)
  add_test ~name:"Entity_tag comparisons"
    [bool; bool; choose [pair opaque opaque; map [opaque] (fun o -> o, o)]]
    (fun weak1 weak2 (opaque1, opaque2) ->
      let a = Entity_tag.make ~weak:weak1 opaque1
      and b = Entity_tag.make ~weak:weak2 opaque2 in
      check_eq ~pp:pp_bool (opaque1 = opaque2) (Entity_tag.weak_equal a b);
      check_eq ~pp:pp_bool
        (opaque1 = opaque2 && (not weak1) && not weak2)
        (Entity_tag.strong_equal a b)
    )

(* {1 Dates} *)

let year = 365.2425 *. 86400.

let long_day_names =
  [| "Sunday"
   ; "Monday"
   ; "Tuesday"
   ; "Wednesday"
   ; "Thursday"
   ; "Friday"
   ; "Saturday"
  |]

let gmtime = Ocsigen_base.Lib.Date.gmtime
let month_name = Ocsigen_base.Lib.Date.name_of_month
let day_name i = String.sub long_day_names.(i) 0 3

(* Times in whole seconds from 1970 to 9999, when the preferred format has a
   four-digit year, made of days and seconds to stay within 31-bit integers.
   The years up to 2100, and the days around the leap days of 2000 and 2100,
   are made frequent. *)
let time =
  let of_day day second = (Float.of_int day *. 86400.) +. Float.of_int second in
  choose
    [ map [range 2932897; range 86400] of_day
    ; map [range 47482; range 86400] of_day
    ; map
        [ choose [const 11016; const 11017; const 47540; const 47541]
        ; range 86400
        ]
        of_day
    ]

let rfc850 t =
  let tm = gmtime t in
  Printf.sprintf "%s, %02d-%s-%02d %02d:%02d:%02d GMT"
    long_day_names.(tm.tm_wday)
    tm.tm_mday (month_name tm.tm_mon) (tm.tm_year mod 100) tm.tm_hour tm.tm_min
    tm.tm_sec

let asctime t =
  let tm = gmtime t in
  Printf.sprintf "%s %s %2d %02d:%02d:%02d %04d" (day_name tm.tm_wday)
    (month_name tm.tm_mon) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec
    (tm.tm_year + 1900)

let pp_date ppf = function
  | Some t -> pp ppf "%.0f" t
  | None -> pp ppf "invalid"

let days_in_month year month =
  match month with
  | 1 ->
      if (year mod 4 = 0 && year mod 100 <> 0) || year mod 400 = 0
      then 29
      else 28
  | 3 | 5 | 8 | 10 -> 30
  | _ -> 31

let () =
  add_test ~name:"Date.parse never fails" [bytes] (fun s ->
      ignore (Ocsigen_http.Header.Date.parse s)
  );
  add_test ~name:"Date.parse reads the preferred format" [time] (fun t ->
      check_eq ~pp:pp_date (Some t)
        (Ocsigen_http.Header.Date.parse (Ocsigen_base.Lib.Date.to_string t))
  );
  add_test ~name:"Date.parse reads the asctime format" [time] (fun t ->
      check_eq ~pp:pp_date (Some t) (Ocsigen_http.Header.Date.parse (asctime t))
  );
  (* A two-digit year is the latest year with these digits that does not put
     the date more than 50 years after [now] (RFC 9110 section 5.6.7). *)
  add_test ~name:"Date.parse reads the RFC 850 format"
    [time; range 200]
    (fun t half_years ->
      let now = t +. (Float.of_int (half_years - 100) *. year /. 2.) in
      let tm = gmtime t in
      match Ocsigen_http.Header.Date.parse ~now (rfc850 t) with
      | None ->
          (* Only a February 29 can fall on a year that is not a leap one. *)
          check_eq ~pp:pp_int 29 tm.tm_mday;
          check_eq ~pp:pp_int 1 tm.tm_mon
      | Some r ->
          let tm' = gmtime r in
          check_eq ~pp:pp_int 0 ((tm'.tm_year - tm.tm_year) mod 100);
          check_eq ~pp:pp_int tm.tm_mon tm'.tm_mon;
          check_eq ~pp:pp_int tm.tm_mday tm'.tm_mday;
          check_eq ~pp:pp_float (Float.rem t 86400.) (Float.rem r 86400.);
          check (r <= now +. (50. *. year));
          (* A century later would be too far, up to the days that leap years
             add or remove over a century. *)
          check (r +. (100. *. year) > now +. (50. *. year) -. (2. *. 86400.))
    );
  (* Fields in wide ranges: a date is read iff its fields are valid, and then
     it is the time with these fields. The day name is not checked against
     the date. *)
  add_test ~name:"Date.parse checks the fields"
    [ range 7
    ; range 40
    ; range 13
    ; range ~min:1970 8030
    ; range 100
    ; range 100
    ; range 100
    ]
    (fun weekday day month year hour minute second ->
      let s =
        Printf.sprintf "%s, %02d %s %04d %02d:%02d:%02d GMT" (day_name weekday)
          day
          (if month = 12 then "Foo" else month_name month)
          year hour minute second
      in
      let valid =
        month < 12 && day >= 1
        && day <= days_in_month year month
        && hour < 24 && minute < 60 && second <= 60
      in
      match Ocsigen_http.Header.Date.parse s with
      | None -> check_eq ~pp:pp_bool false valid
      | Some t ->
          check_eq ~pp:pp_bool true valid;
          (* Second 60 is read as the first second of the next minute. *)
          let tm = gmtime (if second = 60 then t -. 1. else t) in
          check_eq ~pp:pp_int (year - 1900) tm.tm_year;
          check_eq ~pp:pp_int month tm.tm_mon;
          check_eq ~pp:pp_int day tm.tm_mday;
          check_eq ~pp:pp_int hour tm.tm_hour;
          check_eq ~pp:pp_int minute tm.tm_min;
          check_eq ~pp:pp_int (min second 59) tm.tm_sec
    )

(* {1 Request line and header fields} *)

let version = choose [const `HTTP_1_0; const `HTTP_1_1]
let fields = list (pair bytes bytes)
let reg_name = string1_of (one_of (letters ^ digits ^ "._-"))

let ipv4 =
  map
    [range 256; range 256; range 256; range 256]
    (fun a b c d -> Printf.sprintf "%d.%d.%d.%d" a b c d)

let ipv6 =
  map [int32; int32; int32; int32] (fun a b c d ->
      "[" ^ Ipaddr.V6.to_string (Ipaddr.V6.of_int32 (a, b, c, d)) ^ "]"
  )

let port = choose [const ""; map [range 65536] (fun p -> ":" ^ string_of_int p)]
let host_name = choose [reg_name; ipv4; ipv6]

(* A Host value: empty, or a host name with an optional port. *)
let host = choose [const ""; map [host_name; port] ( ^ )]

(* A character that a host cannot have. *)
let bad_char = one_of " ,;%@\"<>{}|\\^`\x00\r\n\t\x7f\xe9"

let origin_target =
  map [string_of (one_of (letters ^ digits ^ "/?=&%"))] (( ^ ) "/")

let pp_check ppf = function
  | Ok () -> pp ppf "Ok"
  | Error reason -> pp ppf "Error %s" reason

let check_host version host ~target =
  Ocsigen_http.Http1.check_host version
    (Http.Header.of_list ["Host", host])
    ~target

let () =
  add_test ~name:"Http1.check_host never fails" [version; fields; bytes]
    (fun version fields target ->
      ignore
        (Ocsigen_http.Http1.check_host version
           (Http.Header.of_list fields)
           ~target
        )
  );
  add_test ~name:"Http1.check_host accepts valid hosts"
    [version; host; origin_target] (fun version host target ->
      check_eq ~pp:pp_check (Ok ()) (check_host version host ~target)
  );
  add_test ~name:"Http1.check_host accepts valid absolute targets"
    [version; host; host_name; port; origin_target]
    (fun version host authority port path ->
      check_eq ~pp:pp_check (Ok ())
        (check_host version host ~target:("http://" ^ authority ^ port ^ path))
  );
  add_test ~name:"Http1.check_host refuses invalid hosts"
    [version; reg_name; bad_char; reg_name; origin_target]
    (fun version before c after target ->
      check
        (Result.is_error
           (check_host version (before ^ String.make 1 c ^ after) ~target)
        )
  );
  add_test ~name:"Http1.check_host refuses ports over 65535"
    [version; reg_name; range ~min:65536 1_000_000; origin_target]
    (fun version name port target ->
      check
        (Result.is_error
           (check_host version (name ^ ":" ^ string_of_int port) ~target)
        )
    );
  add_test ~name:"Http1.request_method gives a token" [bytes] (fun s ->
      match Ocsigen_http.Http1.request_method (Http.Method.of_string s) with
      | Some meth ->
          let s = Http.Method.to_string meth in
          check (s <> "" && String.for_all (String.contains tchars) s)
      | None -> ()
  );
  (* Empty lines before a request line are skipped (RFC 9112 section 2.2). *)
  add_test ~name:"Http1.request_method skips empty lines"
    [list (choose [const "\r\n"; const "\n"]); token]
    (fun lines token ->
      match
        Ocsigen_http.Http1.request_method
          (Http.Method.of_string (String.concat "" lines ^ token))
      with
      | Some meth -> check_eq ~pp:pp_string token (Http.Method.to_string meth)
      | None -> fail "rejected"
    );
  add_test ~name:"Http1.is_tchar"
    [range 256]
    (fun c ->
      check_eq ~pp:pp_bool
        (String.contains tchars (Char.chr c))
        (Ocsigen_http.Http1.is_tchar (Char.chr c))
    );
  (* field-vchar, SP and HTAB (RFC 9110 section 5.5). *)
  add_test ~name:"Http1.is_field_value_char"
    [range 256]
    (fun c ->
      check_eq ~pp:pp_bool
        (c = 0x09 || (c >= 0x20 && c <> 0x7f))
        (Ocsigen_http.Http1.is_field_value_char (Char.chr c))
    );
  add_test ~name:"Http1.valid_field_values" [fields] (fun fields ->
      check_eq ~pp:pp_bool
        (List.for_all
           (fun (_, value) ->
             String.for_all Ocsigen_http.Http1.is_field_value_char value
           )
           fields
        )
        (Ocsigen_http.Http1.valid_field_values (Http.Header.of_list fields))
  )

(* {1 Responses} *)

type response =
  { interim : int option  (** A 1xx response before this one. *)
  ; status : int
  ; headers : (string * string) list
  ; close : bool  (** Whether it says Connection: close. *)
  ; body : string
  ; chunked : (int list * bool * bool * bool) option
        (** The sizes of the chunks, cycled, whether they are written in
          uppercase, with an extension, and followed by a trailer. *)
  }

(* Field names that do not change the framing, and values without line ends
   nor whitespace at their ends, which Raw_http drops. *)
let field =
  map
    [ string1_of (one_of (chars_between 0x61 0x7a))
    ; string_of (one_of (chars_between 0x21 0x7e))
    ]
    (fun name value -> "x-" ^ name, value)

let chunk_size =
  choose [range ~min:1 15; range ~min:16 300; const 255; const 256]

let response =
  map
    [ option (choose [const 100; const 103])
    ; choose [range ~min:200 400; const 204; const 304]
    ; list field
    ; bool
    ; map [list bytes] (String.concat "")
    ; option
        (map [list1 chunk_size; bool; bool; bool] (fun s u e t -> s, u, e, t))
    ]
    (fun interim status headers close body chunked ->
      if status = 204 || status = 304
      then {interim; status; headers; close; body = ""; chunked = None}
      else {interim; status; headers; close; body; chunked}
    )

(* [body] in chunks of the sizes [sizes], cycled, then the last chunk. *)
let chunked body (sizes, uppercase, extension, trailer) =
  let buffer = Buffer.create (String.length body * 2) in
  let extension = if extension then ";a=\"b;c\"" else "" in
  let rec add pos = function
    | [] -> add pos sizes
    | size :: rest ->
        if pos < String.length body
        then (
          let n = min size (String.length body - pos) in
          Printf.bprintf buffer "%s%s\r\n%s\r\n"
            (if uppercase then Printf.sprintf "%X" n else Printf.sprintf "%x" n)
            extension (String.sub body pos n);
          add (pos + n) rest
        )
  in
  add 0 sizes;
  Printf.bprintf buffer "0%s\r\n%s\r\n" extension
    (if trailer then "X-Trailer: 1\r\n" else "");
  Buffer.contents buffer

(* The fields of [r] as [encode] writes them. *)
let fields_of r =
  r.headers
  @ (if r.close then ["Connection", "close"] else [])
  @
  match r.status, r.chunked with
  | (204 | 304), _ -> []
  | _, Some _ -> ["Transfer-Encoding", "chunked"]
  | _, None -> ["Content-Length", string_of_int (String.length r.body)]

let field_lines fields =
  String.concat ""
    (List.map (fun (n, v) -> Printf.sprintf "%s: %s\r\n" n v) fields)

type part =
  {text : string; parsed : Raw_http.http_response; final : bool; closes : bool}

(* The responses of [r] as they are sent, with what Raw_http must read. *)
let encode r =
  let interim =
    match r.interim with
    | Some code ->
        [ { text = Printf.sprintf "HTTP/1.1 %d X\r\n\r\n" code
          ; parsed = {status = code; headers = []; body = ""}
          ; final = false
          ; closes = false
          }
        ]
    | None -> []
  in
  let content =
    match r.chunked with
    | Some chunks when r.status <> 204 && r.status <> 304 ->
        chunked r.body chunks
    | Some _ | None -> r.body
  in
  interim
  @ [ { text =
          Printf.sprintf "HTTP/1.1 %d X\r\n%s\r\n%s" r.status
            (field_lines (fields_of r))
            content
      ; parsed = {status = r.status; headers = fields_of r; body = r.body}
      ; final = true
      ; closes = r.close
      }
    ]

let pp_response ppf {Raw_http.status; headers; body} =
  pp ppf "%d [%s] %S" status
    (String.concat "; " (List.map (fun (n, v) -> n ^ "=" ^ v) headers))
    body

let pp_framing ppf = function
  | Raw_http.Complete {close} -> pp ppf "complete (close %b)" close
  | Incomplete -> pp ppf "incomplete"
  | Unframed -> pp ppf "unframed"

(* [n] made a position in a string of [length] bytes, whatever its sign. *)
let position n length = n land max_int mod (length + 1)

let () =
  add_test ~name:"Raw_http.responses never fails" [bool; bytes] (fun closed s ->
      ignore (Raw_http.framing s);
      ignore (Raw_http.responses ~closed s)
  );
  (* Cut anywhere, a sequence of responses gives its complete responses, the
     rest of the bytes, and a framing that says whether it ends just after a
     final response. *)
  add_test ~name:"Raw_http.responses reads the complete responses"
    [list response; int]
    (fun responses cut ->
      let parts = List.concat_map encode responses in
      let s = String.concat "" (List.map (fun p -> p.text) parts) in
      let cut = position cut (String.length s) in
      (* The parts that end before [cut], and the last one with its end. *)
      let rec complete pos last = function
        | part :: rest when pos + String.length part.text <= cut ->
            let stop = pos + String.length part.text in
            let parts, last = complete stop (Some (stop, part)) rest in
            part :: parts, last
        | _ -> [], last
      in
      let expected, last = complete 0 None parts in
      let parsed, rest = Raw_http.responses (String.sub s 0 cut) in
      check_eq ~pp:(pp_list pp_response)
        (List.map (fun p -> p.parsed) expected)
        parsed;
      let stop = match last with Some (stop, _) -> stop | None -> 0 in
      check_eq ~pp:pp_string (String.sub s stop (cut - stop)) rest;
      check_eq ~pp:pp_framing
        ( match last with
        | Some (stop, {final = true; closes; _}) when stop = cut ->
            Complete {close = closes}
        | Some _ | None -> Incomplete
        )
        (Raw_http.framing (String.sub s 0 cut))
    );
  (* Bytes inserted or deleted anywhere in a sequence of responses: the
     framing agrees with the responses that are read. *)
  add_test ~name:"Raw_http.framing agrees with responses"
    [list response; list (pair int (option char))]
    (fun responses edits ->
      let s =
        String.concat ""
          (List.concat_map
             (fun r -> List.map (fun p -> p.text) (encode r))
             responses
          )
      in
      let edit s (n, insert) =
        let i = position n (String.length s) in
        let before = String.sub s 0 i
        and after = String.sub s i (String.length s - i) in
        match insert with
        | Some c -> before ^ String.make 1 c ^ after
        | None when after = "" -> before
        | None -> before ^ String.sub after 1 (String.length after - 1)
      in
      let s = List.fold_left edit s edits in
      let parsed, rest = Raw_http.responses s in
      let ends_final =
        match List.rev parsed with
        | {Raw_http.status; _} :: _ -> status / 100 <> 1
        | [] -> false
      in
      check_eq ~pp:pp_bool
        (rest = "" && ends_final)
        ( match Raw_http.framing s with
        | Complete _ -> true
        | Incomplete | Unframed -> false
        )
    );
  (* With the connection closed, a response without length takes the rest of
     the bytes. *)
  add_test ~name:"Raw_http.responses reads a response to the end"
    [ choose [range ~min:200 4; range ~min:205 99; range ~min:305 295]
    ; list field
    ; bytes
    ]
    (fun status headers body ->
      let s =
        Printf.sprintf "HTTP/1.1 %d X\r\n%s\r\n%s" status (field_lines headers)
          body
      in
      check_eq ~pp:(pp_list pp_response)
        [{Raw_http.status; headers; body}]
        (fst (Raw_http.responses ~closed:true s))
    )
