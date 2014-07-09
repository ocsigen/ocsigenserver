{

type t =
  | Host
  | L
  (* | User *)
  | Time
  | Request
  | Status
  | Byte (* XXX: byte receive (no byte send) *)
  | Header of string
  | Other of char
  | EOF

let string_of_month = function
  | 0 -> "Jan"
  | 1 -> "Feb"
  | 2 -> "Mar"
  | 3 -> "Apr"
  | 4 -> "May"
  | 5 -> "Jun"
  | 6 -> "Jul"
  | 7 -> "Aug"
  | 8 -> "Sep"
  | 9 -> "Oct"
  | 10 -> "Nov"
  | 11 -> "Dec"
  | _ -> raise (Invalid_argument "Ocsigen_log.string_of_month")

let to_string ri res fmt =
  let rec aux buffer = function
    | Host ->
      Buffer.add_string buffer
      (Ocsigen_request_info.host ri |> function
       | None -> "0.0.0.0"
       | Some host -> host)
    | L -> Buffer.add_string buffer "-"
    | Time ->
      let time = Unix.localtime (Ocsigen_request_info.timeofday ri) in
      Printf.bprintf buffer "[%02d/%s/%04d:%02d:%02d:%02d]"
        time.Unix.tm_mday
        (string_of_month time.Unix.tm_mon)
        (time.Unix.tm_year + 1900)
        time.Unix.tm_hour
        time.Unix.tm_min
        time.Unix.tm_sec
    | Request ->
      Printf.bprintf buffer "%s /%s %s"
        (Ocsigen_request_info.meth ri
         |> To_cohttp.to_meth
         |> Cohttp.Code.string_of_method)
        (Ocsigen_request_info.full_path_string ri)
        (Ocsigen_request_info.protocol ri
         |> To_cohttp.to_version
         |> Cohttp.Code.string_of_version)
    | Status ->
      Printf.bprintf buffer "%s"
      (Cohttp.Code.string_of_status (Cohttp.Response.status res))
    | Byte ->
      Printf.bprintf buffer "%Ld"
      (Ocsigen_request_info.content_length ri |> function
       | None -> Int64.zero
       | Some x -> x)
    | Header name ->
      let str =
        try
          Http_headers.find
            (Http_headers.name name)
            ((Ocsigen_request_info.http_frame ri).frame_header
             |> Ocsigen_http_frame.Http_header.get_headers)
        with Not_found -> ""
      in Buffer.add_string buffer str
    | Other c -> Buffer.add_char buffer c
    | EOF -> ()
  in
  let buffer = Buffer.create 16 in
  List.iter (aux buffer) fmt; Buffer.contents buffer

}

let ALPHA = ['A' - 'Z' 'a' - 'z']
let DIGIT = ['0' - '9']
let NAME = ALPHA (ALPHA | DIGIT | ['-'])*

rule token = parse
  | eof                   { EOF }
  | "%h"                  { Host }
  | "%l"                  { L }
  | "%t"                  { Time }
  | "%r"                  { Request }
  | "%b"                  { Byte }
  | "%{" (NAME as n) "}i" { Header n}
  | _ as c                { Other c }

{

let of_string str =
  let lexbuf = Lexing.from_string str in
  let rec aux acc = match token lexbuf with
    | EOF -> List.rev (EOF :: acc)
    | token -> aux (token :: acc)
  in aux []

}
