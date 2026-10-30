let is_digit = function '0' .. '9' -> true | _ -> false

(* The characters of DNS names. RFC 3986 also allows sub-delimiters, such as
   "," or ";", and percent-encoded octets in a registered name. They are
   refused: a proxy in front of the server may read such a name differently
   (as a list, or decoded, as the server matches virtual hosts against the
   decoded name), and choose another virtual host than the server does. *)
let is_reg_name_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '.' | '_' -> true
  | _ -> false

(* The host of uri-host [":" port] (RFC 9110, section 7.2, and RFC 3986,
   section 3.2.2), if this is one: a registered name or an IPv4 address, or an
   IPv6 address in brackets, then optionally a port, which may be empty, of at
   most 65535. *)
let authority_host s =
  let n = String.length s in
  let port_from k =
    k = n
    || s.[k] = ':'
       &&
       let port = String.sub s (k + 1) (n - k - 1) in
       port = ""
       || String.length port <= 5
          && String.for_all is_digit port
          && int_of_string port <= 65535
  in
  let host, port_start =
    if n > 0 && s.[0] = '['
    then
      match String.index_opt s ']' with
      | Some k ->
          let address = String.sub s 1 (k - 1) in
          ( (match Ipaddr.V6.of_string address with
            | Ok _ -> Some address
            | Error _ -> None)
          , k + 1 )
      | None -> None, n
    else
      let k = Option.value (String.index_opt s ':') ~default:n in
      let name = String.sub s 0 k in
      (if String.for_all is_reg_name_char name then Some name else None), k
  in
  match host with Some _ when port_from port_start -> host | _ -> None

(* The authority of an absolute-form request target, such as
   "http://example.org/path" (RFC 9112, section 3.2.2). *)
let absolute_form_authority target =
  let length = String.length target in
  let has_scheme scheme =
    let n = String.length scheme in
    length >= n && String.lowercase_ascii (String.sub target 0 n) = scheme
  in
  let rec authority_end i =
    if i < length && not (String.contains "/?#" target.[i])
    then authority_end (i + 1)
    else i
  in
  let authority start = String.sub target start (authority_end start - start) in
  if has_scheme "http://"
  then Some (authority 7)
  else if has_scheme "https://"
  then Some (authority 8)
  else None

let check_host version headers ~target =
  let field =
    match Http.Header.get_multi headers "host", version with
    | [value], _ -> (
      match authority_host value with
      | Some _ -> Ok ()
      | None -> Error "invalid Host")
    | [], `HTTP_1_0 -> Ok ()
    | [], (`HTTP_1_1 | `Other _) -> Error "missing Host"
    | _ :: _ :: _, _ -> Error "repeated Host"
  in
  match field with
  | Error _ as error -> error
  | Ok () -> (
      if target = "*" || (target <> "" && target.[0] = '/')
      then Ok ()
      else
        (* The host of an absolute-form target replaces Host, and an "http"
           URI must not have an empty host (RFC 9110, section 4.2.1). *)
        match Option.map authority_host (absolute_form_authority target) with
        | Some (Some host) when host <> "" -> Ok ()
        | Some (Some _ | None) -> Error "invalid host in the request target"
        | None -> Error "invalid request target")

(* Field values are made of visible characters, spaces, horizontal tabs and
   non-ASCII bytes (RFC 9110, section 5.5): no other control character. *)
let valid_field_values headers =
  Http.Header.to_list headers
  |> List.for_all (fun (_, value) ->
    String.for_all
      (function '\t' -> true | '\000' .. '\031' | '\127' -> false | _ -> true)
      value)

(* tchar (RFC 9110, section 5.6.2). *)
let is_tchar = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^' | '_' | '`'
  | '|' | '~' ->
      true
  | _ -> false

let request_method = function
  | `Other name ->
      let length = String.length name in
      (* Empty lines end with CRLF, or a bare LF. A bare CR makes the request
         line invalid (RFC 9112, section 2.2). *)
      let rec start i =
        if i + 1 < length && name.[i] = '\r' && name.[i + 1] = '\n'
        then start (i + 2)
        else if i < length && name.[i] = '\n'
        then start (i + 1)
        else i
      in
      let start = start 0 in
      let name = String.sub name start (length - start) in
      if name <> "" && String.for_all is_tchar name
      then Some (Http.Method.of_string name)
      else None
  | meth -> Some meth
