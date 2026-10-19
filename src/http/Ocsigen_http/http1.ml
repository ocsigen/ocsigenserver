let is_digit = function '0' .. '9' -> true | _ -> false

(* Unreserved characters and sub-delimiters (RFC 3986, section 2). Unlike RFC
   3986, percent-encoded octets are refused: a host name has no use for them,
   and virtual hosts are matched against the decoded name, which a proxy in
   front of the server does not see. *)
let is_reg_name_char = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '-' | '.' | '_' | '~' | '!' | '$' | '&' | '\'' | '(' | ')' | '*' | '+' | ','
  | ';' | '=' ->
      true
  | _ -> false

(* The host of uri-host [":" port] (RFC 9110, section 7.2, and RFC 3986,
   section 3.2.2), if this is one: a registered name or an IPv4 address, or an
   IPv6 address in brackets, then optionally a port of at most 5 digits. *)
let authority_host s =
  let n = String.length s in
  let port_from k =
    k = n
    ||
    let port = String.sub s (k + 1) (n - k - 1) in
    s.[k] = ':' && String.length port <= 5 && String.for_all is_digit port
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
