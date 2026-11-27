type answer = Echoed of Echo.request | Rejected of int | Unreadable of string
type t = {answers : answer list; rest : string; ending : Raw_http.ending}

let answer {Raw_http.status; body; _} =
  if status = 200
  then
    match Echo.of_json body with
    | Ok request -> Echoed request
    | Error message -> Unreadable message
  else Rejected status

let of_outcome {Raw_http.received; ending} =
  let responses, rest =
    Raw_http.responses ~closed:(ending = Raw_http.Closed) received
  in
  { answers =
      List.filter_map
        (fun ({Raw_http.status; _} as response) ->
          if status / 100 = 1 then None else Some (answer response)
        )
        responses
  ; rest
  ; ending
  }

type view = Accepted of Echo.request | Refused | Broken
type quirks = {underscore_names : bool}

let framing_header (name, _) =
  match name with "content-length" | "transfer-encoding" -> true | _ -> false

let is_scheme_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '+' | '-' | '.' -> true
  | _ -> false

(* The authority and the path (with the query) of an absolute-form target,
   such as "http://a/b?c" (RFC 9112 section 3.2.2). *)
let absolute_form target =
  let length = String.length target in
  let rec scheme_end i =
    if i < length && is_scheme_char target.[i] then scheme_end (i + 1) else i
  in
  let i = scheme_end 0 in
  if i > 0 && i + 3 <= length && String.sub target i 3 = "://"
  then
    let rec authority_end j =
      if j = length || target.[j] = '/' || target.[j] = '?'
      then j
      else authority_end (j + 1)
    in
    let j = authority_end (i + 3) in
    let path = String.sub target j (length - j) in
    Some
      ( String.sub target (i + 3) (j - i - 3)
      , if path = "" || path.[0] = '?' then "/" ^ path else path
      )
  else None

(* The fields sorted by name, those of the same name joined in one, in their
   order, as RFC 9110 section 5.3 allows, without whitespace around the
   commas, which servers that join fields write differently. *)
let join fields =
  let elements value =
    String.concat "," (List.map String.trim (String.split_on_char ',' value))
  in
  List.map (fun (name, value) -> name, elements value) fields
  |> List.stable_sort (fun (a, _) (b, _) -> String.compare a b)
  |> List.fold_left
       (fun joined (name, value) ->
         match joined with
         | (n, v) :: rest when n = name -> (n, v ^ "," ^ value) :: rest
         | _ -> (name, value) :: joined
       )
       []
  |> List.rev

let normalize ~ignored quirks (request : Echo.request) =
  let name n =
    let n = String.lowercase_ascii n in
    if quirks.underscore_names
    then String.map (function '_' -> '-' | c -> c) n
    else n
  in
  let headers =
    List.map (fun (n, value) -> name n, value) request.headers
    |> List.filter (fun ((n, _) as h) ->
        not (framing_header h || List.mem n ignored)
    )
  in
  let target, headers =
    match absolute_form request.target with
    | Some (authority, path) ->
        ( path
        , ("host", authority) :: List.filter (fun (n, _) -> n <> "host") headers
        )
    | None -> request.target, headers
  in
  {request with target; headers = join headers}

let view
    ?(ignored = []) ?(quirks = {underscore_names = false}) {answers; rest; _} =
  let ignored = List.map String.lowercase_ascii ignored in
  let views =
    List.map
      (function
        | Echoed request -> Accepted (normalize ~ignored quirks request)
        | Rejected _ -> Refused
        | Unreadable _ -> Broken
        )
      answers
    @ if rest = "" then [] else [Broken]
  in
  let rec drop_refused = function
    | Refused :: rest -> drop_refused rest
    | l -> l
  in
  List.rev (drop_refused (List.rev views))

let clusters ?ignored outcomes =
  List.fold_left
    (fun clusters (name, quirks, outcome) ->
      let v = view ?ignored ~quirks outcome in
      if List.exists (fun (_, v') -> v' = v) clusters
      then
        List.map
          (fun (names, v') -> if v' = v then names @ [name], v' else names, v')
          clusters
      else clusters @ [[name], v]
    )
    [] outcomes

let view_to_string = function
  | [] -> "refused"
  | views ->
      String.concat " | "
        (List.map
           (function
             | Accepted request -> Echo.to_string request
             | Refused -> "refused"
             | Broken -> "broken"
             )
           views
        )

let to_string {answers; rest; ending} =
  String.concat ""
    (List.map
       (fun answer ->
         ( match answer with
           | Echoed request -> Echo.to_string request
           | Rejected status -> Printf.sprintf "status %d" status
           | Unreadable message -> "unreadable: " ^ message
           )
         ^ "\n"
       )
       answers
    )
  ^ (if rest = "" then "" else Printf.sprintf "rest %S\n" rest)
  ^ Raw_http.ending_to_string ending
  ^ "\n"
