type request =
  { meth : string
  ; target : string
  ; version : string
  ; headers : (string * string) list
  ; body : string
  }

let ( let* ) = Result.bind

let base64 field = function
  | `String s -> (
    match Cryptokit.transform_string (Cryptokit.Base64.decode ()) s with
    | decoded -> Ok decoded
    | exception Cryptokit.Error _ -> Error (field ^ " is not base64")
  )
  | _ -> Error (field ^ " is not a string")

let header = function
  | `List [name; value] ->
      let* name = base64 "a header name" name in
      let* value = base64 "a header value" value in
      Ok (name, value)
  | _ -> Error "a header is not a pair"

let rec all f = function
  | [] -> Ok []
  | x :: rest ->
      let* y = f x in
      let* ys = all f rest in
      Ok (y :: ys)

let of_json s =
  match Yojson.Safe.from_string s with
  | exception Yojson.Json_error message -> Error ("invalid JSON: " ^ message)
  | `Assoc fields -> (
      let field name =
        match List.assoc_opt name fields with
        | Some value -> Ok value
        | None -> Error ("no " ^ name)
      in
      let* meth = Result.bind (field "method") (base64 "method") in
      let* target = Result.bind (field "uri") (base64 "uri") in
      let* version = Result.bind (field "version") (base64 "version") in
      let* body = Result.bind (field "body") (base64 "body") in
      let* headers = field "headers" in
      match headers with
      | `List headers ->
          let* headers = all header headers in
          let version =
            if String.starts_with ~prefix:"HTTP/" version
            then String.sub version 5 (String.length version - 5)
            else version
          in
          Ok {meth; target; version; headers; body}
      | _ -> Error "headers is not a list"
    )
  | _ -> Error "not a JSON object"

let to_string {meth; target; version; headers; body} =
  Printf.sprintf "%S %S %S [%s] %S" meth target version
    (String.concat "; "
       (List.map
          (fun (name, value) -> Printf.sprintf "%S: %S" name value)
          headers
       )
    )
    body
