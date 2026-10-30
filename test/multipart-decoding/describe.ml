let digest s = Digest.to_hex (Digest.string s)

let value v =
  if String.length v <= 64
  then Printf.sprintf "%S" v
  else Printf.sprintf "%d bytes %s" (String.length v) (digest v)

let content_type = function
  | None -> "-"
  | Some ((typ, subtype), params) ->
      String.concat "; "
        ((typ ^ "/" ^ subtype) :: List.map (fun (k, v) -> k ^ "=" ^ v) params)

let file
      ( field
      , { Ocsigen.Multipart.tmp_filename
        ; filesize
        ; raw_original_filename
        ; file_content_type } )
  =
  let content = In_channel.with_open_bin tmp_filename In_channel.input_all in
  let size =
    if Int64.of_int (String.length content) = filesize
    then ""
    else Printf.sprintf " (the file has %d bytes)" (String.length content)
  in
  Printf.sprintf "file %S %S %s %Ld bytes%s %s\n" field raw_original_filename
    (content_type file_content_type)
    filesize size (digest content)

let post_data (params, files) =
  String.concat ""
    (List.map
       (fun (name, v) -> Printf.sprintf "param %S %s\n" name (value v))
       params
    @ List.map file files)
