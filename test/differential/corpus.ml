type case = {name : string; input : string}

let case line =
  match String.trim line with
  | "" -> Ok None
  | line when line.[0] = '#' -> Ok None
  | line -> (
    match
      Scanf.sscanf line "%[^ \t] %S%!" (fun name input -> {name; input})
    with
    | case -> Ok (Some case)
    | exception (Scanf.Scan_failure message | Failure message) -> Error message
    | exception End_of_file -> Error "no input"
  )

let parse contents =
  let rec lines number acc = function
    | [] -> Ok (List.rev acc)
    | line :: rest -> (
      match case line with
      | Ok None -> lines (number + 1) acc rest
      | Ok (Some case) -> lines (number + 1) (case :: acc) rest
      | Error message -> Error (Printf.sprintf "line %d: %s" number message)
    )
  in
  lines 1 [] (String.split_on_char '\n' contents)

let load path =
  match In_channel.with_open_bin path In_channel.input_all with
  | contents ->
      Result.map_error (fun message -> path ^ ", " ^ message) (parse contents)
  | exception Sys_error message -> Error message
