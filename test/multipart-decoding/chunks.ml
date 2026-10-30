(* chunks.exe CONTENT-TYPE FILE decodes the body in FILE, of type
   CONTENT-TYPE, with [Ocsigen.Multipart.post_params], and prints the
   description of the result (see [Describe]), or the error. It decodes the
   body again cut into chunks: at each offset, if the body is at most 4096
   bytes long, and into chunks of 1, 2, 3, 7, 64 and 4096 bytes. If a result
   differs from the one of the whole body, it prints the number of such
   chunkings and the first different result. *)

open Lwt.Infix

let () =
  Printexc.register_printer (function
    | Ocsigen_base.Lib.Ocsigen_Bad_Request -> Some "Bad request"
    | _ -> None)

let uploads = "chunk-uploads"

(* [decode ~content_type chunks] is the description of the decoding of the
   body made of [chunks]. The files it creates are removed. *)
let decode ~content_type chunks =
  let created = ref [] in
  let on_file f = created := f :: !created in
  let body = Cohttp_lwt.Body.of_stream (Lwt_stream.of_list chunks) in
  let result =
    match Ocsigen.Multipart.post_params ~content_type ~on_file body with
    | None -> "not form data\n"
    | Some decode ->
        Lwt_main.run
          (Lwt.catch
             (fun () -> decode (Some uploads) None >|= Describe.post_data)
             (fun e ->
                Lwt.return (Printf.sprintf "error %s\n" (Printexc.to_string e))))
  in
  List.iter Sys.remove !created;
  result

(* [chunks_of n s] is [s] cut into chunks of [n] bytes, the last one maybe
   shorter. *)
let chunks_of n s =
  let len = String.length s in
  List.init
    ((len + n - 1) / n)
    (fun i -> String.sub s (i * n) (min n (len - (i * n))))

(* The chunkings of [body] other than the whole body, with their names *)
let chunkings body =
  let len = String.length body in
  let cuts =
    if len > 4096
    then []
    else
      List.init (len + 1) (fun i ->
        ( Printf.sprintf "a cut at %d" i
        , [String.sub body 0 i; String.sub body i (len - i)] ))
  in
  cuts
  @ List.map
      (fun n -> Printf.sprintf "chunks of %d bytes" n, chunks_of n body)
      [1; 2; 3; 7; 64; 4096]

let () =
  match Sys.argv with
  | [|_; content_type; file|] -> (
    match Ocsigen.Multipart.parse_content_type content_type with
    | None ->
        prerr_endline ("chunks.exe: bad Content-Type " ^ content_type);
        exit 2
    | Some content_type -> (
        if not (Sys.file_exists uploads) then Sys.mkdir uploads 0o755;
        let body = In_channel.with_open_bin file In_channel.input_all in
        let reference = decode ~content_type [body] in
        print_string reference;
        let differences =
          List.filter_map
            (fun (name, chunks) ->
               let result = decode ~content_type chunks in
               if result = reference then None else Some (name, result))
            (chunkings body)
        in
        match differences with
        | [] -> ()
        | (name, result) :: _ ->
            Printf.printf "%d chunkings give another result, first %s:\n%s"
              (List.length differences) name result))
  | _ ->
      prerr_endline "Usage: chunks.exe CONTENT-TYPE FILE";
      exit 2
