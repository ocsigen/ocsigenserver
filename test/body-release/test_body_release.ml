(* A response that is sent without its body, as the response to HEAD, or
   with a 304 status, must still free what the body would have freed: the
   body of a file response is what closes the file. *)

open Lwt.Syntax

let fail fmt = Printf.ksprintf (fun s -> prerr_endline s; exit 1) fmt
let open_files () = Array.length (Sys.readdir "/dev/fd")

let send ~meth response =
  let _, write = Ocsigen.Response.to_response_expert ~meth response in
  write () Lwt_io.null

let check what f =
  let n = 20 in
  let* () = f () in
  let before = open_files () in
  let rec loop i =
    if i = 0
    then Lwt.return_unit
    else
      let* () = f () in
      loop (i - 1)
  in
  let* () = loop n in
  let after = open_files () in
  if after <> before
  then
    fail "%s: %d files open before %d responses, %d after" what before n after;
  Lwt.return_unit

(* A response from Cohttp, whose body is not read yet. *)
let cohttp_response () =
  let stream, push = Lwt_stream.create () in
  push (Some "content");
  push None;
  stream, (Cohttp.Response.make ~status:`OK (), `Stream stream)

let () =
  let file = Filename.temp_file "body_release" ".txt" in
  at_exit (fun () -> Sys.remove file);
  Out_channel.with_open_bin file (fun oc -> output_string oc "content\n");
  Lwt_main.run
    (let* () =
       check "GET" (fun () ->
         let* response = Ocsigen.Response.respond_file file in
         send ~meth:`GET response)
     in
     let* () =
       check "HEAD" (fun () ->
         let* response = Ocsigen.Response.respond_file file in
         send ~meth:`HEAD response)
     in
     let* () =
       check "304" (fun () ->
         let* response = Ocsigen.Response.respond_file file in
         send ~meth:`GET (Ocsigen.Response.set_status response `Not_modified))
     in
     (* A body from Cohttp is read to the end by default... *)
     let stream, response = cohttp_response () in
     let* () = send ~meth:`HEAD (Ocsigen.Response.of_cohttp response) in
     let* drained = Lwt_stream.is_empty stream in
     if not drained then fail "of_cohttp: the body is not read";
     (* ... unless its producer frees it another way. *)
     let stream, response = cohttp_response () in
     let released = ref false in
     let release () =
       released := true;
       Lwt.return_unit
     in
     let* () =
       send ~meth:`HEAD (Ocsigen.Response.of_cohttp ~release response)
     in
     let* drained = Lwt_stream.is_empty stream in
     if drained || not !released
     then fail "of_cohttp ~release: the body is read, or not released";
     Lwt.return_unit)
