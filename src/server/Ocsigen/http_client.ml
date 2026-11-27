open Lwt.Infix

let call
    ?(ctx = Lazy.force Cohttp_lwt_unix.Net.default_ctx)
    ?interrupted
    ?headers
    ?body
    ?absolute_form
    meth
    uri =
  let module Connection = Cohttp_lwt_unix.Connection in
  Cohttp_lwt_unix.Net.resolve ~ctx uri
  (* Persistent, as cohttp does, so that a chunked body is sent chunked. *)
  >>= Connection.connect ~ctx ~persistent:true
  >>= fun connection ->
  Lwt.catch
    (fun () -> Connection.call connection ?headers ?body ?absolute_form meth uri)
    (fun exn ->
      Connection.close connection;
      Lwt.reraise exn
    )
  >|= fun (response, body) ->
  ( match body with
  | `Stream stream ->
      let finished =
        match interrupted with
        | None -> Lwt_stream.closed stream
        | Some interrupted ->
            (* [Lwt.choose] removes its callback from the promise that does
               not resolve, so that they do not pile up on [interrupted],
               which may stay pending for long. *)
            Lwt.choose [Lwt_stream.closed stream; interrupted]
      in
      Lwt.on_termination finished (fun () -> Connection.close connection)
  | `Empty | `String _ | `Strings _ -> Connection.close connection
  );
  ( response
  , body
  , fun () ->
      Connection.close connection;
      Cohttp_lwt.Body.drain_body body )

let install () =
  Cohttp_lwt_unix.Client.set_cache
    (fun ?headers ?body ?absolute_form meth uri ->
      call ?headers ?body ?absolute_form meth uri >|= fun (response, body, _) ->
      response, body
  )
