(* Unit tests for [Ocsigen.Multipart.scan_multipart_body_from_stream]: the
   [stop] function of a part, which closes its file, is called once, even when
   it fails, and the first error is the one reported. [Stop_failed n] is the
   failure of the [n]th call to [stop]. A failure of [stop] after another
   error is logged. A malformed body is a bad request, and the reason is
   logged. *)

module S = Ocsigen_base.Ocsigen_stream

exception Add_failed
exception Stop_failed of int

let () =
  Printexc.register_printer (function
    | Add_failed -> Some "Add_failed"
    | Stop_failed n -> Some (Printf.sprintf "Stop_failed %d" n)
    | Ocsigen_base.Lib.Ocsigen_Bad_Request -> Some "Bad request"
    | _ -> None)

let body =
  "--B\r\nContent-Disposition: form-data; name=\"f\"\r\n\r\nx\r\n--B--\r\n"

(* [scan ~add ~stop body] is "ok" if [body] is decoded, or the error. *)
let scan ~add ~stop body =
  Lwt_main.run
    (Lwt.catch
       (fun () ->
          Lwt.map
            (fun () -> "ok")
            (Ocsigen.Multipart.scan_multipart_body_from_stream ~boundary:"B"
               ~create:(fun _ -> ())
               ~add ~stop
               (S.get (S.of_string body))))
       (fun e -> Lwt.return (Printexc.to_string e)))

let show ~add_fails ~stop_fails =
  let calls = ref 0 in
  let add () _ = if add_fails then Lwt.fail Add_failed else Lwt.return () in
  let stop _ () =
    incr calls;
    if stop_fails then Lwt.fail (Stop_failed !calls) else Lwt.return ()
  in
  let result = scan ~add ~stop body in
  Printf.printf "add %s, stop %s -> %s, stop called %d time(s)\n"
    (if add_fails then "fails" else "succeeds")
    (if stop_fails then "fails" else "succeeds")
    result !calls

let show_malformed body =
  let result =
    scan ~add:(fun () _ -> Lwt.return ()) ~stop:(fun _ () -> Lwt.return ()) body
  in
  Printf.printf "%S -> %s\n" body result

let () =
  Logs.set_reporter (Logs.format_reporter ~dst:Format.std_formatter ());
  show ~add_fails:false ~stop_fails:false;
  show ~add_fails:false ~stop_fails:true;
  show ~add_fails:true ~stop_fails:false;
  show ~add_fails:true ~stop_fails:true;
  Logs.Src.set_level Ocsigen.Multipart.section (Some Logs.Info);
  show_malformed "--B\r\nfoo\r\n\r\nx\r\n--B--\r\n";
  show_malformed "--B\r\n Content-Disposition: form-data\r\n\r\nx\r\n--B--\r\n";
  show_malformed "--B";
  show_malformed "--Bx";
  show_malformed "no boundary\r\n";
  show_malformed "--B\r\nContent-Disposition: form-data; name=\"f\"\r\n\r\nx"
