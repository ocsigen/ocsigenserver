(* Tests of the parts of the differential tests that need no server. *)

open Differential

let base64 s =
  Cryptokit.transform_string (Cryptokit.Base64.encode_compact_pad ()) s

let echo
    ?(meth = "GET")
    ?(target = "/")
    ?(version = "HTTP/1.1")
    ?(headers = ["Host", "a"])
    ?(body = "")
    () =
  let string s = "\"" ^ base64 s ^ "\"" in
  Printf.sprintf
    {|{"method":%s,"uri":%s,"version":%s,"headers":[%s],"body":%s}|}
    (string meth) (string target) (string version)
    (String.concat ","
       (List.map
          (fun (name, value) ->
            Printf.sprintf "[%s,%s]" (string name) (string value)
          )
          headers
       )
    )
    (string body)

let () =
  print_endline "== Corpus.parse";
  List.iter
    (fun contents ->
      Printf.printf "%S -> %s\n" contents
        ( match Corpus.parse contents with
        | Ok cases ->
            String.concat "; "
              (List.map
                 (fun {Corpus.name; input} -> Printf.sprintf "%s=%S" name input)
                 cases
              )
        | Error message -> "error: " ^ message
        )
    )
    [ "# comment\n\na \"GET\\r\\n\"\n  b\t\"x\\000\"  \n"
    ; "a \"unterminated\n"
    ; "a\n"
    ; "\"no name\"\n"
    ; "a \"x\" trailing\n"
    ]

let () =
  print_endline "== Echo.of_json";
  List.iter
    (fun json ->
      print_endline
        ( match Echo.of_json json with
        | Ok request -> Echo.to_string request
        | Error message -> "error: " ^ message
        )
    )
    [ echo ~headers:["Host", "a"; "X-A", "1"; "X-A", "2"] ~body:"b\r\n" ()
    ; echo ~version:"1.0" ()
    ; {|{"method":"R0VU","uri":"Lw==","version":"MS4x","headers":[]}|}
    ; {|{"method":"!!!","uri":"Lw==","version":"MS4x","headers":[],"body":""}|}
    ; {|{"method":1,"uri":"Lw==","version":"MS4x","headers":[],"body":""}|}
    ; {|{"method":"R0VU","uri":"Lw==","version":"MS4x","headers":[["SA=="]],"body":""}|}
    ; "[]"
    ; "{"
    ]

let response status body =
  Printf.sprintf "HTTP/1.1 %d X\r\nContent-Length: %d\r\n\r\n%s" status
    (String.length body) body

let outcome ?(ending = Raw_http.Idle) responses =
  Outcome.of_outcome {Raw_http.received = String.concat "" responses; ending}

let none = {Outcome.underscore_names = false}

let () =
  print_endline "== Outcome.of_outcome";
  List.iter
    (fun outcome -> print_string (Outcome.to_string outcome))
    [ outcome [response 200 (echo ()); response 400 ""]
    ; outcome [response 200 "not json"]
    ; outcome ~ending:Closed
        ["HTTP/1.1 100 Continue\r\n\r\n"; response 200 (echo ())]
    ; outcome ~ending:Closed ["HTTP/1.1 200 OK\r\n\r\n" ^ echo ()]
    ; outcome ["garbage"]
    ]

let () =
  print_endline "== Outcome.clusters";
  let accepted = outcome [response 200 (echo ())] in
  let same_but_framing =
    outcome
      [response 200 (echo ~headers:["host", "a"; "Content-Length", "0"] ())]
  in
  let rejected = outcome [response 400 ""] in
  let closed = outcome ~ending:Closed [] in
  let accepted_then_rejected =
    outcome [response 200 (echo ()); response 400 ""]
  in
  let other = outcome [response 200 (echo ~target:"/b" ())] in
  List.iter
    (fun (names, view) ->
      Printf.printf "%s: %s\n" (String.concat ", " names)
        (Outcome.view_to_string view)
    )
    (Outcome.clusters
       (List.map
          (fun (name, outcome) -> name, none, outcome)
          [ "accepted", accepted
          ; "rejected", rejected
          ; "same-but-framing", same_but_framing
          ; "closed", closed
          ; "accepted-then-rejected", accepted_then_rejected
          ; "other", other
          ]
       )
    )

let () =
  print_endline "== Outcome.view, normalization";
  let show ?(underscore_names = false) request =
    print_endline
      (Outcome.view_to_string
         (Outcome.view ~quirks:{underscore_names}
            (outcome [response 200 request])
         )
      )
  in
  show (echo ~headers:["Host", "a"; "X-A", "1"; "B", "2"; "x-a", "3"] ());
  show ~underscore_names:true
    (echo ~headers:["Host", "a"; "X_A", "1"; "Content_Length", "0"] ());
  show (echo ~target:"http://b:8080/c?d" ());
  show (echo ~target:"http://b?d" ~headers:[] ());
  show (echo ~target:"ftp://b" ());
  show (echo ~target:"/a://b" ());
  show (echo ~headers:["Host", "a"; "X-A", "1, 2"; "X-A", " 3"] ());
  print_endline
    (Outcome.view_to_string
       (Outcome.view ~ignored:["Content-Type"]
          (outcome
             [response 200 (echo ~headers:["Host", "a"; "content-type", ""] ())]
          )
       )
    )
