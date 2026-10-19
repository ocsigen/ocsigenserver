(* Tests of Ocsigen_http.Http1. *)

let version_name = function
  | `HTTP_1_0 -> "HTTP/1.0"
  | `HTTP_1_1 -> "HTTP/1.1"
  | `Other v -> v

let fields_text = function
  | [] -> "(no field)"
  | fields ->
      String.concat " | "
        (List.map (fun (name, value) -> name ^ ": " ^ value) fields)

let show_host ?(target = "/") version fields =
  Printf.printf "%s %s, %s: %s\n" (version_name version) target
    (fields_text fields)
    (match
       Ocsigen_http.Http1.check_host version
         (Http.Header.of_list fields)
         ~target
     with
    | Ok () -> "ok"
    | Error reason -> reason)

let () =
  print_endline "== check_host";
  show_host `HTTP_1_1 [];
  show_host `HTTP_1_0 [];
  show_host `HTTP_1_1 ["Host", "example.org"];
  show_host `HTTP_1_1 ["Host", "example.org:8080"];
  show_host `HTTP_1_1 ["Host", "example.org:"];
  show_host `HTTP_1_1 ["Host", ""];
  show_host `HTTP_1_1 ["Host", "192.0.2.1:80"];
  show_host `HTTP_1_1 ["Host", "[2001:db8::1]:80"];
  show_host `HTTP_1_1 ["Host", "[::1]"];
  show_host `HTTP_1_1 ["Host", "example.org:65535"];
  show_host `HTTP_1_1 ["Host", "example.org:00080"];
  show_host `HTTP_1_1 ["Host", "my_host-1.example.org"];
  show_host `HTTP_1_1 ["Host", ":80"];
  show_host `HTTP_1_1 ["Host", "a"; "Host", "a"];
  show_host `HTTP_1_0 ["Host", "a"; "Host", "b"];
  print_endline "-- invalid values";
  show_host `HTTP_1_1 ["Host", "ex%41mple.org"];
  show_host `HTTP_1_1 ["Host", "admin.test%0Aevil"];
  show_host `HTTP_1_1 ["Host", "[v1.x]"];
  show_host `HTTP_1_1 ["Host", "[admin.test]"];
  show_host `HTTP_1_1 ["Host", "[192.0.2.1]"];
  show_host `HTTP_1_1 ["Host", "user@example.org"];
  show_host `HTTP_1_1 ["Host", "example.org/x"];
  show_host `HTTP_1_1 ["Host", "example.org:80:80"];
  show_host `HTTP_1_1 ["Host", "example.org:123456"];
  show_host `HTTP_1_1 ["Host", "example.org:65536"];
  show_host `HTTP_1_1 ["Host", "example.org:99999"];
  show_host `HTTP_1_1 ["Host", "evil,x.admin.test"];
  List.iter
    (fun c -> show_host `HTTP_1_1 ["Host", Printf.sprintf "a%cb" c])
    ['~'; '!'; '$'; '&'; '\''; '('; ')'; '*'; '+'; ';'; '='];
  show_host `HTTP_1_1 ["Host", "b\xc3\xbccher.example"];
  show_host `HTTP_1_1 ["Host", "example.org:x"];
  show_host `HTTP_1_1 ["Host", "a b"];
  show_host `HTTP_1_1 ["Host", "[]"];
  show_host `HTTP_1_1 ["Host", "[::1"];
  show_host `HTTP_1_1 ["Host", "[::1]x"];
  print_endline "-- request targets";
  let host = ["Host", "x"] in
  show_host `HTTP_1_1 host ~target:"*";
  show_host `HTTP_1_1 host ~target:"http://example.org/a?b";
  show_host `HTTP_1_1 host ~target:"HTTPS://example.org:8443";
  show_host `HTTP_1_1 host ~target:"http://[::1]:8080/";
  show_host `HTTP_1_1 host ~target:"http://%61dmin.test/";
  show_host `HTTP_1_1 host ~target:"http://user@example.org/";
  show_host `HTTP_1_1 host ~target:"http://evil,x.admin.test/";
  show_host `HTTP_1_1 host ~target:"http:///a";
  show_host `HTTP_1_1 host ~target:"http://:80/";
  show_host `HTTP_1_1 host ~target:"ftp://example.org/";
  show_host `HTTP_1_1 host ~target:"example.org:443";
  show_host `HTTP_1_1 [] ~target:"http://example.org/"

let show_field_values fields =
  Printf.printf "%S: %b\n" (fields_text fields)
    (Ocsigen_http.Http1.valid_field_values (Http.Header.of_list fields))

let () =
  print_endline "== valid_field_values";
  show_field_values [];
  show_field_values ["X-A", "a b\tc\xe9~"];
  show_field_values ["X-A", "ok"; "X-B", "a\000b"];
  show_field_values ["X-A", "a\rb"];
  show_field_values ["X-A", "a\nb"];
  show_field_values ["X-A", "a\x1bb"];
  show_field_values ["X-A", "a\x7fb"]

let show_method meth =
  Printf.printf "%S: %s\n"
    (Http.Method.to_string meth)
    (match Ocsigen_http.Http1.request_method meth with
    | Some meth -> Printf.sprintf "%S" (Http.Method.to_string meth)
    | None -> "invalid")

let () =
  print_endline "== request_method";
  show_method `GET;
  show_method (`Other "PROPFIND");
  show_method (`Other "get");
  show_method (`Other "\r\n\r\nGET");
  show_method (`Other "\nPOST");
  show_method (`Other "\rGET");
  show_method (`Other "\n\rGET");
  show_method (`Other "\r\r\nGET");
  show_method (`Other "\r\nFROB");
  show_method (`Other "\r\n");
  show_method (`Other "G(ET");
  show_method (`Other "G\001ET");
  show_method (`Other "GET\r\n")
