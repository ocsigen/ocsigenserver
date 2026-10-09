let fail fmt = Printf.ksprintf (fun s -> prerr_endline s; exit 1) fmt
let log_files = ["access.log"; "warnings.log"; "errors.log"]

let open_log_dir () =
  let dir = Filename.temp_file "ocsigenserver-logs" "" in
  Sys.remove dir;
  Sys.mkdir dir 0o700;
  Ocsigen.Config.set_logdir dir;
  Ocsigen.Config.set_silent ();
  Lwt_main.run (Ocsigen.Messages.open_files ());
  dir

let read dir file =
  In_channel.with_open_text (Filename.concat dir file) In_channel.input_all

let contains s sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s && (String.sub s i n = sub || go (i + 1))
  in
  go 0

let check_file dir file ~present ~absent =
  let contents = read dir file in
  List.iter
    (fun msg -> if not (contains contents msg) then fail "%s lacks %S" file msg)
    present;
  List.iter
    (fun msg -> if contains contents msg then fail "%s contains %S" file msg)
    absent

let remove_log_dir dir =
  List.iter (fun file -> Sys.remove (Filename.concat dir file)) log_files;
  Sys.rmdir dir
