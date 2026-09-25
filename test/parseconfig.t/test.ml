(* Tests for the size-valued configuration tags parsed by
   [Ocsigen.Parseconfig]:

   - the SI and binary units accepted by [parse_size_tag], shared by
     [<maxrequestbodysize>], [<maxuploadfilesize>] and
     [<maxrequestbodysizeinmemory>];
   - the wiring of each of these tags to its [Ocsigen.Config] setter, which is
     what a missing tag branch would break;
   - the warning for [<servertimeout>], which is accepted but has no effect. *)

let show_int n = if n = max_int then "max_int" else string_of_int n

let show_int64_option = function
  | None -> "None"
  | Some n -> Int64.to_string n

let show_size s =
  let result =
    match Ocsigen.Parseconfig.parse_size_tag "test" s with
    | n -> show_int64_option n
    | exception Ocsigen.Config.Config_file_error _ -> "Config_file_error"
  in
  Printf.printf "%-12s -> %s\n" (Printf.sprintf "%S" s) result

(* Bare single-letter suffixes (k, M, G, T) are binary; two-letter suffixes
   (kB/ko, MB/Mo, ...) are SI; three-letter suffixes (kiB/kio, MiB, ...) are
   binary. *)
let () =
  print_endline "parse_size_tag:";
  List.iter show_size
    [ "8192"
    ; "8192B"
    ; "8192o"
    ; "10kB"
    ; "10ko"
    ; "10kiB"
    ; "10kio"
    ; "2MB"
    ; "2MiB"
    ; "1GB"
    ; "1TB"
    ; "1k"
    ; "1M"
    ; "infinity"
    ; ""
    ; "not-a-size" ]

(* Feed a single tag through [later_pass], as if it were a child of
   [<server>] in the configuration file, then read the resulting value. *)
let show_tag tag value get =
  Ocsigen.Parseconfig.later_pass [Xml.Element (tag, [], [Xml.PCData value])];
  Printf.printf "<%s>%s -> %s\n" tag value (get ())

let () =
  let in_memory () =
    show_int (Ocsigen.Config.get_maxrequestbodysizeinmemory ())
  and body_size () =
    show_int64_option (Ocsigen.Config.get_maxrequestbodysize ())
  and upload_size () =
    show_int64_option (Ocsigen.Config.get_maxuploadfilesize ())
  in
  print_newline ();
  Printf.printf "default in-memory request body size -> %s\n" (in_memory ());
  print_newline ();
  print_endline "tags:";
  show_tag "maxrequestbodysize" "2MB" body_size;
  show_tag "maxrequestbodysize" "infinity" body_size;
  show_tag "maxuploadfilesize" "3MB" upload_size;
  show_tag "maxrequestbodysizeinmemory" "1MB" in_memory;
  show_tag "maxrequestbodysizeinmemory" "infinity" in_memory

(* A tag that is accepted but has no effect says so with a warning, printed
   here on the standard output. *)
let () =
  Logs.set_reporter
    { Logs.report =
        (fun _src level ~over k msgf ->
          msgf (fun ?header:_ ?tags:_ fmt ->
            Format.kasprintf
              (fun message ->
                 Printf.printf "%s: %s\n"
                   (Logs.level_to_string (Some level))
                   message;
                 over ();
                 k ())
              fmt)) };
  print_newline ();
  print_endline "tags without effect:";
  Ocsigen.Parseconfig.later_pass
    [Xml.Element ("servertimeout", [], [Xml.PCData "20"])]
