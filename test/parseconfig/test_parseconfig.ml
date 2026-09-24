(* Unit tests for the size-valued configuration tags parsed by
   {!Ocsigen.Parseconfig}:

   - the SI and binary unit parsing shared by [<maxrequestbodysize>],
     [<maxuploadfilesize>] and [<maxrequestbodysizeinmemory>]
     ([Ocsigen.Parseconfig.parse_size_tag]);
   - the wiring of each of these tags to its [Ocsigen.Config] setter, which is
     what a missing tag branch would break. *)

let failures = ref 0

let check name cond =
  if not cond
  then (
    incr failures;
    Printf.printf "FAIL: %s\n" name)

let parse s = Ocsigen.Parseconfig.parse_size_tag "test" s

(* [parse_size_tag]: units and special values. Bare single-letter suffixes
   (k, M, G, T) are binary; two-letter suffixes (kB/ko, MB/Mo, ...) are SI;
   three-letter suffixes (kiB/kio, MiB, ...) are binary. *)
let () =
  check "no unit is bytes" (parse "8192" = Some 8192L);
  check "explicit B suffix" (parse "8192B" = Some 8192L);
  check "explicit o suffix" (parse "8192o" = Some 8192L);
  check "kB is 1000" (parse "10kB" = Some 10_000L);
  check "ko is 1000" (parse "10ko" = Some 10_000L);
  check "kiB is 1024" (parse "10kiB" = Some 10_240L);
  check "kio is 1024" (parse "10kio" = Some 10_240L);
  check "MB is 10^6" (parse "2MB" = Some 2_000_000L);
  check "MiB is 2^20" (parse "2MiB" = Some 2_097_152L);
  check "GB is 10^9" (parse "1GB" = Some 1_000_000_000L);
  check "TB is 10^12" (parse "1TB" = Some 1_000_000_000_000L);
  check "bare k is 1024" (parse "1k" = Some 1024L);
  check "bare M is 2^20" (parse "1M" = Some 1_048_576L);
  check "infinity is None" (parse "infinity" = None);
  check "empty is None" (parse "" = None)

(* An unparsable size is reported as a configuration error. *)
let () =
  let raised =
    try
      ignore (parse "not-a-size");
      false
    with Ocsigen.Config.Config_file_error _ -> true
  in
  check "invalid size raises Config_file_error" raised

(* Default in-memory cap, before any tag sets it. *)
let () =
  check "<maxrequestbodysizeinmemory> defaults to 1 MiB"
    (Ocsigen.Config.get_maxrequestbodysizeinmemory () = 1_048_576)

(* Wiring: feeding a single tag through [later_pass] must update the matching
   [Config] value. This is exactly what fails when a tag branch is missing. *)
let set_tag tag value =
  Ocsigen.Parseconfig.later_pass [Xml.Element (tag, [], [Xml.PCData value])]

let () =
  set_tag "maxrequestbodysize" "2MB";
  check "wire <maxrequestbodysize>"
    (Ocsigen.Config.get_maxrequestbodysize () = Some 2_000_000L);
  set_tag "maxuploadfilesize" "3MB";
  check "wire <maxuploadfilesize>"
    (Ocsigen.Config.get_maxuploadfilesize () = Some 3_000_000L);
  set_tag "maxrequestbodysizeinmemory" "1MB";
  check "wire <maxrequestbodysizeinmemory>"
    (Ocsigen.Config.get_maxrequestbodysizeinmemory () = 1_000_000);
  (* "infinity" on the in-memory cap means no limit, i.e. [max_int]. *)
  set_tag "maxrequestbodysizeinmemory" "infinity";
  check "<maxrequestbodysizeinmemory> infinity is max_int"
    (Ocsigen.Config.get_maxrequestbodysizeinmemory () = max_int)

let () =
  if !failures > 0
  then exit 1
  else print_string "All config size-tag tests passed\n"
