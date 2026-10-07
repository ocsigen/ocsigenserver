string_of_stream collects a string stream into one string and fails with
String_too_large when the total length exceeds the limit.

  $ dune exec ./test.exe 2>&1
  limit 0       [] -> ""
  limit 3       ["abc"] -> "abc"
  limit 2       ["abc"] -> String_too_large
  limit 6       ["ab"; "cd"; "ef"] -> "abcdef"
  limit 5       ["ab"; "cd"; "ef"] -> String_too_large
  limit max_int ["ab"; "cd"; "ef"] -> "abcdef"
