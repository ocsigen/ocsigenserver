Percent-encoding helpers of Ocsigen_base.Lib.Url.

  $ dune exec ./test.exe 2>&1
  decode ~plus:false:
  unreserved               -> unreserved
  caf%C3%A9                -> café
  a%20b                    -> a b
  a+b                      -> a+b
  %41%42%43                -> ABC
  %2e%2e%2fetc%2fpasswd    -> ../etc/passwd
  100%                     -> Failure "decode"
  a%2                      -> Failure "decode"
  %zz                      -> Failure "decode"
  
  decode ~plus:true:
  a+b                      -> a b
  a%2Bb                    -> a+b
  
  encode ~plus:false:
  unreserved               -> unreserved
  café                    -> caf%C3%A9
  a b                      -> a%20b
  a/b                      -> a%2Fb
  
  encode ~plus:true:
  a b                      -> a+b
  a+b                      -> a%2Bb
  
  split_decoded_path:
                                   -> []
  foo                              -> ["foo"]
  foo/                             -> ["foo"; ""]
  foo/bar/baz                      -> ["foo"; "bar"; "baz"]
  a//b                             -> ["a"; ""; "b"]
  caf%C3%A9/a%20b                  -> ["caf\195\169"; "a b"]
  ../../etc/passwd                 -> ["etc"; "passwd"]
  a/../b                           -> ["a"; "b"]
  %2e%2e%2f%2e%2e%2fetc%2fpasswd   -> ["etc"; "passwd"]
  ..%2f..%2fetc%2fpasswd           -> ["etc"; "passwd"]
  a%2Fb                            -> ["a"; "b"]
  100%/x                           -> ["100%"; "x"]
