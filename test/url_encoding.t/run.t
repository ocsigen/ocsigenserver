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
