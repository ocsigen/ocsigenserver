let () =
  Ocsigen_server.start
    ~config:(Ocsigen_parseconfig.parse_config ())
    ()
