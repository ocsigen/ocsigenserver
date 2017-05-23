let () =
  Ocsigen_extensions.set_we_have_xml_config ();
  Ocsigen_server.start
    ~config:(Ocsigen_parseconfig.parse_config ())
    ()
