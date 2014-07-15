let () =
  Cgimod.init ~timeout:5 ();
  Deflatemod.init ~level:5 ~size:1024 ();
  Ocsigen_server.start_server ()
