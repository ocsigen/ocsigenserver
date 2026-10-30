Ocsigen_http.Header.add_vary adds Origin to the Vary lines on the left. It
reads every Vary line, and writes a single one when it adds a name.

  $ ../vary/test.exe
  (none)                       -> origin
  Cookie                       -> Cookie, origin
  Cookie, Accept-Encoding      -> Cookie, Accept-Encoding, origin
  Cookie | Accept-Encoding     -> Cookie, Accept-Encoding, origin
  Cookie, ,Accept-Encoding,    -> Cookie, Accept-Encoding, origin
  Cookie | Origin              -> Cookie | Origin
  Cookie | ORIGIN              -> Cookie | ORIGIN
  Cookie, *                    -> Cookie, *
  Cookie |   *                 -> Cookie |   *
