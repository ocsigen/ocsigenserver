The CORS extension.

Cors.run refuses a negative max_age, as the configuration file does:

  $ ../cors/test.exe
  Cors.run ~max_age:(-1) () -> Invalid_argument: Cors.run: negative max_age -1
  Cors.run ~max_age:0 () -> accepted
  Cors.run () -> accepted
