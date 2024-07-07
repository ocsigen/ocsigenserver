val run :
   mode:
     [ `Rewrite of Ocsigen_header.Name.t * Re.Pcre.regexp * string
     | `Add of Ocsigen_header.Name.t * string * bool option
     | `Code of Cohttp.Code.status ]
  -> unit
  -> Ocsigen_server.instruction
