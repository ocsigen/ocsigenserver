val run :
   ?site:Ocsigen_server.Site.t
  -> mode:
       [ `Rewrite of Ocsigen_header.Name.t * Re.Pcre.regexp * string
       | `Add of Ocsigen_header.Name.t * string * bool option
       | `Code of Cohttp.Code.status ]
  -> unit
  -> unit
