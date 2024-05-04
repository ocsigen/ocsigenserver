val run :
   ?site:Ocsigen_server.Site.t
  -> ?credentials:bool
  -> ?max_age:int
  -> ?exposed_headers:string list
  -> ?methods:Cohttp.Code.meth list
  -> unit
  -> unit
