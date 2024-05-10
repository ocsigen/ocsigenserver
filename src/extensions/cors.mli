val run :
   ?credentials:bool
  -> ?max_age:int
  -> ?exposed_headers:string list
  -> ?methods:Cohttp.Code.meth list
  -> unit
  -> Ocsigen_server.instruction
