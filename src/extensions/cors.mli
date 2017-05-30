val credentials : bool Ocsigen_server.Vhost.Config.key
val max_age : int Ocsigen_server.Vhost.Config.key
val exposed_headers : string list Ocsigen_server.Vhost.Config.key
val methods : Cohttp.Code.meth list Ocsigen_server.Vhost.Config.key

val register : Ocsigen_server.Vhost.t -> unit
