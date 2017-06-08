val credentials : bool Ocsigen_server.Site.Config.key
val max_age : int Ocsigen_server.Site.Config.key
val exposed_headers : string list Ocsigen_server.Site.Config.key
val methods : Cohttp.Code.meth list Ocsigen_server.Site.Config.key

val extension : Ocsigen_server.Site.extension
