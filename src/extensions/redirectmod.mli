type redirection

val create_redirection :
  ?full : [ `Maybe | `No | `Yes ] ->
  ?temporary : bool ->
  regexp : string ->
  string ->
  redirection

val redirection : redirection Ocsigen_server.Vhost.Config.key

val register : Ocsigen_server.Vhost.t -> unit
