
# API reference

API reference for Ocsigen Server. See the [manual](./index.md) to get started.

[`Ocsigen.Server`](./Ocsigen-Server.md) 

### Extensions

[`Staticmod`](./Staticmod.md) Staticmod: serve static files
[`Extendconfiguration`](./Extendconfiguration.md) Extendconfiguration: More configuration options for Ocsigen Server
[`Accesscontrol`](./Accesscontrol.md) Accesscontrol: Conditional access to some sites
[`Authbasic`](./Authbasic.md) Authbasic: Basic HTTP authentication
[`Deflatemod`](./Deflatemod.md) Deflatemod: compress output data
[`Redirectmod`](./Redirectmod.md) Redirectmod: HTTP redirections
[`Revproxy`](./Revproxy.md) Revproxy: Forward a request to another Web server
[`Rewritemod`](./Rewritemod.md) Rewrite: Change the request
[`Outputfilter`](./Outputfilter.md) Outputfilter: Rewrite some part of the output
[`Userconf`](./Userconf.md) 
[`Cors`](./Cors.md) Cross-Origin Resource Sharing

### Requests and responses

[`Ocsigen.Request`](./Ocsigen-Request.md) 
[`Ocsigen.Response`](./Ocsigen-Response.md) 
[`Ocsigen.Multipart`](./Ocsigen-Multipart.md) 
[`Ocsigen_cookie_map`](./Ocsigen_cookie_map.md) 
[`Ocsigen_http.Charset_mime`](./Ocsigen_http-Charset_mime.md) 

### Persistent data, writing in the logs, configuration file extension, polymorphic tables

[`Ocsigen_base.Lib`](./Ocsigen_base-Lib.md) This module contains some auxiliaries for the Ocsigenserver. In contrast to Ocsigen\_lib\_base, the function may also refer to libraries other than the standard library.
[`Ocsigen.Messages`](./Ocsigen-Messages.md) Writing messages in the logs
[`Ocsigen.Parseconfig`](./Ocsigen-Parseconfig.md) Config file parsing. See also module Extensions.​Configuration
[`Polytables`](./Polytables.md) Polymorphic tables (using Map)
[`Ocsigen_base.Cache`](./Ocsigen_base-Cache.md) Cache. Association tables (from any kind of database) that keep the most recently used values in memory. It is also possible to set a maximum lifetime for data in the cache.
[`Ocsigen.Config`](./Ocsigen-Config.md) Configuring Ocsigen server

### Extending Ocsigen Server

[`Ocsigen.Extensions`](./Ocsigen-Extensions.md) Extensions interface for Ocsigen Server
[`Ocsigen.Local_files`](./Ocsigen-Local_files.md) 
[`Ocsigen_http.Header`](./Ocsigen_http-Header.md) 
[`Ocsigen_base.Ocsigen_stream`](./Ocsigen_base-Ocsigen_stream.md) 
[`Ocsigen_base.Loader`](./Ocsigen_base-Loader.md) Module Loader: Dynamic loading for Ocsigen.
[`Ocsigen.Command`](./Ocsigen-Command.md) Extending server commands