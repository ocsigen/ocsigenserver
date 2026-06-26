
# Migrating to Ocsigen Server 8\.0

Ocsigen Server 8\.0 reorganises its public modules under hierarchical names and modernises its base library. These changes break compatibility with earlier versions. This page explains what changed and how to update existing code, either by switching to the new names or by relying on the `ocsigenserver-compat` package during the transition.


## Hierarchical module names

The historical flat `Ocsigen_xxx` modules are now grouped under three namespaces: `Ocsigen` for the server, `Ocsigen_base` for the base library and `Ocsigen_http` for the HTTP layer.


### Server modules (`Ocsigen.Xxx`)

- `Ocsigen_server` becomes [`Ocsigen.Server`](./Ocsigen-Server.md)
- `Ocsigen_config` becomes [`Ocsigen.Config`](./Ocsigen-Config.md)
- `Ocsigen_extensions` becomes [`Ocsigen.Extensions`](./Ocsigen-Extensions.md)
- `Ocsigen_request` becomes [`Ocsigen.Request`](./Ocsigen-Request.md)
- `Ocsigen_response` becomes [`Ocsigen.Response`](./Ocsigen-Response.md)
- `Ocsigen_multipart` becomes [`Ocsigen.Multipart`](./Ocsigen-Multipart.md)
- `Ocsigen_messages` becomes [`Ocsigen.Messages`](./Ocsigen-Messages.md)
- `Ocsigen_local_files` becomes [`Ocsigen.Local_files`](./Ocsigen-Local_files.md)
- `Ocsigen_command` becomes [`Ocsigen.Command`](./Ocsigen-Command.md)
- `Ocsigen_parseconfig` becomes [`Ocsigen.Parseconfig`](./Ocsigen-Parseconfig.md)
- `Ocsigen_cohttp` becomes [`Ocsigen.Ocsigen_cohttp`](./Ocsigen-Ocsigen_cohttp.md)

### Base library modules (`Ocsigen_base.Xxx`)

- `Ocsigen_lib` becomes [`Ocsigen_base.Lib`](./Ocsigen_base-Lib.md)
- `Ocsigen_cache` becomes [`Ocsigen_base.Cache`](./Ocsigen_base-Cache.md)
- `Ocsigen_loader` becomes [`Ocsigen_base.Loader`](./Ocsigen_base-Loader.md)
- `Ocsigen_stream` becomes [`Ocsigen_base.Ocsigen_stream`](./Ocsigen_base-Ocsigen_stream.md)
- `Ocsigen_config_static` becomes [`Ocsigen_base.Config_static`](./Ocsigen_base-Config_static.md)

### HTTP modules (`Ocsigen_http.Xxx`)

- `Ocsigen_header` becomes [`Ocsigen_http.Header`](./Ocsigen_http-Header.md)
- `Ocsigen_charset_mime` becomes [`Ocsigen_http.Charset_mime`](./Ocsigen_http-Charset_mime.md)

### Modules that keep their name

A few public modules are unchanged and need no update: `Ocsigen_lib_base`, `Polytables`, `Ocsigen_cookie_map` and the main `Ocsigenserver` module.


## The ocsigenserver-compat package

To make the transition smooth, the `ocsigenserver-compat` package provides every old `Ocsigen_xxx` name as an alias to its new module. Existing code keeps compiling without changes: just add the package to your `dune` file.

The shims are split to mirror the libraries they alias:

- `ocsigenserver-compat`: old server names (`Ocsigen_server`, `Ocsigen_extensions`, `Ocsigen_config`, ...)
- `ocsigenserver-compat.baselib`: old base-library names (`Ocsigen_lib`, `Ocsigen_cache`, ...)
- `ocsigenserver-compat.http`: old HTTP names (`Ocsigen_header`, `Ocsigen_charset_mime`)
For example, to keep using the old names in an executable:

```
(executable
 (public_name myserver)
 (name main)
 (libraries
  ocsigenserver
  ocsigenserver-compat
  ocsigenserver-compat.baselib
  ocsigenserver-compat.http))
```
The compat package is meant as a temporary migration aid: it is recommended to update your code to the new module names and drop the dependency once done. The compat modules are intentionally excluded from the [API reference](./api.md), which documents the new names only.


## Other breaking changes


### Minimum OCaml version

Ocsigen Server 8\.0 requires OCaml 4\.14 or later.


### Removed deprecated helpers from the base library

Several helpers that duplicated standard-library features have been removed from `Ocsigen_lib` (now [`Ocsigen_base.Lib`](./Ocsigen_base-Lib.md)). Use the standard library instead:

- `Ocsigen_lib.id` is removed; use `Sys.opaque_identity`
- `Ocsigen_lib.leftright` is removed; use `Either.t`
- `Ocsigen_lib.List.map_filter` is removed; use `List.filter_map`
- the `Ocsigen_lib.Option` module is removed; use the standard library `Option`
These helpers have no replacement in `ocsigenserver-compat`: their standard equivalents should be used directly.
