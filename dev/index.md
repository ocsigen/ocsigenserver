
# Ocsigen Server

Ocsigen Server is a powerful and modular generic purpose Web Server. It supports static files, redirections, reverse proxy, CORS, user pages with local configuration, page compression, authentication, etc.

Ocsigen Server can be used:

- as an executable: it reads its configuration from a file and dynamically loads the extensions and libraries you need for your sites,
- or as a library: build your own executable and statically link your the modules you need for your pages.
It is written in OCaml and uses the [Cohttp](https://github.com/mirage/ocaml-cohttp) library.


## Installation

The easier way to install Ocsigenserver is to use the `opam` package manager. Ocsigen Server is also available in Linux distributions.


## Quick start: serving a directory in one command

To serve the static files of a directory without writing any configuration file, just give the directory to `ocsigenserver`:

```
ocsigenserver ./public
```
This starts a server on port 8080 serving the files of `./public`. No configuration file and no log directory are needed: the logs go to the terminal. A few options are available:

```
ocsigenserver --serve ./public --port 8000 --directory-listing
```
You can also start a reverse proxy that forwards every request to another server, again without any configuration file:

```
ocsigenserver --reverse-proxy http://localhost:9000 --port 8080
```
This is the simplest way to get started. For anything more elaborate (several hosts, extensions, fine-grained reverse proxy rules, etc.) use a configuration file or the library API, as described below.


## Using Ocsigen Server as an executable

Create your configuration file. You can take this one as an example:

```
<ocsigen>
  <server>
    <port>8080</port>

    <logdir>local/var/log/mysite</logdir>
    <datadir>local/var/data/mysite</datadir>
    <charset>utf-8</charset>

    <commandpipe>local/var/run/mysite-cmd</commandpipe>
    <extension findlib-package="ocsigenserver.ext.staticmod"/>
    <host hostfilter="*">
      <static dir="local/var/www/mysite/" />
    </host>
  </server>
</ocsigen>
```
Update the paths to match your configuration and create the missing directories. Then run:

```
ocsigenserver -c <name of your configuration file>
```

### Personalising your configuration

Have a look at the page [Configuration](./config.md) to learn how to personalise the configuration file and adapt it to your needs.


## Using Ocsigen Server as a library

If you want to include a Web Server in your OCaml app, add library `ocsigenserver` and the extensions you need (here `ocsigenserver.ext.staticmod`) in your Dune file:

```
(executable
 (public_name myapp)
 (name main)
 (libraries
  ocsigenserver
  ocsigenserver.ext.staticmod))
```
Call function [`Ocsigen.Server.start`](./Ocsigen-Server.md#val-start) like this to run the server:

```ocaml
let _ =
  Ocsigen.Server.start
    [ Ocsigen.Server.host [Staticmod.run ~dir:"local/var/www/mysite/" ()]]
```
The server runs on port 8080 by default. Customise this using optional parameters of function [`Ocsigen.Server.start`](./Ocsigen-Server.md#val-start)


## Extensions

Depending on the features you need, you can extend the server by loading some modules.

The extensions provided are:

- [Staticmod](./staticmod.md): for serving static pages,
- [Accesscontrol](./accesscontrol.md): if you need to restrict the access to the sites,
- [Eliom](./eliom.md): if you want to use dynamic Web and mobile applications written in OCaml with Eliom,
- [Revproxy](./revproxy.md): if you want to redirect some requests to another Web server,.
- [Redirectmod](./redirectmod.md): if you want to define some HTTP redirections,
- [Deflatemod](./deflatemod.md): if you want to compress the content before sending your pages,
- [Outputfilter](./outputfilter.md): allows to change the header of the HTTP response,
- [Securityheaders](./securityheaders.md): adds common security-related HTTP headers (HSTS, nosniff, X-Frame-Options, CSP),
- [Rewritemod](./rewritemod.md): allows to rewrite the request before continuing,
- [Extendconfiguration](./extendconfiguration.md): adds many useful configuration options,
- [Authbasic](./authbasic.md): for basic HTTP authentication,
- [CORS](./cors.md): for settings CORS options,
- [Userconf](./userconf.md): for allowing local users to have personal pages,
If you are using a configuration, file, use tag `<extension/>` to load an exetnsion (see examples in the manual of each extension). Most extension have configuration options, and define new tags for the configuration file.

If you are building a static executable, load the extensions as any library from your Dune file. Have a look at the API documentation of each documentation to see the functions it defines and the configuration options.


## API reference

See the [API reference](./api.md) for all the modules, functions and configuration options provided by Ocsigen Server and its extensions.


## Upgrading from an earlier version

Ocsigen Server 8\.0 renamed its modules and modernised its base library. See [Migrating to Ocsigen Server 8\.0](./migration.md) for the details and for the `ocsigenserver-compat` package that eases the transition.


## Running on port 80 or 443

It is not recommended to run a Web server as root. If you want to use priviledged ports (port numbers less than 1024\), like 80 or 443, you can add the capability to ocsigenserver with:

```
setcap 'cap_net_bind_service=+ep' <path to the server executable>
```

## If something goes wrong

If something goes wrong, first have a look in the logs to see if there is an error message. The directory of logs is set in the configuration file or through option `?logdir` of function [`Ocsigen.Server.start`](./Ocsigen-Server.md#val-start) (`$OPAM_SWITCH_PREFIX/lib/ocsigenserver/var/log/ocsigenserver` by default).

If you cannot solve your problem, if you find a bug, a mistake in the documentation (even a small one), or if you want to make a suggestion, [open an issue on Github](https://github.com/ocsigen/ocsigenserver).
