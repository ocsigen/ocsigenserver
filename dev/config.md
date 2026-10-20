# Configuration file

The main configuration file is usually named `/etc/ocsigenserver/ocsigenserver.conf`. It contains the port(s) on which you want to run the server (usually 80\), the protocol to use (HTTP or HTTPS), the location of log files, many settings for the server, the extensions to be loaded, the OCaml libraries you need for your Web sites, the configuration of each Web site, etc. One default configuration file should be provided by your distribution.

### Basic layout of the configuration file

The configuration file is an XML file. Its layout is the following:

```
<ocsigen>

  <server>

    <!-- General setting -->
    <port>80</port>

    <logdir>...</logdir>
    <commandpipe>...</commandpipe>

    <!-- Extensions to be loaded: -->
    <extension module=.../>
    <extension module=...>
      <!-- Options for the extension -->
      ...
    </extension>

    <!-- You can also use Findlib to load an extension and its dependencies: -->
    <findlib path=.../>
    <extension findlib-package=.../>

    <!-- Libraries needed by Web sites: -->
    <require module=.../> <!-- <require is equivalent to <extension> -->
    <require findlib-package=.../>
    <require module=...>
      <!-- Options for the library -->
      ...
    </require>

    <!-- <library> is equivalent to <extension> too: -->
    <library module=.../>
    <library findlib-package=.../>
    <library module=...>
      <!-- Options for the library -->
      ...
    </library>

    <!-- Inclusion of all external configuration files matching *.conf
     from this directory (in alphabetical order): -->
    <extconf dir=... />

    <!-- Virtual hosts configuration: -->
    <host defaulthostname=... hostfilter=...>

      <!-- configuration for the site at the root -->
      ...

      <site path=...>
        <!-- configuration for the first sub-site -->
        <!-- Warning: it was <site dir=...> before 0.99.4 -->
        ...
      </site>

      <site path=...>
        <!-- configuration for another sub-site -->
        ...
      </site>
    </host>

    <host defaulthostname=... hostfilter=...>
      <!-- configuration for the second virtual host -->
      ...
    </host>

    ...

  </server>

</ocsigen>
```
Here is a simple example:

```
<ocsigen>
  <server>

    <port>127.0.0.1:80</port>

    <logdir>/var/log/ocsigenserver</logdir>
    <commandpipe>/var/run/ocsigenserver/command</commandpipe>

    <charset>utf-8</charset>

    <extension findlib-package="ocsigenserver.ext.staticmod"/>

    <extension findlib-package="ocsipersist-sqlite-config">
      <database file="/var/lib/ocsigenserver/ocsidb"/>
    </extension>
    <!-- Its dependencies will be loaded automatically using findlib -->

    <extension findlib-package="eliom.server"/>
    <!-- Eliom's dependencies will be loaded automatically using findlib -->

    <extconf dir="/etc/ocsigenserver/conf.d" />

    <host charset="utf-8" hostfilter="www.mywonderfulwebsite.org">

      <static dir="/var/www/ocsigenserver" />

      <site path="tuto">
        <eliom module="/usr/local/lib/ocaml/tutoeliom/tutoeliom.cmo" />
        <static dir="/var/www/tutorial" />
      </site>

    </host>

  </server>

</ocsigen>
```
Here is another example, for use as unprivileged user toto on port 8000\. We use the PostgreSQL (version \>= 9\.5) backend for ocsipersist. Note that the ocsipersist-database under the specified name (default: "ocsipersist") needs to exist. To create it run `psql` and execute `CREATE DATABASE ocsipersist;`. Ocsipersist creates its tables itself.

```
<ocsigen>
  <server>

    <port>8000</port>

    <logdir>/home/toto/var/log/ocsigenserver</logdir>
    <commandpipe>/home/toto/var/run/ocsigenserver_command</commandpipe>

    <charset>utf-8</charset>

    <extension findlib-package="ocsigenserver.ext.staticmod"/>

    <extension findlib-package="ocsipersist-pgsql-config">
      <!-- <database> is optional, and so is each of its attributes:
           host, port, user, password, database, unix_domain_socket_dir
           and size_conn_pool -->
      <database
        port="5432"
        user="toto"
        password="toto's secret"
      />
    </extension>
    <!-- Its dependencies will be loaded automatically using findlib -->

    <extension findlib-package="eliom.server"/>
    <!-- Eliom's dependencies will be loaded automatically using findlib -->

    <extconf dir="/home/toto/etc/ocsigenserver/conf.d" />

    <host>

      <static dir="/home/toto/var/www/ocsigenserver" />

      <site path="tuto">
        <eliom module="/usr/local/lib/ocsigenserver/tutoeliom.cmo" />
        <static dir="/home/toto/var/www/ocsigenserver/tutorial" />
        <!-- Module's path could be relative but static dir must be absolute -->
      </site>

    </host>

  </server>

</ocsigen>
```

### Details about settings

#### `<port>` : port and protocol

The port on which the server is listening. You can have several `<port>`lines if you want to listen on several ports.

`<port>` has one optional attribute called `protocol`. Its value may be either `HTTP` (default) or `HTTPS` if you want to use HTTPS on that port. The default port is 80 for HTTP, and 443 for HTTPS.

You can make the server bind on a specific address:

```
    <port>127.0.0.1:80</port>
```
To make the server listen on all IPv4 interfaces (and only them):

```
    <port>*:80</port>
```
To make the server listen on all IPv6 interfaces (and only them):

```
    <port>[::]:80</port>
```
To make the server listen on all IPv4 and IPv6 interfaces:

```
   <port>80</port>
```
**Warning:** with versions \< 1\.3.1, the two previous variants were equivalent and would make the server listen on all IPv6 interfaces, and on IPv4 interfaces as well depending on the operating system (more specifically, the default value of IPV6\_V6ONLY). This OS-dependent behaviour has been fixed in version 1\.3.1.

You can also make the server bind on a specific IPv6 address (it must be between brackets):

```
    <port>[2001:660:3001:4002::10]:80</port>
```
If you want to use HTTPS, you need to specify a certificate to use, and a private key, as follows:

```
    <ssl>
       <certificate>path_to/cert.pem</certificate>
       <privatekey>path_to/privkey.pem</privatekey>
    </ssl>
```
Use the tools provided by openssl to create a private key. Use at least a 2048-bit RSA key (4096 bits is common today), or an elliptic-curve key:

```
    openssl genrsa -des3 -out privkey.pem 4096
```
To create a master certificate based on this key, to use when signing other certificates:

```
    openssl req -new -x509 -days 1001 -key privkey.pem -out cert.pem
```
If you don't want to be asked for a password at start-up, you can uncrypt the private key (if you consider it is safe ...):

```
    openssl rsa -in privkey.pem -out privkey-unsec.pem
```
The TLS context requires **TLS 1\.2 at minimum** (TLS 1\.0 and 1\.1 are deprecated, RFC 8996\) and prefers the server's cipher order. You can further tune the context inside `<ssl>`:

```
    <ssl>
       <certificate>path_to/cert.pem</certificate>
       <privatekey>path_to/privkey.pem</privatekey>
       <ciphers>ECDHE+AESGCM:ECDHE+CHACHA20</ciphers>
       <dhfile>path_to/dhparams.pem</dhfile>
       <curve>prime256v1</curve>
    </ssl>
```
`<ciphers>` is an OpenSSL cipher string, `<dhfile>` a file of Diffie-Hellman parameters, and `<curve>` a named elliptic curve.

If you serve both HTTP and HTTPS and want to force HTTPS, redirect plain HTTP requests to HTTPS using the [Redirectmod](./redirectmod.md) extension guarded by the `<ssl/>` condition of [Accesscontrol](./accesscontrol.md) (both extensions must be loaded):

```
    <host hostfilter="www.example.org" defaulthttpport="80" defaulthttpsport="443">
      <if>
        <not><ssl/></not>
        <then>
          <redirect suburl="(.*)" dest="https://www.example.org/\1"/>
        </then>
      </if>
    </host>
```
Note: HTTP Strict Transport Security (HSTS) is not enabled automatically. Once you serve only over HTTPS, add a `Strict-Transport-Security` response header. The simplest way is the `hsts` attribute of the [Securityheaders](./securityheaders.md) extension. The [Outputfilter](./outputfilter.md) extension can also add it.

#### `<logdir>` : log files directory

The directory for log files. Usually `/var/log/ocsigenserver`. Ocsigenserver is using three log files: `access.log` where all requests are logged, `errors.log` for error messages, and `warnings.log` for warnings.

Requests are logged in the standard Apache/nginx [Combined Log Format](https://httpd.apache.org/docs/current/logs.html#combined):

```
%h %l %u %t "%r" %>s %b "%{Referer}i" "%{User-agent}i"
```
that is, client address, identity (`-`), authenticated user (set by an authentication extension such as [Authbasic](./authbasic.md), `-` otherwise), request time, request line, response status, response size in bytes (`-` when unknown, for example for a streamed response), then the `Referer` and `User-Agent` headers. This format is understood out of the box by log analysers such as GoAccess, AWStats or Apache's own tools.

If `<logdir>` is omitted (and `<syslog>` is not used), there is no log directory: the logs go to the standard output and standard error, as in the one-command modes. This suits a server run by systemd, or deployed as a statically linked program. The access lines go to the standard output in the very same Combined Log Format, and the silent (`-s`) and daemon (`-d`) modes, which redirect the standard output and error to `/dev/null`, are refused.

Example :

```
<logdir>/var/log/ocsigenserver</logdir>
```

#### `<syslog>` : log to syslog

Specifies which syslog facility to use, instead of logging to files. This option is mutually exclusive with `<logdir>`: the server refuses to start when both are given (an empty `<logdir></logdir>`, which means no log directory, is allowed). The same holds for options `?logdir` and `?syslog_facility` of [`Ocsigen.Server.start`](./Ocsigen-Server.md#val-start).

Example:

```
<syslog>Local7</syslog>
```

#### `<loglevel>` : level of a log source

Sets the level of a [Logs](https://erratique.ch/software/logs) source, by its name: the messages of that source below this level are not logged. The levels are `debug`, `info`, `notice`, `warning`, `error` and `fatal` (the same as `error`). The tag may appear several times, once per source, anywhere among the children of `<server>`.

It is meant for the sources of extensions and applications, a debug trace in particular. The command-line options `-v`, `-vv` and `-vvv` set the level of Ocsigen Server's own sources and of the sources created after the command line is read, but not of the other sources of the libraries the server is linked with; `<loglevel>` sets the level of any source, and takes precedence.

The levels are set once the whole configuration file is loaded, since the sources of an application are made when its modules load, and again when it is reloaded. A warning is logged for a name that no source has.

Example:

```
<loglevel source="myapp:requests" level="debug"/>
```
The `logs:` order of the command pipe (see `<commandpipe>` below) sets the level of a source at run time.

#### `<user>` and `<group>` : user who runs the server

**Deprecated and ignored.** Ocsigenserver no longer drops privileges: these tags are accepted for backward compatibility but have **no effect**, and the server logs a warning if they are present.

**Do not launch the server as root.** Run it directly as a dedicated unprivileged user (e.g. `www-data`). To serve a privileged port (80 or 443\) as a non-root user, use one of:

- a reverse proxy (nginx, HAProxy, Caddy, ...) terminating ports 80/443 and forwarding to Ocsigenserver on a high port (recommended, see the `<timeout>` note below);
- the Linux capability `cap_net_bind_service` on the native executable (`setcap 'cap_net_bind_service=+ep' ...`, on `ocsigenserver.opt`: see the [introduction](./index.md));
- a firewall/port redirection from 80/443 to a high port.

#### `<charset>` : default charset for pages

Example:

```
<charset>utf-8</charset>
```

#### `<uploaddir>` : directory where files are uploaded

If you want to allow the users to upload files on the server, add this option.

Example:

```
<uploaddir>/var/upload</uploaddir>
```
By default, no directory is specified and uploading is forbidden. An empty element, `<uploaddir></uploaddir>`, forbids it too. Note that the server will remove the files after the request has been served. If the service (the script) want to keep the file, it must create a new hard link on the file itself.

You can specify the maximum size of uploaded files using `<maxuploadfilesize>`. For example :

```
<maxuploadfilesize>2MB</maxuploadfilesize>
```
The value may be "infinity" or written using SI or binary units, e.g. 10 10B 10kB 10kiB 10MiB 10TB ... (or 10o 10ko ...) (default in bytes when no unit is specified)

*Warning:* the maximum size of files is also limited by `<maxrequestbodysize>`.

Starting from Ocsigenserver 1\.3, you can redefine the options `<uploaddir>` and `<maxuploadfilesize>` on each host or inside `<site>` tags. The syntax is the same as above. There are however some limitations; please read carefully the documentation of the module [Extendconfiguration](./extendconfiguration.md#upload) before using this functionality.

#### `<maxrequestbodysize>` : maximum size of the body of incoming requests

Example:

```
<maxrequestbodysize>100MB</maxrequestbodysize>
```
The value may be "infinity" or written using SI or binary units, e.g. 10 10B 10kB 10kiB 10MiB 10TB ... (or 10o 10ko ...) (default in bytes when no unit is specified)

#### `<maxrequestbodysizeinmemory>` : maximum size of a request body kept in memory

This is the maximum size of a request body that Ocsigenserver keeps in memory while decoding it, for example when reading the parameters of a form encoded as `application/x-www-form-urlencoded`. A request whose body exceeds this size is rejected with HTTP status 413 (Request Entity Too Large), and a warning is logged. The default is 1 MiB (1048576 bytes).

Decoding a request body may use a few times this size in memory, for each request being decoded, so the memory used in the worst case grows with `<maxconnected>`. Lower this value to reduce it, or raise it if legitimate requests, for example large RPC calls, carry more parameters. The whole request body is still limited by `<maxrequestbodysize>`.

Example:

```
<maxrequestbodysizeinmemory>4MiB</maxrequestbodysizeinmemory>
```
The value may be "infinity" or written using SI or binary units, e.g. 10 10B 10kB 10kiB 10MiB 10TB ... (or 10o 10ko ...) (default in bytes when no unit is specified). The value "infinity" removes the in-memory limit, so the body is then bounded only by `<maxrequestbodysize>`.

#### `<mimefile>` : MIME type file

The file associating file name extensions to their MIME type. Example:

```
<mimefile>/etc/ocsigenserver/mime.types</mimefile>
```
**Limitation.** The file is read, but currently not used: Staticmod derives the `Content-Type` of the files it sends from their names, with the built-in table of the `magic-mime` library.

#### `<debugmode/>` : Debug mode for applications

Use this option for debugging your Web sites. Ocsigenserver itself does not use it: error pages never show the details of an exception, which go to the logs. Applications may read it with [`Ocsigen.Config.get_debugmode`](./Ocsigen-Config.md#val-get_debugmode): Eliom then explains its "Wrong parameters" pages, and keeps debugging information in the data it sends to the client. Example:

```
<debugmode/>
```

#### `<usedefaulthostname/>` : Do not trust Host HTTP header for absolute links

*(From Ocsigenserver 1\.2)*Use this option if you do not trust the Host HTTP header for building absolute links, CGI scripts and redirections. The default hostname set in the configuration file will be used instead. (Same as Apache's UseCanonicalName option). Example:

```
<usedefaulthostname/>
```

#### `<maxconnected>` : Maximum number of simultaneous connections

The maximum number of connections open at once, on all the ports together. Each port counts as one connection while it waits for a new one, so the server is full slightly before this number. Example:

```
<maxconnected>20000</maxconnected>
```
The default is 10000, or less if the system does not allow as many open files. Each connection is an open file, its socket, and may use a second one: a static file it sends, or a connection to a proxied server. 100 files are kept for the rest of the server. At startup, the server raises its limit on open files (the soft limit) to what `<maxconnected>` needs, as far as the system allows (the hard limit), and no further, since the programs it runs inherit this limit. It then lowers `<maxconnected>` to what this limit affords, with a warning if the value was set in the configuration.

To accept more connections, raise the hard limit: with `ulimit -Hn` in the shell that starts the server, or with `LimitNOFILE=` in its systemd unit. Lwt needs libev for that (the `conf-libev` opam package, a dependency of Ocsigen Server except on Windows): with its `select` engine, which handles 1024 files at most, the server keeps to that.

When the server is full, it makes room for a new client by closing the connection that has waited the longest among those that their clients are not using: idle between two requests, without the start of a request, or with a TLS handshake not finished. The closing is logged at the `info` level. When all the connections are in use, new connections wait until others end.

Since a connection counts until it ends, the timeouts below matter: they keep clients that are slow, or pretend to be, from holding connections for ever. A client that sends its requests slowly, and opens a new connection each time one is cut, can still take many connections: `<maxconnectionsperaddress>` limits how many a single client address may take.

An Eliom application keeps a connection open for each page in use in a browser, for its comet channel: it needs at least as many connections as its users may have pages open at once.

#### `<timeout>` : Timeout for connections

Written in seconds; `<clienttimeout>` is an alias. Default: 30\. Example:

```
<timeout>20</timeout>
```
The timeout bounds how long the server waits for a client, in a way that depends on what the connection is waiting for:

- the TLS handshake, and the headers of a request, must have arrived in full within the timeout, counted from the opening of the connection (for the headers, from the end of the handshake), or from the first byte of the request after an idle period. A client that sends its request slowly, even one byte at a time ("Slowloris"), cannot hold a connection;
- the body of a request must keep coming: each part of it must arrive within the timeout after the previous one, and it must also keep the minimum rate set by `<mindatarate>`;
- likewise, a client must keep reading its response: each part of it must be taken within the timeout, at the minimum rate;
- producing the response is not bounded: a long computation, a streamed response or an Eliom comet request, which waits for an event, is not cut. Only the time spent waiting for the client counts.
A connection that exceeds these bounds is closed without an answer, and the closing is logged at the `info` level with the address of the client. The server checks the bounds once a second, so a connection is closed up to two seconds after its bound, never before. A value of `0` disables the timeout, and the minimum rate of `<mindatarate>` with it.

`<servertimeout>` is accepted for compatibility, but has no effect, and a warning says so.

#### `<keepalivetimeout>` : Timeout for idle connections

Written in seconds. Default: 75, as in nginx. Example:

```
<keepalivetimeout>30</keepalivetimeout>
```
How long a connection may stay idle between two requests (HTTP keep-alive) before the server closes it. Once the first byte of the next request arrives, the request is bounded by `<timeout>`. A value of `0` disables this bound.

Behind a reverse proxy that keeps its connections to Ocsigenserver alive, this value must be larger than the idle timeout of the proxy (60 seconds for nginx and for AWS load balancers), so that the proxy closes an idle connection first: otherwise the proxy may send a request on a connection at the moment the server closes it, and answer that request with an error (502).

#### `<mindatarate>` : Minimum data rate

Written in bytes per second. Default: 500, as the `MinRate` of Apache's `mod_reqtimeout`, which bounds requests only. Example:

```
<mindatarate>1000</mindatarate>
```
The minimum rate at which a client must send the body of a request, and read its response, once it has had `<timeout>` seconds of grace. It keeps a client from holding a connection by sending a body, or reading a response, a few bytes at a time ("R-U-Dead-Yet" and "Slow Read" attacks), each within the timeout. Only the time spent waiting for the client counts: a body that the server reads slowly, or a response that it produces slowly, does not make the client too slow. A value of `0` disables this bound, and so does a `<timeout>` of `0`.

#### `<maxconnectionsperaddress>` : Maximum number of connections per client

Default: no limit. Example:

```
<maxconnectionsperaddress>32</maxconnectionsperaddress>
```
The maximum number of connections a single client may have open at once. A further connection is closed as soon as it is accepted, and logged at the `info` level. This keeps a single client from taking all the connections that `<maxconnected>` allows. An IPv6 address counts for its /64 network, the usual allocation to a single site. Connections through Unix domain sockets are not limited.

Browsers open up to six connections per site, and several users may share an address (behind a NAT, in a company or on a mobile network), so the limit should not be too low. Behind a reverse proxy, all the connections come from the address of the proxy: leave this limit out, and set it in the proxy instead.

**Recommendation.** For an Internet-facing deployment, a reverse proxy (nginx, HAProxy, Caddy, ...) in front of Ocsigenserver still brings what the server does not do itself: buffering slow clients, so that they do not hold a connection to the server at all, rate limiting, and obtaining certificates.

#### `<shutdowntimeout>` : Timeout for shutdown

Written in seconds, or `notimeout`. Example:

```
<shutdowntimeout>10</shutdowntimeout>
```
**Limitation.** This option is currently parsed but **not used**: the `shutdown` server command does not read it (see [Stopping the server](./#shutdown) below).

#### `<netbuffersize>` : Size of the multipart buffer

Written in bytes, without a unit. Default: 8192\. The maximum size of the buffer used to read `multipart/form-data` request bodies (forms with files): the headers of each part must fit in it, otherwise the request gets `413 Request Entity Too Large`. The headers of the request itself are read by cohttp, and do not depend on this option. Example:

```
<netbuffersize>8192</netbuffersize>
```

#### Settings with no effect

The following settings are accepted for compatibility, but have no effect:

- `<filebuffersize>`, the size of the buffer for reading files to send;
- `<minthreads>` and `<maxthreads>`, the size of the pool of preemptive threads, which Ocsigenserver no longer has since version 7\.0. The server still refuses a `<minthreads>` larger than `<maxthreads>`;
- `<disablepartialrequests/>` and `<respectpipeline/>`.

#### `<maxretries>` : Maximum number of retries for the same request

To prevent looping requests, for example after a rewrite of the request, the number of retries is limited to this number. Example:

```
<maxretries>10</maxretries>
```

#### `<commandpipe>` : The pipe used to give orders to the server

Ocsigenserver is waiting for orders on a pipe. Write the orders on the pipe. This option allows to set the name of the pipe. Example:

```
<commandpipe>/var/run/ocsigenserver_command</commandpipe>
```
Without this option, the server has no command pipe.

The server creates the pipe with mode 0o600 if it does not exist: only the user running the server can give orders. If the path already exists, it must be a named pipe that belongs to this user, otherwise the server logs an error and runs without a command pipe. A pipe with group or other permissions is replaced with a new one, so that a process that opened it before loses its access.

Each extension can define its own orders.

Predefined orders:

- `reload`: will reload the configuration file. Extensions and libraries will not be reloaded, but Eliom loads the modules of its sites (`<eliom module=...>`) again *(see below for more information)*.
- `gc`: will trigger a heap compaction
- `shutdown`: will stop the server *(see below for more information)*
- `reopen_logs`: will reopen the logs files (see logrotate configuration for an example of use)
- `clearcache`: will empty all the caches defined by the module `Ocsigen_base.Cache`
- `logs:name level`: will set the level of the log source `name`, as `<loglevel>` does; `logs:name` turns the source off

#### `<extconf>` : Splitting the configuration file

If your configuration is complex, it is recommended to split the configuration file into several files. The following command will include all external configuration files matching `*.conf` from this directory (in alphabetical order):

```
<extconf dir="..."/>
```

### Loading extensions and libraries

#### `<extension>` : Loading extensions and libraries

Ocsigenserver does not do anything without some extensions loaded. For example, you need the extension staticmod for serving static pages. Extensions are loaded dynamically when launching the server. Specify the extensions and libraries you want using the tag \<extension\>.

Warning: The order in which extensions are loaded is usually not significant (from 0\.99.4) (but if you have dependencies between the modules). Extensions will try to handle requests in the order in which the options are written inside `<host>`.

If you want to run a Web site with Eliom, you probably need the extensions Staticmod and Eliom, the first one to generate static pages, the second one for dynamic pages.

If you don't have any extension, no page will be generated.

Extensions will not be reloaded if you ask the server to reread the configuration file. And they will be loaded only once, even if you have several times the same extension. For example if you are using `<extconf>` to configure several websites in several configuration files, you can put all the dependencies in each file.

Some extensions take parameters between `<extension>` and `</extension>`

Example of use:

```
<extension module="/opt/godi/lib/ocaml/site-lib/ocsigenserver/staticmod.cma"/>
```
(From version 1\.0) Extensions can also be loaded using Findlib. If you specify a package which has dependencies, these dependencies will also be loaded, without parameters. Dependencies are always loaded only once, so you can safely load several extensions which have a shared dependency.

If you don't want to clutter your installation with a lot of Ocsigen-specific packages, you can put them outside the default search path (for example, something below `/usr/local/lib/ocsigenserver`), and extend the search path in the configuration file. Here, "search path" has the same semantics as in Findlib. For example, you might have the following line in your configuration file if you compiled and installed Ocsigenserver on your own:

```
<findlib path="/usr/local/lib/ocsigenserver/METAS"/>
```
Extensions shipped with Ocsigen Server are available as subpackages of `ocsigenserver.ext` (this package is in an extended search path). For example:

```
<extension findlib-package="ocsigenserver.ext.deflatemod"/>
```
This will load deflatemod, and its dependencies (bytesrw and its zlib and zstd libraries) if not already loaded.

#### `<require>` : Loading libraries

From version 1\.0, `<require>` is a synonymous for `<extension>`.

#### `<library>` : Loading libraries

`<library>` loads OCaml libraries for your extensions or Web sites. It now behaves exactly as `<extension>`: a library is loaded once, and is not reloaded when the configuration file is reread.

Libraries may take parameters between `<library>` and `</library>`.

Example of use:

```
<library module="/opt/godi/lib/ocaml/site-lib/mysite/mylib.cma"/>
```
As in `<extension>`, you can also use `findlib-package=` instead of `module=`.

### Virtual host and site configuration

#### `<host>` : Virtual host setting

It is possible to have several virtual hosts on the same server. For example if your server has two host names, you may want to serve different Web sites for each of them. Note that this feature is based on the `Host:` HTTP header, which is not mandatory in HTTP/1.0.

The attribute `defaulthostname` is just an information about the hostname, that can be used for example by services to create absolute links. If you want to create virtual hosts for several domain names, use the `hostfilter` attribute to filter on the `Host` HTTP header. You may specify several hostnames, and the optional `hostfilter` field may contain '\*' (wildcard). Example:

```
<host defaulthostname="www.mysite.com" hostfilter="www.mysite.com *.org" charset="utf-8"> ... </host>
```
The `charset` attribute is the default charset for your pages. All attributes are optional, but we recommend to set at least `defaulthostname`.

If you do not want to send any charset, explicitly: `charset=""`. In that case, browsers usually use html meta-data (when present).

*Warning:* Before version 1\.2.0, `hostfilter` was called: `name` (or `aliases`).

It is also possible to set the default ports for http and https, and default protocol using `defaulthttpport`, `defaulthttpsport` and `defaultprotocol=`. These arguments may be used by extensions (like Eliom) if they need to create absolute links. Example:

```
<host defaulthostname="www.mywebsite.org" hostfilter="*.org" defaulthttpport="8080" defaulthttpsport="4433" defaultprotocol="https"> ... </host>
```
Inside `<host>`, write the configuration for your main (root) site. These options depend on the extensions loaded. See the documentation about the extensions to know what to put here. It is also possible to define here several subsites, using `<site>` (see below).

#### `<site>` : Configuring sub-sites

In each virtual host, it is possible to define several sub-sites (in sub-paths) between `<site>` and `</site>`. `<site>` has a mandatory attribute `path`, the path of the site in the URLs (`dir` is an older name for it). You can also set the default charset for the site using the `charset` attribute. The options between `<site>` and `</site>` are exactly the same as those you can put between `<host>` and `</host>`. They depend on the extensions you loaded. It is also possible to define sub-sites inside sub-sites.

Example of use:

```
<site path="mysite"> ... </site>
```

### Order of the directives

Inside a `<host>` or a `<site>`, the server tries the directives (the tags defined by the extensions) in the order of the file:

- As long as no page has been generated, each directive may generate one (for example `<static>`, `<eliom>`, `<redirect>` or `<revproxy>`), or change the request for the directives that follow it (for example the options of [Extendconfiguration](./extendconfiguration.md), or `<allow-forward-for/>`).
- Once a page has been generated, the directives that generate pages are skipped, and the output filters (`<outputfilter>`, `<deflate>`, `<cors>`, `<securityheaders>`) may change the response. So an output filter must come after the directives whose responses it changes. The error that the server sends when no directive generated a page (for example a 404\) does not go through the output filters.
- A `<site>` applies only to the requests whose path starts with its path, and only if no page has been generated yet. The changes that its directives make to the request do not last after the end of the site.
- A request rewritten by `<rewrite>` starts again from the first `<host>`, unless the rewriting has the `continue` attribute. A request starts again at most `<maxretries>` times.

### Regular expressions

The regular expressions of the configuration file use the Perl syntax as implemented by the [ocaml-re](https://github.com/ocaml/ocaml-re) library (`Re.Pcre`). Unlike PCRE, it has no back-references and no look-around.

They must match the *whole* string, as if they started with `^` and ended with `$`. The exception is `<outputfilter>`, which replaces every match inside the value of a header. To match a site `app` and all the pages below it, write `app(/.*)?`.

The string depends on the directive:

- the path of the request, relative to the enclosing `<site>`, without its leading slash, and decoded: `<static regexp>`;
- the same path, but percent-encoded (a space is `%20`): `<path>` of Accesscontrol, `<userconf regexp>`;
- the same encoded path, followed by `?` and the query string if there is one: `<redirect suburl>`, `<revproxy suburl>`, `<rewrite regexp>`;
- the whole URL, such as `http://example.org:8080/dir/page?a=1`, where the path starts at the root of the host (not of the enclosing site), and where the port is written only when it is not the default port of the protocol: `<redirect fullurl>`, `<revproxy fullurl>`;
- each value of a header: `<header>` of Accesscontrol, `<outputfilter>`;
- the path of a local file: `<regexp>` in `<hidefile>` and `<forbidfile>`;
- the status code of the error, such as `404`: `<static code>`, `<ifnotfound code>`.
In the destination of a rewriting (`dest`, `url`), `\0` stands for the whole match and `\1` to `\9` for the groups between parentheses, and a backslash escapes the next character. In Staticmod and Userconf, `$u(name)` stands for the home directory of user `name`.

### Reloading the configuration file

To reload the modules of the configuration file without stopping the server, write the command `reload` in the server's command pipe (the path given by `<commandpipe>`). For example:

```
echo reload > /var/run/ocsigenserver_command
```
From version 1\.3, it is possible to give the name of the configuration file as parameter:

```
echo reload /etc/ocsigenserver/ocsigenserver.conf.2 > /var/run/ocsigenserver_command
```
Most distributions have a special command to do that automatically (for the default configuration file), usually:

```
/etc/init.d/ocsigenserver reload
```
The configuration file will be reloaded, and the changes will be taken into account if possible. For example it is not possible to change the port numbers without stopping the server for now. Extensions and libraries loaded using `<extension>` will not be reloaded.

If the file has an error, the server logs it, followed by "Config file reload failed", and keeps its sites as they were (the server-wide settings that come before the error in the file may already be applied). Otherwise it logs "Config file reloaded".

### Stopping the server

To stop the server, use the `shutdown` command, for example:

```
echo shutdown > /var/run/ocsigenserver_command
```
The server process exits at once. It does not wait for the requests in progress to finish.

You can give a delay as parameter (in seconds):

```
echo shutdown 10 > /var/run/ocsigenserver_command
```
The server process then exits after 10 seconds. During this delay, the server keeps working as usual: it still accepts new connections, and it does not wait for the requests in progress to finish before exiting.

The order `shutdown notimeout` is accepted too. It is meant to wait for the requests in progress without a time limit, but since the server does not wait for them yet, the process exits at once, as with `shutdown`.

The `<shutdowntimeout>` option of the configuration file is currently not used by this command.
