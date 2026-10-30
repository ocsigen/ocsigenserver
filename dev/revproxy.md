# Revproxy

Revproxy is a reverse proxy for Ocsigen. It cannot be used in [user configuration files](./userconf.md).

This module allows to transmit some requests to another Web server using the HTTP or HTTPS protocol. For example if you want to use Ocsigen together with another Web server (Apache, Tomcat, etc. or even another Ocsigen).

### Configuration

To use that extension, load OCamlfind package `ocsigenserver.ext.revproxy`, either from your Dune file (if you are using static linking), or from the configuration file:

```
<extension findlib-package="ocsigenserver.ext.revproxy"/>
```
This page describes the configuration file options. If you are building a statically linked executable without configuration file, use the corresponding functions from module [`Revproxy`](./Revproxy.md).

Here are some examples of configuration file options:

```
<revproxy suburl="(.*)" dest="http://my.newaddress.org:8080/\1"/>
<revproxy suburl="dir/(.*)\.html" dest="https://my.newaddress.org/\1.php"/>
<revproxy fullurl="http://(.*).mydomain.org:(80|8080)/(.*)" dest="http://\1.my.newdomain.org:\2/\3"/>
```
The value of `suburl` or `fullurl` is a regular expression, which must match the whole path or URL (see [Regular expressions](./config.md#regexps)). The destination URL is built from `dest`, where `\1` to `\9` stand for the parts of the request that match the groups between parentheses of the regular expression.

Add the attribute `keephost="keephost"` if you want to send the original `Host` header to the distant server. Otherwise, it is computed from `dest`. In both cases, the proxy connects to the host of `dest`. With keephost, if `dest` has a port and the header has none, that port is added to the header.

The attribute `nopipeline="nopipeline"` is accepted for compatibility, but has no effect: the reverse proxy opens a new connection for each request.

You probably also need to rewrite locations in the output (if the server is doing redirections). For example:

```
<outputfilter header="location" regexp="http://my.newaddress.org/(.*)" dest="http://my.publicaddress.org/\1"/>
```
Note that if you are using ocsigen behind a reverse proxy that sets the X-Forwarded-For and X-Forwarded-Proto headers (like the revproxy module), you should allow ocsigen to use this information, using the [accesscontrol](./accesscontrol.md) extension:

```
<if>
	<ip value="ip of the proxy"/>
	<then>
	 	<allow-forward-for/>
    		<allow-forward-proto/>
	</then>
</if>
```

### Technical details

- The reverse proxy does no caching for now.
- The query string is decoded, then encoded again before it is sent, so its encoding may change: for instance, `+` is sent as `%20` and `;` as `%3B`. `+` and `%20` both mean a space in a form, but a target that reads `+` literally, or splits the query on `;`, sees something else.
