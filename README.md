Ocsigen server

------------------------------------------------------------------

Requirements:
=============

Compilers:

 * ocaml and camlp4  (need version >= 3.12.1)
 * a C compiler      (tested with gcc-4.4.5)

Libraries:

 * findlib
 * react             (tested with 0.9.3)
 * ocamlssl          (tested with 0.4.6)
 * lwt               (need version >= 2.4.2, with react and ssl support)
 * ocamlnet          (tested with 3.6, with netstring, netstring-pcre and netsys)
 * pcre-ocaml        (tested with 6.2.5)
 * cryptokit         (tested with 1.6)
 * ocaml-text        (tested with 0.6)
 * tyxml             (need version 3)
 * ipaddr            (need version >= 2.1)
 * ocamlsqlite3      (tested with 2.0.2) OR
 * dbm               (tested with 1.0)


Optional libraries:

 * camlzip           (tested with 1.04)

Ocsigenserver supports both dbm and sqlite3. Note well, that dbm isn't part of
the distribution of OCaml>=4 any more, but an external package.

If OCaml 3.12.1 and the needed libraries (findlib/react/lwt...) are not
installed on your computer and not available on your favorite linux
distribution, you may use the Ocsigen bunble GODI to install them
automatically, see:

  http://ocsigen.org/install#bundle
  http://godi.camlcity.org/godi/index.html

To run the native code version of ocsigen server, you may need to
generate cmxs files for the libraries you need, if they are not
included in your distribution, see:

  http://ocsigen.org/ocsigenserver/manual/misc


------------------------------------------------------------------

Build instructions:
===================

 * run "sh configure [options]" to generate 'Makefile.config'
 - For the full list of options, run "sh configure --help".

 * verify that 'Makefile.config' suits to your needs.

 * run "make" to compile
 * run "make install" as root to install

 * [optional] run "make logrotate" as root to install logrotate
              configuration files in /etc/logrotate.d

 * [optional] run "make doc" to build the ocamldoc
 * [optional] run "make install.doc" as root to install the ocamldoc

 * run "make uninstall" to uninstall (almost) everything

 * run "make purge" to uninstall everything (even configuration files)

------------------------------------------------------------------

Local testings:
===============

 * run "make run.local" of "make run.opt.local"
   in the ocsigen source directory.

 * open http://localhost:8080/index.html in your browser

 * if it does not work, look at the logs (see 'local/var/log/' in the
   ocsgigen source directory) or run ocsigen with options -v or -V
   (verbose and debug mode).

------------------------------------------------------------------

Authors:
========

* Vincent Balat
  (project leader, Web server, Ocsigenmod, Eliom, Eliom client, Staticmod, XHTML syntax extension, documentation, Ocsimore, extension mechanism, Ocsidbm, Ocsipersist with DBM, ...)

* Jérôme Vouillon
 (Lwt, Web server, js_of_ocaml, O'Closure, ...)

* Boris Yakobowski
 (Ocsimore, module Extendconfiguration, Ocsigen server...)

* Benjamin Canou
 (O'Browser)

* Jérémie Dimino
 (Lwt)

* Raphaël Proust
 (Ocsforge, Eliom client, Comet)

* Stéphane Glondu
 (Configuration file, Findlib integration, access control, HTTP authentication, Debian package, ...)

* Gabriel Kerneis
 (XHTML syntax extension for OCaml 3.10, Ocsipersist with SQLite, CGI module, forms in Eliom, deflatemod, ...)

* Denis Berthod
 (HTTP protocol, Web server)

* Grégoire Henry
 (safe unmarshalling of client data)

* Pierre Chambart
 (Comet)

* Jaap Boender
 (Ocsimore, NetBSD and Godi packages)

* Gabriel Scherer
 (Macaque)

* Gabriel Cardoso
 (O'Closure)

* Jean-Henri Granarolo
 (Ocsforge)

* Simon Castellan
 (HTML5, OpenID, SVG)

* Piero Furiesi
 (Ocsimore)

* Thorsten Ohl
 (most of the functions generating XHTML (xML and xHTML modules))

* Mauricio Fernandez
 (Xhtmlcompact, static linking of extensions and Eliom modules)

* Nataliya Guts
 (Web server, HTTPS)

* Archibald Pontier
 (Atom, Pubsubhubbub)

* Jérôme Velleine
 (CGI module)

* Charles Oran
 (O'Closure)

* Pierre Clairambault
 (Lwt_lib, Gentoo package, configure script, ...)

* Cécile Herbelin
  (HTML5, Benchmarks)
