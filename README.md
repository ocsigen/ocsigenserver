Ocsigen server

------------------------------------------------------------------

Build instructions:
===================

 * run "sh configure [options]" to generate 'Makefile.config'
 - For the full list of options, run "sh configure --help".

 * verify that 'Makefile.config' suits to your needs.

 * run "make" to compile

 * [optional] run "make logrotate" as root to install logrotate
              configuration files in /etc/logrotate.d

 * [optional] run "make doc" to build the ocamldoc

 * run "make purge" to uninstall everything (even configuration files)

------------------------------------------------------------------

Local testings:
===============

To run the automated tests, do:

 * dune runtest --auto-promote

 * this will update the files at test/extensions/*/run.t to reflect the
   behavior of ocsigenserver. The test files don't change if the server did not
   change behavior. Use Git to see the eventual changes.

Alternatively, you can also test the 'ocsigenserver' program using a config file:

 * run "make run.local" or "make run.opt.local"
   in the ocsigen source directory.

 * open http://localhost:8080/index.html in your browser

 * if it does not work, look at the logs (see 'local/var/log/' in the
   ocsgigen source directory) or run ocsigen with options -v.

 * this will use the config file at 'local/etc/ocsigenserver.conf'.

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

* Jan Rochel
  (Ocsipersist)
