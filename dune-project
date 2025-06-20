(lang dune 3.19)
(name ocsigenserver)
(version 6.0.0)

(generate_opam_files true)

(license "LGPL-2.1-only WITH OCaml-LGPL-linking-exception")
(authors "Ocsigen team <dev@ocsigen.org>")
(maintainers "Ocsigen team <dev@ocsigen.org>")
(source (github ocsigen/ocsigenserver))
(homepage "http://ocsigen.org/ocsigenserver")

(package
 (name ocsigenserver)
 (synopsis "A full-featured and extensible Web server")
 (description
  "Ocsigen Server is a Web Server that can be used either as a library for OCaml or as an executable (taking its configuration from a file). It has a very powerful extension mechanism that makes it very easy to plug your own OCaml modules for generating pages. Many extensions are already implemented, like a reverse proxy, content compression, access control, authentication, etc.")
 (depends
  (ocaml (>= 4.08.1))
  (camlzip (>= 1.04))
  (cohttp-eio (>= 6.0.0))
  http
  cryptokit
  (ipaddr (>= 2.1))
  (eio (>= 0.13))
  eio_main
  lwt_react
  lwt_ssl
  ocamlfind
  (re (>= 1.11))
  react
  (ssl (>= 0.5.8))
  xml-light
  logs
  logs-syslog
  syslog-message)
 (conflicts
  (pgocaml (< 2.2))))
