(lang dune 3.19)
(name ocsigenserver)
(version 7.0.0)

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
 (maintenance_intent "(latest)")
 (depends
  (ocaml (>= 4.10))
  (camlzip (>= 1.08))
  (cohttp-lwt-unix (>= 6.0))
  conduit-lwt-unix
  http
  cryptokit
  (ipaddr (>= 2.1))
  (lwt (>= 5.7.0))
  lwt_react
  lwt_ssl
  ocamlfind
  (re (>= 1.11))
  react
  (ssl (>= 0.5.8))
  xml-light
  logs
  logs-syslog
  (syslog-message (>= 1.1.0)))
 (conflicts
  (pgocaml (< 2.2))))