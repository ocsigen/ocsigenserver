
# Module `Ocsigen_http.Charset_mime`

```ocaml
type extension = string
```
```ocaml
type file = string
```
```ocaml
type filename = string
```
Charset

```ocaml
type charset = string
```
By convention, "no specified charset" is represented by the empty string

```ocaml
val no_charset : charset
```
```ocaml
type charset_assoc
```
Association between extensions and charset, with a default value.

```ocaml
val empty_charset_assoc : ?default:charset -> unit -> charset_assoc
```
All files are mapped to `no_charset`

```ocaml
val empty_mime_assoc : ?default:charset -> unit -> charset_assoc
```
```ocaml
val find_charset : string -> charset_assoc -> charset
```
```ocaml
val default_charset : charset_assoc -> charset
```
Functions related to the default charset in the association

```ocaml
val set_default_charset : charset_assoc -> charset -> charset_assoc
```
```ocaml
val update_charset_ext : charset_assoc -> extension -> charset -> charset_assoc
```
Updates the mapping between extensions from a file to its charset. The update can be specified using the extension of the file, the name of the file, or the entire file (with its path)

```ocaml
val update_charset_file : charset_assoc -> filename -> charset -> charset_assoc
```
```ocaml
val update_charset_regexp : 
  charset_assoc ->
  Re.Pcre.regexp ->
  charset ->
  charset_assoc
```
```ocaml
type mime_type = string
```
MIME types; the default value is `"application/octet-stream"`

```ocaml
val default_mime_type : mime_type
```
```ocaml
type mime_assoc
```
association between extensions and mime types, with default value

```ocaml
val default_mime_assoc : unit -> mime_assoc
```
Default values, obtained by reading the file specified by `Ocsigen_base.Config_static.get_mimefile`

```ocaml
val parse_mime_types : filename:string -> mime_assoc
```
Parsing of a file containing mime associations, such as /etc/mime-types

```ocaml
val find_mime : file -> mime_assoc -> string
```
```ocaml
val default_mime : mime_assoc -> mime_type
```
```ocaml
val set_default_mime : mime_assoc -> mime_type -> mime_assoc
```
```ocaml
val update_mime_ext : mime_assoc -> extension -> mime_type -> mime_assoc
```
```ocaml
val update_mime_file : mime_assoc -> filename -> mime_type -> mime_assoc
```
```ocaml
val update_mime_regexp : 
  mime_assoc ->
  Re.Pcre.regexp ->
  mime_type ->
  mime_assoc
```