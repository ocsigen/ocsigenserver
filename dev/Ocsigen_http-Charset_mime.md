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
`default_mime_assoc ()` has no rules, and the default type [`default_mime_type`](./#val-default_mime_type). The mime.types file of [`set_mime_file`](./#val-set_mime_file) is not part of it: [`find_mime`](./#val-find_mime) looks it up after the rules.

```ocaml
val parse_mime_types : filename:string -> mime_assoc
```
Parsing of a file containing mime associations, such as /etc/mime.types

```ocaml
val set_mime_file : string option -> unit
```
`set_mime_file file` sets the mime.types file whose types come after the rules (see [`find_mime`](./#val-find_mime)), or none. The file is read at the next call of [`find_mime`](./#val-find_mime). There is none by default.

```ocaml
val mime_file : unit -> string option
```
`mime_file ()` is the file that [`set_mime_file`](./#val-set_mime_file) sets.

```ocaml
val find_mime : file -> mime_assoc -> string
```
`find_mime file assoc` is the MIME type of `file`: the one of the first rule of `assoc` that matches `file` (its extension, its name or a regular expression), else the one of the mime.types file of [`set_mime_file`](./#val-set_mime_file), else the one of the built-in table of the `magic-mime` library, else the default type of `assoc`.

```ocaml
val content_type : 
  mime_assoc:mime_assoc ->
  charset_assoc:charset_assoc ->
  file ->
  string
```
`content_type ~mime_assoc ~charset_assoc file` is the value of the `Content-Type` header for `file`: its MIME type, followed by its charset for text and XML types, when `charset_assoc` gives one.

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
