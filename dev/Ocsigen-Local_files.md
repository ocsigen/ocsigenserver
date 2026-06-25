
# Module `Ocsigen.Local_files`

```ocaml
val section : Logs.src
```
```ocaml
exception Failed_404
```
The requested file does not exists

```ocaml
exception Failed_403
```
The requested file cannot be served: does not exists, not enough permissions ...

```ocaml
exception NotReadableDirectory
```
The file is a directory which we should not display

```ocaml
type resolved = 
  | RFile of string
  | RDir of string
```
Local file corresponding to a request. The string argument represents the real file or directory to serve, eg. foo/index.html instead of foo

```ocaml
val resolve : 
  ?no_check_for:string ->
  request:Extensions.request ->
  filename:string ->
  unit ->
  resolved
```
Finds `filename` in the filesystem, with a possible redirection if it is a directory. Takes into account the fact that `filename` does not exists, is a symlink or is a directory, and raises Failed\_404 or Failed\_403 accordingly.

- we return `"filename/index.html"` if `filename` corresponds to a directory, `"filename/index.html"` is valid, and `"index.html"` is one possible index (trying all possible indexes in order)
- we raise `Failed_404` if `filename` corresponds to a directory, no index exists and `list_dir_content` is false. Warning: this behaviour is not the same as Apache's but it corresponds to a missing service in Eliom (answers 404\). This also allows to have an Eliom service after a "forbidden" directory
- we raise `Failed_403` if `filename` is a symlink that must not be followed
- raises `Failed_404` if `filename` does not exist, or is a special file
- otherwise returns `filename`
`no_check_for` is supposed to be a prefix of `filename` ; directories above `no_check_for` are not checked for symlinks
