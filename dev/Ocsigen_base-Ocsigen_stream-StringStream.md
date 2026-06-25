
# Module `Ocsigen_stream.StringStream`

```ocaml
type out = string t
```
Interface for stream creation (for tyxml)

```ocaml
type m
```
```ocaml
val make : m -> out
```
```ocaml
val empty : m
```
Create an empty stream

```ocaml
val put : string -> m
```
Create a stream with one element

```ocaml
val concat : m -> m -> m
```
Concatenate two stream
