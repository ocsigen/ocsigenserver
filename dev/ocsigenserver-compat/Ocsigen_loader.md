
# Module `Ocsigen_loader`

Notes about Findlib usage:

- Findlib is called with predicates "plugin", "mt". Moreover, the predicate "native" or "byte" is added, depending on whether Ocsigen is running in native or bytecode mode.
- In native mode, .cmx/.cmxa extensions provided by META files are replaced by .cmxs.
- The OCAMLPATH environment variable is ignored altogether.
```ocaml
exception Dynlink_error of string * exn
```
```ocaml
exception Findlib_error of string * exn
```
```ocaml
val section : Logs.src
```
```ocaml
val translate : string -> string
```
`translate filename` translate .cmo/.cma extensions to .cmxs in native mode, and .cmxs to .cmo (.cma if it exists) in bytecode mode.

```ocaml
val set_init_on_load : bool -> unit
```
If set to `true`, the module initialization functions passed to `set_module_init_function` will be executed directly. Otherwise, they will have to be invoked using `init_module` at some later stage.

```ocaml
val loadfile : (unit -> unit) -> (unit -> unit) -> bool -> string -> unit
```
`loadfile pre post force file` (dynamically) loads `file`. If `force` is `false`, remember `file` so that it isn't loaded twice. If the loading effectively occurs, `pre` (resp. `post`) is called before (resp. after) the loading. `post` will be called even if the loading fails.

```ocaml
val loadfiles : (unit -> unit) -> (unit -> unit) -> bool -> string list -> unit
```
`loadfiles pre post force file` loads all the `files`, using `loadfile (fun () -> ()) (fun () -> ()) false` for all the files but the last one, and `loadfile pre post force` for the last one (if any).

```ocaml
val add_module_init_function : string -> (unit -> unit) -> unit
```
`add_module_init_function name f` adds function `f` to the initialisation functions to be run when `init_module name` is called.

```ocaml
val set_module_init_function : string -> (unit -> unit) -> unit
```
`set_module_init_function name f` registers the function `f`, which will be used to initialize the module when `init_module name` is called. Will replace the prvious value.

```ocaml
val init_module : (unit -> unit) -> (unit -> unit) -> bool -> string -> unit
```
`init_module pre post force name` runs the init function for the module `name`. If `force` is `false`, remember `name` so that the init function isn't executed twice. If the function is executed, `pre` (resp. `post`) is called before (resp. after) the loading. `post` will be called even if the loading fails.

```ocaml
val get_ocamlpath : unit -> string list
```
Returns the current Findlib library search path.

```ocaml
val set_ocamlpath : string list -> unit
```
Sets the current Findlib library search path. The OCaml standard library path and some site-specific paths are always implicitly added.

```ocaml
val add_ocamlpath : string -> unit
```
Adds a path to the Findlib library search path.

```ocaml
val findfiles : string -> string list
```
`findfiles pkg` returns the list of files needed to load Findlib package `pkg`, including dependencies. The archive files of `pkg` will appear last in the returned result.
