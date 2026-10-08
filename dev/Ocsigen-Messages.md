# Module `Ocsigen.Messages`

Writing messages in the logs

```ocaml
val access_sect : Logs.src
```
```ocaml
val accesslog : string -> unit
```
Write a message in access.log

```ocaml
val errlog : ?section:Logs.src -> string -> unit
```
Write a message in errors.log

```ocaml
val warning : ?section:Logs.src -> string -> unit
```
Write a message in warnings.log

```ocaml
val console : (unit -> string) -> unit
```
Write a message in the console (if not called in silent mode)

```ocaml
val unexpected_exception : exn -> string -> unit
```
Use that function for all impossible cases in exception handlers (`try ... with ... | e -> unexpected_exception ...` or `Lwt.catch ...`). A message will be written in `warnings.log`. Put something in the string to help locating the problem (usually the name of the function where is has been called).

```ocaml
val error_log_path : unit -> string
```
Path to the error log file

```ocaml
val stdio_reporter : Logs.reporter
```
A reporter that writes warnings and errors to `stderr` and everything else to `stdout`, without opening any log file. It is used by the one-command serve mode (where access lines are written directly to `stdout`) and to report command-line errors before the logging system is configured.

```ocaml
val level_of_string : string -> Logs.level option
```
`level_of_string s` is the level named `s`: `"debug"`, `"info"`, `"notice"`, `"warning"`, `"error"` or `"fatal"` (the same as `"error"`).

```ocaml
val set_source_level : string -> Logs.level option -> bool
```
`set_source_level name level` sets the level of every log source named `name` to `level` (`None` turns them off), and is `false` when there is no such source.
