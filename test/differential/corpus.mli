(** Inputs of the differential tests.

    A corpus file holds one case per line: a name, then the bytes to send,
    written as an OCaml string literal, so that line ends and other control
    bytes are visible and survive editors and Git:

    {v
# Comments and empty lines are ignored.
two-hosts "GET / HTTP/1.1\r\nHost: a\r\nHost: b\r\n\r\n"
    v} *)

type case = {name : string; input : string}

val parse : string -> (case list, string) result
(** [parse contents] is the list of the cases of the corpus file [contents],
    or an error message giving the line number of the first invalid line. *)

val load : string -> (case list, string) result
(** [load path] is [parse] of the contents of the file [path], the error
    message naming [path]. *)
