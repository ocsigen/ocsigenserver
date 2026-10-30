(** Text description of decoded form data, shared by the server and the
    chunk tests, so that their outputs can be compared. *)

val post_data : Ocsigen.Multipart.post_data -> string
(** [post_data (params, files)] has one line per parameter, in order, then one
    line per file, in order. A parameter line has its name and its value, or
    the size and the digest of a value longer than 64 bytes. A file line has
    its field name, its original file name, its Content-Type ("-" if none),
    its size and the digest of the content of its temporary file. *)
