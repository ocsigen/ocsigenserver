(** {3 Extending server commands} *)
exception Unknown_command

(** Use a prefix for all your commands when you want to create
    extension-specific commands.
    For example if the prefix is "myextension" and the commande "blah",
    the actual command to be written by the user is "myextension:blah".
    Give as parameter the function that will parse the command and do an action.
    Its first parameter is the full command as a string.
    The second one is the command without prefix, split by word.
    It must raise [ocsigen_extensions.Unknown_command] if it does
    not recognize the command.
*)
val register_command_function :
  ?prefix:string -> (string -> string list -> unit Lwt.t) -> unit

(**/**)
val get_command_function :
  unit -> (?prefix:string -> string -> string list -> unit Lwt.t)
