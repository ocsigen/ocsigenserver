(* Ocsigen
 * http://www.ocsigen.org
 * Module ocsigen_extensions.ml
 * Copyright (C) 2015 Vincent Balat
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*)

(** Extending server commands *)


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
