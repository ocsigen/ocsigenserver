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

let section = Lwt_log.Section.make "ocsigen:command"

exception Unknown_command

let register_command_function, get_command_function =
  let command_function = ref (fun ?prefix _ _ -> Lwt.fail Unknown_command) in
  ((fun ?prefix f ->
      let prefix' = prefix in
      let old_command_function = !command_function in
      command_function :=
        (fun ?prefix s c ->
           Lwt.catch (fun () -> old_command_function ?prefix s c)
             (function
               | Unknown_command ->
                 if prefix = prefix'
                 then f s c
                 else Lwt.fail Unknown_command
               | e -> Lwt.fail e))),
   (fun () -> !command_function))


let () =
  register_command_function
    ~prefix:"logs"
    (Ocsigen_messages.command_f Unknown_command)
