(* Ocsigen
 * http://www.ocsigen.org
 * Module accesscontrol.ml
 * Copyright (C) 2007 Vincent Balat, Stéphane Glondu
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

(** Accesscontrol: Conditional access to some sites *)

(** If you want to use this extension with Ocsigen Server's configuration file, 
+   have a look at the {% <<a_manual chapter="accesscontrol"|manual page>>%}.
+   If you are using Ocsigen Server as a library, use the interface described
+   here. Each of these functions behaves exactly as its configuration file
    counterpart. 
+*)

(**
This module belongs to ocamlfind package
   [ocsigenserver.ext.accesscontrol].
*)

type condition

val ip : string -> condition
val port : int -> condition
val ssl : condition
val header : name:string -> regexp:string -> condition
val method_ : Cohttp.Code.meth -> condition
val protocol : Cohttp.Code.version -> condition
val path : regexp:string -> condition
val and_ : condition list -> condition
val or_ : condition list -> condition
val not_ : condition -> condition

val if_ :
   condition
  -> Ocsigen_server.instruction list
  -> Ocsigen_server.instruction list
  -> Ocsigen_server.instruction

val iffound : Ocsigen_server.instruction list -> Ocsigen_server.instruction

val ifnotfound :
   ?code:string
  -> Ocsigen_server.instruction list
  -> Ocsigen_server.instruction

val notfound : Ocsigen_server.instruction
val nextsite : Ocsigen_server.instruction
val nexthost : Ocsigen_server.instruction
val stop : Ocsigen_server.instruction
val forbidden : Ocsigen_server.instruction

val allow_forward_for :
   ?check_equal_ip:bool
  -> unit
  -> Ocsigen_server.instruction

val allow_forward_proto : unit -> Ocsigen_server.instruction

val section : Lwt_log_core.section
(** Use Lwt_log.Section.set_level in order to change the log level *)
