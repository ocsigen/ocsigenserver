(* Ocsigen
 * http://www.ocsigen.org
 * File dynlink_wrapper.ml
 * Copyright (C) 2008 Vincent Balat
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

#if native_dynlink

open Dynlink

exception Error = Dynlink.Error

let loadfile = loadfile
let error_message = error_message
let init = init
let allow_unsafe_modules = allow_unsafe_modules
let prohibit = prohibit

#else

exception Error of string

let message = "ocsigen compiled without native dynlink support"
let loadfile _ = failwith message
let error_message _ = failwith message
let init _ = ()
let allow_unsafe_modules _ = ()
let prohibit _ = ()

#endif
