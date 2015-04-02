(* Ocsigen
 * http://www.ocsigen.org
 * ocsigen_range.ml Copyright (C) 2008
 * Vincent Balat
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

val compute_range : 
  Ocsigen_request_info.request_info ->
  Ocsigen_http_frame.result ->
  Ocsigen_http_frame.result Lwt.t

val get_range : Ocsigen_http_frame.t ->
  ((int64 * int64) list * int64 option * Ocsigen_request_info.ifrange) option
