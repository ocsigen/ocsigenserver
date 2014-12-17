(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2005
 * Vincent Balat, Denis Berthod, Nataliya Guts, Jérôme Vouillon
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

(** Reload the configuration of the server.
    The optional parameter [?file] may be use to read the configuration
    from another file.
*)
val reload: ?file:string -> unit -> unit

(** Start the server (does not return)

    @param connector function to connect the main loop of the server with
    the extensions engine (by default, the extensions engine is Ocsigen
    Extensions)

    @param configuration the optionnal argument can load OcsigenServer's
    extensions by default according to the configuration file. Otherwise,
    it waits a server configuration (like [(Ocsigen_socket.All, 8080)]).
    In the latter case, you must load extensions by hand (using the [init]
    function) before the application of this function.
*)
val start_server :
  ?connector:(Ocsigen_extensions.Ocsigen_request_info.request_info ->
              Ocsigen_http_frame.result Lwt.t) ->
  ?configuration:Ocsigen_server_configuration.t list ->
  unit -> unit
