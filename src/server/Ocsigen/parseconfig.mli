(* Ocsigen
 * http://www.ocsigen.org
 * Module ocsigen_parseconfig.ml
 * Copyright (C) 2005 Vincent Balat, Nataliya Guts
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

(** Config file parsing. See also module
    {!Ocsigen.Extensions.Configuration} *)

(**/**)

val section : Logs.src

val parse_size_tag : string -> string -> int64 option
(** [parse_size_tag tag s] parses a size.

    The size can be either "infinity" or use SI or binary units, e.g.,
    10 10B 10o 10ko 10kB 10kiB 10MiB 10TB ... .

    In case of error, raises [Config.Config_file_error m]
    where [m] is an error message explaining that a size was expected
    in tag [<tag>]. *)

val first_pass : Xml.xml list -> unit
(** Extracts (and stores via Config) the following information:
    {ul
    {- user to execute OcsigenServer (ex: www-data) }
    {- group to execute OcsigenServer (ex: www-data) }
    {- SSL key, SSL certificate, SSL ciphers list,
       SSL DH file, SSL EC curve }
    {- list of HTTP port to listen on (ex: 80) }
    {- list of HTTPS port to listen on (ex: 443) }
    {- minimum and maximum number of threads }
    }
    To be called early by [Server].
*)

val later_pass : Xml.xml list -> unit

val parse_xml_file : string -> Xml.xml
(** [parse_xml_file file] is the XML document in [file]. An XML or DTD error
    raises [Config.Config_file_error], with the name of [file] and the position
    of the error. [Xml_light_errors.File_not_found file] is left to the caller,
    as a missing file is not always an error. *)

(**/**)

val parse_config : ?file:string -> unit -> Xml.xml list list
(** Returns the config file. Use this if you want to read a config file from
    your own executable. See {!Server.exec}. Raises
    [Config.Config_file_error] on an error, such as a file that cannot be
    opened or is not well-formed XML. *)
