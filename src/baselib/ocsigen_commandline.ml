(* Ocsigen
 * Copyright (C) 2005 Vincent Balat
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

open Ocsigen_config

let cmdline : unit = 
  try
    Arg.parse_argv Ocsigen_getcommandline.commandline
      [("-c", Arg.String set_configfile,
        "Alternate config file (default "^ Ocsigen_config.get_config_file() ^")");
       ("--config", Arg.String set_configfile,
        "Alternate config file (default "^ Ocsigen_config.get_config_file() ^")");
       ("-s", Arg.Unit set_silent, "silent mode (no logging to console; does not affect *.log files)");
       ("--silent", Arg.Unit set_silent, "silent mode (no logging to console; does not affect *.log files)");
       ("-p", Arg.String set_pidfile, "Specify a file where to write the PIDs of servers");
       ("--pidfile", Arg.String set_pidfile, "Specify a file where to write the PIDs of servers");
       ("-v", Arg.Unit set_verbose, "Verbose mode (notice)");
       ("--verbose", Arg.Unit set_verbose, "Verbose mode (notice)");
       ("-vv", Arg.Unit set_veryverbose, "Very verbose mode (info)");
       ("--veryverbose", Arg.Unit set_veryverbose, "Very verbose mode (info)");
       ("-vvv", Arg.Unit set_debug, "Extremely verbose mode (info)");
       ("--debug", Arg.Unit set_debug, "Extremely verbose mode (debug)");
       ("-d", Arg.Unit set_daemon, "Daemon mode (detach the process)");
       ("--daemon", Arg.Unit set_daemon, "Daemon mode (detach the process) (This is the default when there are more than 1 process)");
       ("--version", Arg.Unit display_version, "Display version number and exit")
      ]
      (fun _ -> ())
      "usage: ocsigen [-c configfile]"
  with
  | Arg.Help s -> print_string s; exit 0
  | Arg.Bad s -> prerr_string s; exit 1
