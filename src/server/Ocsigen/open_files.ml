external limits : unit -> (int * int) option = "ocsigen_open_files_limits"

external raise_soft_limit :
   int
  -> (int * int) option
  = "ocsigen_open_files_raise"
