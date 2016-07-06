let section = Lwt_log.Section.make "ocsipersist:sql"

module Lwt_thread = struct
  include Lwt
  include Lwt_chan
end
module Lwt_PGOCaml = PGOCaml_generic.Make(Lwt_thread)
module Lwt_Query = Query.Make_with_Db(Lwt_thread)(Lwt_PGOCaml)
module PGOCaml = Lwt_PGOCaml
open Lwt

type 'a t = string * 'a

type store = string

let host = ref None
let port = ref None
let db   = ref None

let connect = Lwt_PGOCaml.connect ?host:!host ?port:!port ?database:!db ()

let open_store table = table

let make_persistent ~store ~name ~default = failwith "TODO"
  (* PGSQL(store) "SELECT a FROM b" *)

let make_persistent_lazy ~store ~name ~default = failwith "TODO"

let make_persistent_lazy_lwt ~store ~name ~default = failwith "TODO"

let get = failwith "TODO"

let set = failwith "TODO"

type 'value table = string Lwt.t

let table_name = failwith "TODO"

let open_table = failwith "TODO"

let find = failwith "TODO"

let add = failwith "TODO"

let replace_if_exists = failwith "TODO"

let remove = failwith "TODO"

let length = failwith "TODO"

let iter_step = failwith "TODO"

let iter_table = failwith "TODO"

let fold_step = failwith "TODO"

let fold_table = failwith "TODO"


let iter_block = failwith "TODO"

(*exception Ocsipersist_error*)


open Simplexmlparser
let parse_global_config = function
  | [] -> ()
  | [Element ("database", attrs, [])] -> let parse_attr = function
    | ("host", h) -> host := Some h
    | ("port", p) -> begin
        try port := Some (int_of_string p)
        with Failure _ -> raise @@ Ocsigen_extensions.Error_in_config_file
                                     "port is not an integer"
      end
    | ("database", d) -> db := Some d
    | _ -> raise @@ Ocsigen_extensions.Error_in_config_file
                      "Unexpected content inside Ocsipersist config"
    in ignore @@ List.map parse_attr attrs; ()
  | _ -> raise @@ Ocsigen_extensions.Error_in_config_file
                    "Unexpected content inside Ocsipersist config"


let init_fun config = parse_global_config config


let _ = Ocsigen_extensions.register_extension ~name:"ocsipersist" ~init_fun ()
