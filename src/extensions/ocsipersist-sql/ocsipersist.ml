let section = Lwt_log.Section.make "ocsipersist:sql"

module Lwt_thread = struct
  include Lwt
  include Lwt_chan
end
module Lwt_PGOCaml = PGOCaml_generic.Make(Lwt_thread)
module Lwt_Query = Query.Make_with_Db(Lwt_thread)(Lwt_PGOCaml)
open Lwt

type 'a t = string * 'a

type store = string Lwt_PGOCaml.t Lwt_PGOCaml.monad

let host = ref "localhost"
let port = ref 3000

let open_store db = Lwt_PGOCaml.connect ~host:!host ~port:!port ~database:db ()

let make_persistent ~store ~name ~default = failwith "TODO"

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
let parse_global_config (host, port) = function
  | [] -> (None, None)
  | (Element ("database", attrs, []))::[] ->
    let rec parse_attrs (host, port) = function
      | ("host", h) :: xs -> parse_attrs (Some h, port) xs
      | ("port", p) :: xs -> begin try
          parse_attrs (host, Some (int_of_string p)) xs
        with Failure _ -> raise (Ocsigen_extensions.Error_in_config_file
                                   ("port is not an integer"))
        end
      | [] -> (host, port)
      | _ -> raise (Ocsigen_extensions.Error_in_config_file
                      ("Unexpected content inside Ocsipersist config"))
    in parse_attrs (host, port) attrs
  | _ -> raise (Ocsigen_extensions.Error_in_config_file
                  ("Unexpected content inside Ocsipersist config"))


let init_fun config =
  let maybe m f = match m with | None -> () | Some x -> f x in
  let (h, p) = parse_global_config (None, None) config in
  maybe h (fun h -> host := h);
  maybe p (fun p -> port := p)


let _ = Ocsigen_extensions.register_extension ~name:"ocsipersist" ~init_fun ()
