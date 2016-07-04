let section = Lwt_log.Section.make "ocsipersist:sql"

(** Module Ocsipersist: persistent data *)

open Lwt
open Printf

module Lwt_thread = struct
  include Lwt
  include Lwt_chan
end
module Lwt_PGOCaml = PGOCaml_generic.Make(Lwt_thread)
module Lwt_Query = Query.Make_with_Db(Lwt_thread)(Lwt_PGOCaml)


type store = string Lwt.t (*TODO*)

exception Ocsipersist_error


open Simplexmlparser
let parse_global_config (host, port, db) = function
  | [] -> (None, None, None)
  | (Element ("database", attrs, []))::[] ->
    let rec parse_attrs (host, port, db) = function
      | ("host", h) :: xs -> parse_attrs (Some h, port, db) xs
      | ("port", p) :: xs -> begin try
          parse_attrs (host, Some (int_of_string p), db) xs
        with Failure _ -> raise (Ocsigen_extensions.Error_in_config_file
                                   ("port is not an integer"))
        end
      | ("db",  db) :: xs -> parse_attrs (host, port, Some db) xs
      | [] -> (host, port, db)
      | _ -> raise (Ocsigen_extensions.Error_in_config_file
                      ("Unexpected content inside Ocsipersist config"))
    in parse_attrs (host, port, db) attrs
  | _ -> raise (Ocsigen_extensions.Error_in_config_file
                  ("Unexpected content inside Ocsipersist config"))


(* Registration of the extension *)

let dbh = ref (failwith "undefined" : string Lwt_PGOCaml.t Lwt_PGOCaml.monad)

let init config =
  let default maybe value = match maybe with | None -> value | Some x -> x in
  let (host, port, db) = parse_global_config (None, None, None) config in
  let host = default host "localhost" in
  let port = default port 3000 in
  let db   = default db   "ocsipersist" in
  dbh := Lwt_PGOCaml.connect ~host ~port ~database:db ();
  ()


let _ = Ocsigen_extensions.register_extension
    ~name:"ocsipersist"
    ~init_fun:init
    ()
