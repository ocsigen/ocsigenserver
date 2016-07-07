let section = Lwt_log.Section.make "ocsipersist:sql"

module Lwt_thread = struct
  include Lwt
  include Lwt_chan
end
module Lwt_PGOCaml = PGOCaml_generic.Make(Lwt_thread)
module Lwt_Query = Query.Make_with_Db(Lwt_thread)(Lwt_PGOCaml)
module PGOCaml = Lwt_PGOCaml
open Lwt
open Printf

let host = ref None
let port = ref None
let user = ref None
let password = ref None
let database = ref None
let unix_domain_socket_dir = ref None

let connect = Lwt_PGOCaml.connect
                ?host:!host
                ?port:!port
                ?user:!user
                ?password:!password
                ?database:!database
                ?unix_domain_socket_dir:!unix_domain_socket_dir

let (>>) f g = f >>= fun _ -> g

let transaction_block db f =
  Lwt_PGOCaml.begin_work db >>= fun _ ->
  try_lwt
    lwt r = f () in
    Lwt_PGOCaml.commit db >>
    Lwt.return r
  with e ->
    Lwt_PGOCaml.rollback db >>
    Lwt.fail e

let pool : (string, bool) Hashtbl.t Lwt_PGOCaml.t Lwt_pool.t =
  Lwt_pool.create 16 ~validate:PGOCaml.alive connect

(* same as full_transaction_block from Eba_db *)
let full_transaction_block f =
  Lwt_pool.use pool (fun db -> transaction_block db (fun () -> f db))

let exec db query params =
  PGOCaml.prepare db ~query () >>
  PGOCaml.execute db ~params:(List.map (fun x -> Some x) params) ()

let (@.) f g = fun x -> f (g x) (* function composition *)

(* get one value from the result of a query *)
let one = function
  | [key; Some value] :: _ -> value
  | _ -> raise Not_found

let unmarshal str = Marshal.from_string str 0

let create_table db table =
  let query = sprintf "CREATE TABLE IF NOT EXISTS %s (key TEXT, value BLOB,  PRIMARY KEY(key) ON CONFLICT REPLACE)" table in
  exec db query [] >>
  Lwt.return ()

(* TODO: risk of SQL injections via the store name? *)
type store = string

type 'a t = {
  store : string;
  name  : string;
  value : 'a
}

let open_store store = full_transaction_block @@ fun db ->
  create_table db store >> Lwt.return store

let make_persistent ~store ~name ~default = full_transaction_block @@ fun db ->
  let query = sprintf "INSERT INTO %s VALUES ( $1 , $2 )" store in
  exec db query [name; Marshal.to_string default []] >>
  Lwt.return {store = store; name = name; value = default}

let make_persistent_lazy ~store ~name ~default = failwith "TODO"

let make_persistent_lazy_lwt ~store ~name ~default = failwith "TODO"

let get p = full_transaction_block @@ fun db ->
  let query = sprintf "SELECT value FROM %s WHERE key = $1 " p.store in
  Lwt.map (unmarshal @. one) (exec db query [p.name])

let set p v = full_transaction_block @@ fun db ->
  let query = sprintf "INSERT INTO %s VALUES ( $1 , $2 )" p.store in
  exec db query [p.name; Marshal.to_string v []] >>
  Lwt.return ()

type 'value table = string

let table_name table = Lwt.return table

let open_table table = full_transaction_block @@ fun db ->
  create_table db table >> Lwt.return table

let find table key = full_transaction_block @@ fun db ->
  let query = sprintf "SELECT value FROM %s WHERE key = $1 " table in
  Lwt.map (unmarshal @. one) (exec db query [key])

let add table key value = full_transaction_block @@ fun db ->
  let query = sprintf "INSERT INTO %s VALUES ( $1 , $2 )" table in
  exec db query [key; Marshal.to_string value []] >>
  Lwt.return ()

let replace_if_exists table key value =
  try_lwt
    find table key >> add table key value
  with Not_found -> Lwt.return ()

let remove table key = full_transaction_block @@ fun db ->
  let query = sprintf "DELETE FROM %s WHERE key = $1 " table in
  exec db query [key] >> Lwt.return ()

let length table = full_transaction_block @@ fun db ->
  let query = sprintf "SELECT count(*) FROM %s " table in
  Lwt.map (unmarshal @. one) (exec db query [])

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
    | ("user", u) -> user := Some u
    | ("password", pw) -> password := Some pw
    | ("database", db) -> database := Some db
    | ("unix_domain_socket_dir", udsd) -> unix_domain_socket_dir := Some udsd
    | _ -> raise @@ Ocsigen_extensions.Error_in_config_file
                      "Unexpected content inside Ocsipersist config"
    in ignore @@ List.map parse_attr attrs; ()
  | _ -> raise @@ Ocsigen_extensions.Error_in_config_file
                    "Unexpected content inside Ocsipersist config"


let init_fun config = parse_global_config config


let _ = Ocsigen_extensions.register_extension ~name:"ocsipersist" ~init_fun ()
