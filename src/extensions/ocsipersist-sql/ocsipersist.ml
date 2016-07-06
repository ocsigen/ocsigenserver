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

let db_handle = ref None

let do_if cond actions = if cond then actions else Lwt.return ()

(* ensure that connection to database is alive; throws exception otherwise. *)
let with_db action =
  let%lwt () = do_if (!db_handle = None) begin
    let%lwt db = connect () in
    db_handle := Some db;
    Lwt.return ()
  end in
  let%lwt () = match !db_handle with
    | None -> failwith "forgot to use with_db?"
    | Some db ->
      let%lwt alive = PGOCaml.alive db in
      do_if (not alive) begin
      let%lwt db = connect () in
      db_handle := Some db;
      PGOCaml.ping db
    end
  in action

let transaction_block db f = (* copied from Eba_db *)
  Lwt_PGOCaml.begin_work db >>= fun _ ->
  try%lwt
    let%lwt r = f () in
    let%lwt () = Lwt_PGOCaml.commit db in
    Lwt.return r
  with e ->
    let%lwt () = Lwt_PGOCaml.rollback db in
    Lwt.fail e

(* copied from Eba_db *)
let pool : (string, bool) Hashtbl.t Lwt_PGOCaml.t Lwt_pool.t =
  Lwt_pool.create 16 ~validate:PGOCaml.alive connect

let full_transaction_block f = (* copied from Eba_db *)
  Lwt_pool.use pool (fun db -> transaction_block db (fun () -> f db))


let open_store table = table

let make_persistent ~store ~name ~default = full_transaction_block @@ fun dbh ->
	failwith "TODO"
	(* PGSQL(store) "UPDATE $store SET $name = $default" *)

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
