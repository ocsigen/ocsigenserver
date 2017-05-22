(** PostgreSQL (>= 9.5) backend for Ocsipersist. *)

let section = Lwt_log.Section.make "ocsipersist:pgsql"

module Lwt_thread = struct
  include Lwt
  let close_in = Lwt_io.close
  let really_input = Lwt_io.read_into_exactly
  let input_binary_int = Lwt_io.BE.read_int
  let input_char = Lwt_io.read_char
  let output_string = Lwt_io.write
  let output_binary_int = Lwt_io.BE.write_int
  let output_char = Lwt_io.write_char
  let flush = Lwt_io.flush
  let open_connection a = Lwt_io.open_connection a
  type in_channel = Lwt_io.input_channel
  type out_channel = Lwt_io.output_channel
end
module PGOCaml = PGOCaml_generic.Make(Lwt_thread)
open Lwt.Infix
open Printf

exception Ocsipersist_error

let host = ref None
let port = ref None
let user = ref None
let password = ref None
let database = ref "ocsipersist"
let unix_domain_socket_dir = ref None
let size_conn_pool = ref 16

let make_hashtbl () = Hashtbl.create 8

let connect () =
  lwt dbhandle = PGOCaml.connect
                   ?host:!host
                   ?port:!port
                   ?user:!user
                   ?password:!password
                   ?database:(Some !database)
                   ?unix_domain_socket_dir:!unix_domain_socket_dir
                   () in
  PGOCaml.set_private_data dbhandle @@ make_hashtbl ();
  Lwt.return dbhandle

let (>>) f g = f >>= fun _ -> g

let conn_pool : (string, unit) Hashtbl.t PGOCaml.t Lwt_pool.t ref =
  (* This connection pool will be overwritten by init_fun! *)
  ref @@ Lwt_pool.create !size_conn_pool ~validate:PGOCaml.alive connect

let use_pool f = Lwt_pool.use !conn_pool @@ fun db -> f db

let key_value_of_row = function
  | [Some key; Some value] -> (PGOCaml.bytea_of_string key, PGOCaml.bytea_of_string value)
  | _ -> raise Ocsipersist_error

(* get one value from the result of a query *)
let one = function
  | [Some value]::xs -> PGOCaml.bytea_of_string value
  | _ -> raise Not_found

let marshal value = Marshal.to_string value []
let unmarshal str = Marshal.from_string str 0

let prepare db query =
  let hashtbl = PGOCaml.private_data db in
  (* Get a unique name for this query using an MD5 digest. *)
  let name = Digest.to_hex (Digest.string query) in
  (* Have we prepared this statement already?  If not, do so. *)
  let is_prepared = Hashtbl.mem hashtbl name in
  lwt () = if is_prepared then Lwt.return () else begin
    PGOCaml.prepare db ~name ~query () >>
    Lwt.return @@ Hashtbl.add hashtbl name ()
  end in
  Lwt.return name

let exec db query params =
  lwt name = prepare db query in
  let params = params |> List.map @@ fun x -> Some (PGOCaml.string_of_bytea x) in
  PGOCaml.execute db ~name ~params ()

let cursor db query params f =
  lwt name = prepare db query in
  let params = params |> List.map @@ fun x -> Some (PGOCaml.string_of_bytea x) in
  let error = ref None in
  lwt () = PGOCaml.cursor db ~name ~params @@ fun row -> try_lwt
      let (key,value) = key_value_of_row row in
      f key (unmarshal value)
    with exn ->
      Lwt_log.ign_error ~exn ~section
        "exception while evaluating cursor argument";
      error := Some exn;
      Lwt.return ()
  in match !error with
    | None -> Lwt.return ()
    | Some e -> Lwt.fail e

let (@.) f g = fun x -> f (g x) (* function composition *)

let create_table db table =
  let query = sprintf "CREATE TABLE IF NOT EXISTS %s \
                       (key TEXT, value BYTEA, PRIMARY KEY(key))" table
  in exec db query [] >> Lwt.return ()


type store = string

type 'a t = {
  store : string;
  name  : string;
}

let open_store store = use_pool @@ fun db ->
  create_table db store >> Lwt.return store

let make_persistent_worker ~store ~name ~default db =
  let query = sprintf "INSERT INTO %s VALUES ( $1 , $2 )
                       ON CONFLICT ( key ) DO NOTHING" store in
  (* NOTE: incompatible with < 9.5 *)
  exec db query [name; marshal default] >> Lwt.return {store; name}

let make_persistent ~store ~name ~default =
  use_pool @@ fun db -> make_persistent_worker ~store ~name ~default db

let make_persistent_lazy_lwt ~store ~name ~default = use_pool @@ fun db ->
  let query = sprintf "SELECT 1 FROM %s WHERE key = $1 " store in
  lwt result = exec db query [name] in
  match result with
  | [] ->
    lwt default = default () in
    make_persistent_worker ~store ~name ~default db
  | _ -> Lwt.return {store = store; name = name}

let make_persistent_lazy ~store ~name ~default =
  let default () = Lwt.wrap default in
  make_persistent_lazy_lwt ~store ~name ~default

let get p = use_pool @@ fun db ->
  let query = sprintf "SELECT value FROM %s WHERE key = $1 " p.store in
  Lwt.map (unmarshal @. one) (exec db query [p.name])

let set p v = use_pool @@ fun db ->
  let query = sprintf "UPDATE %s SET value = $2 WHERE key = $1 " p.store
  in exec db query [p.name; marshal v] >> Lwt.return ()

type 'value table = string

let table_name table = Lwt.return table

let open_table table = use_pool @@ fun db ->
  create_table db table >> Lwt.return table

let find table key = use_pool @@ fun db ->
  let query = sprintf "SELECT value FROM %s WHERE key = $1 " table in
  Lwt.map (unmarshal @. one) (exec db query [key])

let add table key value = use_pool @@ fun db ->
  let query = sprintf "INSERT INTO %s VALUES ( $1 , $2 )
                       ON CONFLICT ( key ) DO UPDATE SET value = $2 " table
  (* NOTE: incompatible with < 9.5 *)
  in exec db query [key; marshal value] >> Lwt.return ()

let replace_if_exists table key value = use_pool @@ fun db ->
  let query = sprintf "UPDATE %s SET value = $2 WHERE key = $1 RETURNING 0" table in
  lwt result = exec db query [key; marshal value] in
  match result with
  | [] -> raise Not_found
  | _ -> Lwt.return ()

let remove table key = use_pool @@ fun db ->
  let query = sprintf "DELETE FROM %s WHERE key = $1 " table in
  exec db query [key] >> Lwt.return ()

let length table = use_pool @@ fun db ->
  let query = sprintf "SELECT count(*) FROM %s " table in
  Lwt.map (unmarshal @. one) (exec db query [])

let iter_step f table = use_pool @@ fun db ->
  let query = sprintf "SELECT * FROM %s " table in
  cursor db query [] f

let iter_table = iter_step

let fold_step f table x =
  let res = ref x in
  let g key value =
    lwt res' = f key value !res in
    res := res';
    Lwt.return ()
  in iter_step g table >> Lwt.return !res

let fold_table = fold_step

let iter_block a b = failwith "Ocsipersist.iter_block: not implemented"

let parse_global_config = function
  | [] -> ()
  | [Xml.Element ("database", attrs, [])] -> let parse_attr = function
    | ("host", h) -> host := Some h
    | ("port", p) -> begin
        try port := Some (int_of_string p)
        with Failure _ -> raise @@ Ocsigen_extensions.Error_in_config_file
                                     "port is not an integer"
      end
    | ("user", u) -> user := Some u
    | ("password", pw) -> password := Some pw
    | ("database", db) -> database := db
    | ("unix_domain_socket_dir", udsd) -> unix_domain_socket_dir := Some udsd
    | ("size_conn_pool", scp) -> begin
        try size_conn_pool := int_of_string scp
        with Failure _ -> raise @@ Ocsigen_extensions.Error_in_config_file
                                     "size_conn_pool is not an integer"
      end
    | _ -> raise @@ Ocsigen_extensions.Error_in_config_file
                      "Unexpected attribute for <database> in Ocsipersist config"
    in ignore @@ List.map parse_attr attrs; ()
  | _ -> raise @@ Ocsigen_extensions.Error_in_config_file
                    "Unexpected content inside Ocsipersist config"


let init_fun config =
  parse_global_config config;
  conn_pool := Lwt_pool.create !size_conn_pool ~validate:PGOCaml.alive connect

let _ = Ocsigen_extensions.register ~name:"ocsipersist" ~init_fun ()
