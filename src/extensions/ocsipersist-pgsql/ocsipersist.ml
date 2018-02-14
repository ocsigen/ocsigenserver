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
  let open_connection x = Lwt_io.open_connection x
  type out_channel = Lwt_io.output_channel
  type in_channel = Lwt_io.input_channel
end
module PGOCaml = PGOCaml_generic.Make(Lwt_thread)
open Lwt
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
  PGOCaml.connect
    ?host:!host
    ?port:!port
    ?user:!user
    ?password:!password
    ?database:(Some !database)
    ?unix_domain_socket_dir:!unix_domain_socket_dir
    ()
  >>= fun dbhandle ->
  PGOCaml.set_private_data dbhandle @@ make_hashtbl ();
  Lwt.return dbhandle

let (>>) f g = f >>= fun _ -> g

let dispose db =
  Lwt.catch (fun () -> PGOCaml.close db) (fun _ -> Lwt.return_unit)

let conn_pool : (string, unit) Hashtbl.t PGOCaml.t Lwt_pool.t ref =
  (* This connection pool will be overwritten by init_fun! *)
  ref (Lwt_pool.create !size_conn_pool
         ~validate:PGOCaml.alive ~dispose connect)

let use_pool f =
  Lwt_pool.use !conn_pool @@ fun db ->
  Lwt.catch
    (fun () -> f db)
    (function
      | PGOCaml.Error msg as e ->
        Lwt_log.ign_error_f ~section "postgresql protocol error: %s" msg;
        PGOCaml.close db >>= fun () ->
        Lwt.fail e
      | Lwt.Canceled as e ->
        Lwt_log.ign_error ~section "thread canceled";
        PGOCaml.close db >>= fun () ->
        Lwt.fail e
      | e ->
        Lwt.fail e
    )

(* escapes characters that are not in the range of 0x20..0x7e;
   this is to meet PostgreSQL's format requirements for text fields
   while keeping the key column readable whenever possible. *)
let escape_string s =
  let len = String.length s in
  let buf = Buffer.create (len * 2) in
  for i = 0 to len - 1 do
    let c = s.[i] in
    let cc = Char.code c in
    if cc < 0x20 || cc > 0x7e then
      Buffer.add_string buf (sprintf "\\%03o" cc) (* non-print -> \ooo *)
    else if c = '\\' then
      Buffer.add_string buf "\\\\" (* \ -> \\ *)
    else
      Buffer.add_char buf c
  done;
  Buffer.contents buf

let unescape_string str =
  let is_first_oct_digit c = c >= '0' && c <= '3'
  and is_oct_digit c = c >= '0' && c <= '7'
  and oct_val c = Char.code c - 0x30
  in

  let len = String.length str in
  let buf = Buffer.create len in
  let i = ref 0 in
  while !i < len do
    let c = str.[!i] in
    if c = '\\' then (
      incr i;
      if !i < len && str.[!i] = '\\' then (
  Buffer.add_char buf '\\';
  incr i
      ) else if !i+2 < len &&
  is_first_oct_digit str.[!i] &&
  is_oct_digit str.[!i+1] &&
  is_oct_digit str.[!i+2] then (
    let byte = oct_val str.[!i] in
    incr i;
    let byte = (byte lsl 3) + oct_val str.[!i] in
    incr i;
    let byte = (byte lsl 3) + oct_val str.[!i] in
    incr i;
    Buffer.add_char buf (Char.chr byte)
  )
    ) else (
      incr i;
      Buffer.add_char buf c
    )
  done;
  Buffer.contents buf

type 'a parameter = Key of string | Value of 'a

let pack = function
  | Key k -> escape_string k
  | Value v -> PGOCaml.string_of_bytea @@ Marshal.to_string v []

let unpack_key = unescape_string
let unpack_value value = Marshal.from_string (PGOCaml.bytea_of_string value) 0

let key_value_of_row = function
  | [Some key; Some value] ->
      unpack_key key, unpack_value value
  | _ -> raise Ocsipersist_error

(* get one value from the result of a query *)
let one_value = function
  | [Some value]::xs -> unpack_value value
  | _ -> raise Not_found

let prepare db query =
  let hashtbl = PGOCaml.private_data db in
  (* Get a unique name for this query using an MD5 digest. *)
  let name = Digest.to_hex (Digest.string query) in
  (* Have we prepared this statement already?  If not, do so. *)
  let is_prepared = Hashtbl.mem hashtbl name in
  begin if is_prepared then Lwt.return () else begin
    PGOCaml.prepare db ~name ~query () >>
    Lwt.return @@ Hashtbl.add hashtbl name ()
  end end >>= fun () ->
  Lwt.return name

let exec db query params =
  prepare db query >>= fun name ->
  let params = List.map (fun x -> Some (pack x)) params in
  PGOCaml.execute db ~name ~params ()

let cursor db query params f =
  prepare db query >>= fun name ->
  let params = List.map (fun x -> Some (pack x)) params in
  let error = ref None in
  begin PGOCaml.cursor db ~name ~params @@ fun row ->
    Lwt.catch
      (fun () -> let key, value = key_value_of_row row in f key value)
      (fun exn ->
         Lwt_log.ign_error
           ~exn ~section "exception while evaluating cursor argument";
         error := Some exn;
         Lwt.return ()
      )
  end >>= fun () ->
  match !error with
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
  exec db query [Key name; Value default] >> Lwt.return {store; name}

let make_persistent ~store ~name ~default =
  use_pool @@ fun db -> make_persistent_worker ~store ~name ~default db

let make_persistent_lazy_lwt ~store ~name ~default = use_pool @@ fun db ->
  let query = sprintf "SELECT 1 FROM %s WHERE key = $1 " store in
  exec db query [Key name] >>= function
  | [] ->
    default () >>= fun default ->
    make_persistent_worker ~store ~name ~default db
  | _ -> Lwt.return {store = store; name = name}

let make_persistent_lazy ~store ~name ~default =
  let default () = Lwt.wrap default in
  make_persistent_lazy_lwt ~store ~name ~default

let get p = use_pool @@ fun db ->
  let query = sprintf "SELECT value FROM %s WHERE key = $1 " p.store in
  Lwt.map one_value (exec db query [Key p.name])

let set p v = use_pool @@ fun db ->
  let query = sprintf "UPDATE %s SET value = $2 WHERE key = $1 " p.store
  in exec db query [Key p.name; Value v] >> Lwt.return ()

type 'value table = string

let table_name table = Lwt.return table

let existing_tables = Hashtbl.create 16

let open_table table =
  if Hashtbl.mem existing_tables table then Lwt.return table else begin
    use_pool @@ fun db ->
    create_table db table >>= fun () ->
    Hashtbl.add existing_tables table ();
    Lwt.return table
  end

let find table key = use_pool @@ fun db ->
  let query = sprintf "SELECT value FROM %s WHERE key = $1 " table in
  Lwt.map one_value (exec db query [Key key])

let add table key value = use_pool @@ fun db ->
  let query = sprintf "INSERT INTO %s VALUES ( $1 , $2 )
                       ON CONFLICT ( key ) DO UPDATE SET value = $2 " table
  (* NOTE: incompatible with < 9.5 *)
  in exec db query [Key key; Value value] >> Lwt.return ()

let replace_if_exists table key value = use_pool @@ fun db ->
  let query = sprintf "UPDATE %s SET value = $2 WHERE key = $1 RETURNING 0" table in
  exec db query [Key key; Value value] >>= function
  | [] -> raise Not_found
  | _ -> Lwt.return ()

let remove table key = use_pool @@ fun db ->
  let query = sprintf "DELETE FROM %s WHERE key = $1 " table in
  exec db query [Key key] >> Lwt.return ()

let length table = use_pool @@ fun db ->
  let query = sprintf "SELECT count(*) FROM %s " table in
  Lwt.map one_value (exec db query [])

let iter_step f table = use_pool @@ fun db ->
  let query = sprintf "SELECT * FROM %s " table in
  cursor db query [] f

let iter_table = iter_step

let fold_step f table x =
  let res = ref x in
  let g key value =
    f key value !res >>= fun res' ->
    res := res';
    Lwt.return ()
  in iter_step g table >> Lwt.return !res

let fold_table = fold_step

let iter_block a b = failwith "Ocsipersist.iter_block: not implemented"


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

let _ = Ocsigen_extensions.register_extension ~name:"ocsipersist" ~init_fun ()
