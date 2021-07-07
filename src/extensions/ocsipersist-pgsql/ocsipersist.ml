(** PostgreSQL (>= 9.5) backend for Ocsipersist. *)

let section = Lwt_log.Section.make "ocsigen:ocsipersist:pgsql"

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

let rec list_last l =
  match l with
  | [x] -> x
  | _ :: r -> list_last r
  | [] -> raise Not_found

(* get one value from the result of a query *)
let one_value = function
  | [Some value]::_xs -> unpack_value value
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

(* FUNCTORIAL INTERFACE *******************************************************)

module type TABLE_CONF = sig
  val name : string
end

type internal = string

module type COLUMN = sig
  type t
  val column_type : string
  val encode : t -> internal
  val decode : internal -> t
end

module type TABLE = sig
  type key
  type value

  val name : string
  val find : key -> value Lwt.t
  val add : key -> value -> unit Lwt.t
  val replace_if_exists : key -> value -> unit Lwt.t
  val remove : key -> unit Lwt.t
  val modify_opt : key -> (value option -> value option) -> unit Lwt.t
  val length : unit -> int Lwt.t
  val iter :
    ?count:int64 ->
    ?gt:key -> ?geq:key -> ?lt:key -> ?leq:key ->
    (key -> value -> unit Lwt.t) -> unit Lwt.t
  val fold :
    ?count:int64 ->
    ?gt:key -> ?geq:key -> ?lt:key -> ?leq:key ->
    (key -> value -> 'a -> 'a Lwt.t) -> 'a -> 'a Lwt.t
  val iter_block :
    ?count:int64 ->
    ?gt:key -> ?geq:key -> ?lt:key -> ?leq:key ->
    (key -> value -> unit) -> unit Lwt.t
end

module Table (T : TABLE_CONF) (Key : COLUMN) (Value : COLUMN)
       : TABLE with type key = Key.t and type value = Value.t = struct
  type key = Key.t
  type value = Value.t
  let name = T.name

  module Aux = struct
    let exec_opt db query params =
      prepare db query >>= fun name ->
      PGOCaml.execute db ~name ~params ()

    let exec db query params =
      prepare db query >>= fun name ->
      let params = List.map (fun x -> Some x) params in
      PGOCaml.execute db ~name ~params ()

    let exec_ db query params = exec db query params >> Lwt.return_unit

    let encode_pair key value = [Key.encode key; Value.encode value]
  end

  let init =
    let create_table table db =
      let query =
        sprintf "CREATE TABLE IF NOT EXISTS %s \
                 (key %s, value %s, PRIMARY KEY (key))"
                table Key.column_type Value.column_type
      in Aux.exec_ db query []
    in
    lazy (use_pool @@ create_table T.name)

  let with_table f = Lazy.force init >>= fun () -> use_pool f

  let find key = with_table @@ fun db ->
    let query = sprintf "SELECT value FROM %s WHERE key = $1 " name in
    Aux.exec db query [Key.encode key] >>=
    function
    | [Some value]::_ -> Lwt.return (Value.decode value)
    | _ -> Lwt.fail Not_found

  let add key value = with_table @@ fun db ->
    let query = sprintf "INSERT INTO %s VALUES ($1, $2)
                         ON CONFLICT (key) DO UPDATE SET value = $2" name
    in Aux.exec_ db query @@ Aux.encode_pair key value

  let replace_if_exists key value = with_table @@ fun db ->
    let query = sprintf "UPDATE %s SET value = $2 WHERE key = $1 RETURNING 0" name in
    Aux.exec db query (Aux.encode_pair key value) >>= function
    | [] -> raise Not_found
    | _ -> Lwt.return_unit

  let remove key = with_table @@ fun db ->
    let query = sprintf "DELETE FROM %s WHERE key = $1" name in
    Aux.exec_ db query [Key.encode key]

  let modify_opt key f = with_table @@ fun db ->
    let query = sprintf "SELECT value FROM %s WHERE key = $1" name in
    Aux.exec db query [Key.encode key] >>= fun value ->
    let old_value = match value with
      | [Some v]::_ -> Some (Value.decode v)
      | _ -> None
    in
    match f old_value with
    | Some new_value ->
        let query = sprintf "INSERT INTO %s VALUES ($1, $2)
                             ON CONFLICT (key) DO UPDATE SET value = $2" name
        in Aux.exec_ db query @@ Aux.encode_pair key new_value
    | None ->
        let query = sprintf "DELETE FROM %s WHERE key = $1" name in
        Aux.exec_ db query [Key.encode key]

  let length () = with_table @@ fun db ->
    let query = sprintf "SELECT count (1) FROM %s" name in
    Lwt.map one_value @@ Aux.exec db query []

  let max_iter_block_size = 1000L

  let rec iter_rec ?count ?gt ?geq ?lt ?leq f last =
    match count with Some c when c <= 0L -> Lwt.return_unit | _ ->
    let key_value_of_row = function
      | [Some key; Some value] -> Key.decode key, Value.decode value
      | _ -> raise Ocsipersist_error
    in
    let query = sprintf "SELECT * FROM %s
                         WHERE ($1 :: %s IS NULL OR key > $1)
                         AND ($2 :: %s IS NULL OR key > $2)
                         AND ($3 :: %s IS NULL OR key >= $3)
                         AND ($4 :: %s IS NULL OR key < $4)
                         AND ($5 :: %s IS NULL OR key <= $5)
                         ORDER BY key LIMIT $6"
                         name Key.column_type Key.column_type
                         Key.column_type Key.column_type Key.column_type
    and args =
      let count = match count with
        | Some c when c <= max_iter_block_size -> c
        | _ -> max_iter_block_size
      in
      [Option.map Key.encode last;
       Option.map Key.encode gt;
       Option.map Key.encode geq;
       Option.map Key.encode lt;
       Option.map Key.encode leq;
       Some (Int64.to_string count)]
    in
    with_table (fun db -> Aux.exec_opt db query args) >>= fun l ->
    Lwt_list.iter_s (fun row -> let k, v = key_value_of_row row in f k v) l
    >>= fun () ->
    if l = [] then
      Lwt.return_unit
    else
      let last, _ = key_value_of_row @@ list_last l in
      let count =
        Option.map Int64.(fun c -> sub c @@ of_int @@ List.length l) count
      in
      iter_rec ?count f ?gt ?geq ?lt ?leq (Some last)

  let iter ?count ?gt ?geq ?lt ?leq f =
    iter_rec ?count ?gt ?geq ?lt ?leq f None

  let fold ?count ?gt ?geq ?lt ?leq f x =
    let res = ref x in
    let g key value =
      f key value !res >>= fun res' ->
      res := res';
      Lwt.return_unit
    in iter ?count ?gt ?geq ?lt ?leq g >> Lwt.return !res

  let iter_block ?count:_ ?gt:_ ?geq:_ ?lt:_ ?leq:_ _ =
    failwith "Ocsipersist.iter_block: not implemented"
end

module Column = struct
  module String : COLUMN with type t = string = struct
    type t = string
    let column_type = "text"
    let encode = escape_string
    let decode = unescape_string
  end

  module Float : COLUMN with type t = float = struct
    type t = float
    let column_type = "float"
    let encode = PGOCaml.string_of_float
    let decode = PGOCaml.float_of_string
  end

  module Marshal (C : sig type t end) : COLUMN with type t = C.t = struct
    type t = C.t
    let column_type = "bytea"
    let encode v = PGOCaml.string_of_bytea @@ Marshal.to_string v []
    let decode v = Marshal.from_string (PGOCaml.bytea_of_string v) 0
  end
end

(******************************************************************************)

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

let rec iter_rec f table last =
  let (query, args) =
    match last with
    | None ->
      (sprintf "SELECT * FROM %s ORDER BY key LIMIT 1000" table, [])
    | Some last ->
      (sprintf "SELECT * FROM %s WHERE key > $1 ORDER BY key LIMIT 1000" table,
       [Key last])
  in
  (use_pool @@ fun db -> exec db query args) >>= fun l ->
  Lwt_list.iter_s
    (fun row -> let key, value = key_value_of_row row in f key value) l
  >>= fun () ->
  if l = [] then
    Lwt.return_unit
  else
    let last, _ = key_value_of_row (list_last l) in
    iter_rec f table (Some last)

let iter_step f table = iter_rec f table None

let iter_table = iter_step

let fold_step f table x =
  let res = ref x in
  let g key value =
    f key value !res >>= fun res' ->
    res := res';
    Lwt.return ()
  in iter_step g table >> Lwt.return !res

let fold_table = fold_step

let iter_block _a _b = failwith "Ocsipersist.iter_block: not implemented"

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
