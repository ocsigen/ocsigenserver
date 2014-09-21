(* Ocsigen
 * http://www.ocsigen.org
 * Module ocsidbm.ml
 * Copyright (C) 2007 Vincent Balat
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


(** Module Ocsidbm: persistent data server for Ocsigen *)

open Dbm
open Ocsidbmtypes
open Lwt

let directory = Sys.argv.(1)

exception Ocsidbm_error

let socketname = "socket"
let suffix = ".otbl"

(*****************************************************************************)
(* error messages *)
let errlog s =
  let date =
    let t = Unix.localtime (Unix.time ()) in
    Printf.sprintf
      "%02d-%02d-%04d %02d:%02d:%02d"
      t.Unix.tm_mday
      (t.Unix.tm_mon + 1)
      (1900 + t.Unix.tm_year)
      t.Unix.tm_hour
      t.Unix.tm_min
      t.Unix.tm_sec
  in
  let s = date^" Ocsidbm - "^s^"\n" in
  prerr_endline s



(*****************************************************************************)
(** Internal functions: storage in files using DBM *)

module Tableoftables = Map.Make(struct
    type t = string
    let compare = compare
  end)

let tableoftables = ref Tableoftables.empty

let list_tables () =
  let d =
    try
      Unix.opendir directory
    with
    | Unix.Unix_error(error,_,_) ->
      failwith ( Printf.sprintf "Ocsidbm: can't open directory  %s: %s"
                   directory (Unix.error_message error))
  in
  let rec aux () =
    try
      let n = Unix.readdir d in
      if Filename.check_suffix n suffix
      then (Filename.chop_extension n)::(aux ())
      else if Filename.check_suffix n (suffix^".pag")
      (* depending on the version of dbm, there may be a .pag suffix *)
      then (Filename.chop_extension (Filename.chop_extension n))::(aux ())
      else aux ()
    with End_of_file -> Unix.closedir d; []
  in aux ()

(* try to create the directory if it does not exist *)
let _ =
  try
    Unix.access directory [Unix.R_OK; Unix.W_OK; Unix.X_OK; Unix.F_OK]
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) ->
    begin
      try
        Unix.mkdir directory 0o750
      with
      | Unix.Unix_error(error,_,_) ->
        failwith ( Printf.sprintf "Ocsidbm: can't create directory %s: %s"
                     directory (Unix.error_message error) )
    end
  | Unix.Unix_error(error,_,_) ->
    failwith ( Printf.sprintf "Ocsidbm: can't access directory %s: %s"
                 directory (Unix.error_message error) )


let open_db name =
  let t = opendbm (directory^"/"^name^suffix) [Dbm_rdwr; Dbm_create] 0o640 in
  tableoftables := Tableoftables.add name t !tableoftables;
  t

let open_db_if_exists name =
  try
    let t = opendbm (directory^"/"^name^suffix) [Dbm_rdwr] 0o640 in
    tableoftables := Tableoftables.add name t !tableoftables;
    t
  with
  | Unix.Unix_error (Unix.ENOENT, _, _)
  | Dbm.Dbm_error _ -> raise Not_found

(* open all files and register them in the table of tables *)
(*
   let _ = List.iter (fun a ->
   try ignore (open_db a)
   with ... -> errlog ("Error while openning database "^a))
    (list_tables ())
si je remets �a, �a doit �tre apr�s la cr�ation de la socket
car si je n'arrive pas � cr�er la socket,
c'est peut-�tre que les tables sont d�j� ouvertes
*)

let find_create_table name =
  try
    Tableoftables.find name !tableoftables
  with Not_found -> open_db name

let find_dont_create_table name =
  try
    Tableoftables.find name !tableoftables
  with Not_found -> open_db_if_exists name

let db_get store name =
  find (find_dont_create_table store) name

let db_remove store name =
  try
    remove (find_dont_create_table store) name
  with
  | Not_found -> ()
  | Dbm.Dbm_error "dbm_delete" -> ()

let db_replace store name value =
  replace (find_create_table store) name value

let db_firstkey t = Dbm.firstkey (find_dont_create_table t)

let db_nextkey t = Dbm.nextkey (find_dont_create_table t)

let db_length t =
  let table = find_dont_create_table t in
  let rec aux f n =
    catch
      (fun () ->
         ignore (f table);
         Lwt_unix.yield () >>=
         (fun () -> aux Dbm.nextkey (n+1)))
      (function
        | Not_found -> return n
        | e -> fail e)
  in
  aux Dbm.firstkey 0
(* Because of Dbm implementation, the result may be less than the expected
   result in some case *)

(*****************************************************************************)
(* signals *)
let close_all i _ =
  Unix.unlink (directory^"/"^socketname);
  Tableoftables.iter (fun k t -> Dbm.close t) !tableoftables;
  exit i

let the_end i =
  exit i

open Sys
let sigs = [sigabrt;sigalrm;sigfpe;sighup;sigill;sigint;
            sigquit;sigsegv;sigterm;sigusr1;sigusr2;
            sigchld;sigttin;sigttou;sigvtalrm;sigprof]

let _ =
  List.iter (fun s ->
      Sys.set_signal s (Signal_handle (close_all 0))) sigs


let _ = Sys.set_signal Sys.sigpipe Sys.Signal_ignore


let _ = Unix.setsid ()

(*****************************************************************************)
(** Communication functions: *)

let send outch v =
  Lwt_chan.output_value outch v >>=
  (fun () -> Lwt_chan.flush outch)

let execute outch =
  let handle_errors f = try f () with e -> send outch (Error e) in
  function
  | Get (t, k) ->
    handle_errors
      (fun () ->
         try
           send outch (Value (db_get t k))
         with
         | Not_found -> send outch Dbm_not_found)
  | Remove (t, k) -> handle_errors (fun () -> db_remove t k; send outch Ok)
  | Replace (t, k, v) ->
    handle_errors (fun () -> db_replace t k v; send outch Ok)
  | Replace_if_exists (t, k, v) ->
    handle_errors (fun () ->
        try
          ignore (db_get t k);
          db_replace t k v;
          send outch Ok
        with Not_found -> send outch Dbm_not_found)
  | Firstkey t ->
    handle_errors (fun () ->
        try send outch (Key (db_firstkey t))
        with Not_found -> send outch End)
  | Nextkey t ->
    handle_errors (fun () ->
        try send outch (Key (db_nextkey t))
        with Not_found -> send outch End)
  | Length t ->
    handle_errors (fun () ->
        catch
          (fun () ->
             db_length t >>=
             (fun i -> send outch (Value (Marshal.to_string i []))))
          (function Not_found -> send outch Dbm_not_found
                  | e -> send outch (Error e)))

let nb_clients = ref 0

let rec listen_client inch outch =
  Lwt_chan.input_value inch >>=
  (fun v -> execute outch v) >>=
  (fun () -> listen_client inch outch)

let finish _ =
  nb_clients := !nb_clients - 1;
  if !nb_clients = 0
  then close_all 0 ();
  return ()


let b = ref false

let rec loop socket =
  Lwt_unix.accept socket >>=
  (fun (indescr, _) ->
     ignore (
       b := true;
       nb_clients := !nb_clients + 1;
       let inch = Lwt_chan.in_channel_of_descr indescr in
       let outch = Lwt_chan.out_channel_of_descr indescr in
       catch
         (fun () -> listen_client inch outch >>= finish)
         finish);
     loop socket)




let _ = Lwt_unix.run
    (let socket = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
     (try
        Lwt_unix.bind socket (Unix.ADDR_UNIX (directory^"/"^socketname))
      with _ -> errlog ("Please make sure that the directory "^directory^" exists, writable for ocsidbm, and no other ocsidbm process is running on the same directory. If not, remove the file "^(directory^"/"^socketname)); the_end 1);
     Lwt_unix.listen socket 20;
     (* Done in ocsipersist.ml
        let devnull = Unix.openfile "/dev/null" [Unix.O_WRONLY] 0 in
            Unix.dup2 devnull Unix.stdout;
            Unix.dup2 devnull Unix.stderr;
            Unix.close devnull;
            Unix.close Unix.stdin; *)
     ignore (Lwt_unix.sleep 4.1 >>=
             (fun () -> if not !b then close_all 0 (); return ()));
     (* If nothing happened during 5 seconds, I quit *)

     loop socket)




(*****************************************************************************)
(** Garbage collection of expired data *)
(* Experimental

   exception Exn1
   let dbm_fold f t beg =
   let rec aux nextkey beg =
    try
      let k = try nextkey t with Not_found -> raise Exn1 in
      let v = try Dbm.find k t with Not_found -> raise Exn1 in
      aux Dbm.nextkey (f k v beg)
    with Exn1 -> beg
   in
   aux Dbm.firstkey beg

   let _ =
   match sessiongcfrequency with
    None -> () (* No garbage collection *)
   | Some t ->
      let rec f () =
        Lwt_unix.sleep t >>=
        (fun () ->
          let now = Unix.time () in
          print_endline "GC of persistent data";
          Tableoftables.fold
            (fun name t thr ->
              thr >>=
              (fun () ->
                dbm_fold
                  (fun k v thr ->
                    thr >>=
                    (fun () ->
                      (match fst (Marshal.from_string v 0) with
                      | Some exp when exp < now ->
                          try
                            Dbm.remove t k
                          with _ -> ());
                      Lwt_unix.yield ()
                    )
                  )
                  t
                  (return ()))
            )
            !tableoftables
            (return ())
        ) >>=
        f
      in ignore (f ())

*)

