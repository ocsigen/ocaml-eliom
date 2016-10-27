(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Consistency tables: for checking consistency of module CRCs *)

(* ELIOM *)
type side = Poly | Server | Client
type elt = string * side
(* /ELIOM *)

type t = (elt, Digest.t * string) Hashtbl.t

let create () = Hashtbl.create 13

let clear = Hashtbl.clear

exception Inconsistency of elt * string * string

exception Not_available of elt

let check tbl name crc source =
  try
    let (old_crc, old_source) = Hashtbl.find tbl name in
    if crc <> old_crc then raise(Inconsistency(name, source, old_source))
  with Not_found ->
    Hashtbl.add tbl name (crc, source)

let check_noadd tbl name crc source =
  try
    let (old_crc, old_source) = Hashtbl.find tbl name in
    if crc <> old_crc then raise(Inconsistency(name, source, old_source))
  with Not_found ->
    raise (Not_available name)

let set tbl name crc source = Hashtbl.add tbl name (crc, source)

let source tbl name = snd (Hashtbl.find tbl name)

let extract l tbl =
  let compare (n1,s1) (n2,s2) =
    let x = String.compare n1 n2 in
    if x = 0 then compare s1 s2 else x
  in
  let l = List.sort_uniq compare l in
  List.fold_left
    (fun assc name ->
       try
         let (crc, _) = Hashtbl.find tbl name in
           (name, Some crc) :: assc
       with Not_found ->
         (name, None) :: assc)
    [] l

let filter p tbl =
  let to_remove = ref [] in
  Hashtbl.iter
    (fun name (crc, auth) ->
      if not (p name) then to_remove := name :: !to_remove)
    tbl;
  List.iter
    (fun name ->
       while Hashtbl.mem tbl name do Hashtbl.remove tbl name done)
    !to_remove
