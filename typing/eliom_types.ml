[@@@ocaml.warning "+a-4-9-40-42"]
open Types
open Btype

(** New magic number for eliom-specific interfaces. *)
let cmi_magic_number = "Eliom000I020"
let () = assert
  (String.length cmi_magic_number = String.length Config.cmi_magic_number)


let make_iterator it_ident =
  let r = ref TypeSet.empty in
  let it_path p = List.iter it_ident @@ Path.heads p in
  let it_type_expr it ty =
    if TypeSet.mem ty !r then ()
    else begin
      r := TypeSet.add ty !r ;
      it.it_do_type_expr it ty ;
      let () = match ty.desc with
        | Tconstr (_, _, abbrev) ->
            iter_abbrev (it.it_type_expr it) !abbrev
        | _ -> ()
      in
      ()
    end
  in
  { type_iterators with
    it_path ; it_ident ; it_type_expr ;
  }


(** Change the side of all the identifiers in a given Types. *)
module Translate = struct
  open Eliom_base

  (* The side of builtin must not be changed, since they have a special path
     (doesn't start with a global module). *)
  let is_predef i =
    let rec aux i = function
      | (_, id)::_ when Ident.same id i -> true
      | _::t -> aux i t
      | [] -> false
    in aux i Predef.builtin_idents

  let make side =
    let it_ident i =
      if is_predef i then ()
      else Ident.change_side side i
    in
    make_iterator it_ident

  let client = make (Loc Client)
  let server = make (Loc Server)

  let get = function
    | Client -> client
    | Server -> server

  let signature side =
    let it = get side in
    it.it_signature it

  let modtype side =
    let it = get side in
    it.it_module_type it

  let module_declaration side =
    let it = get side in
    it.it_module_declaration it

  let modtype_declaration side =
    let it = get side in
    it.it_modtype_declaration it

  let class_declaration side =
    let it = get side in
    it.it_class_declaration it

  let class_type_declaration side =
    let it = get side in
    it.it_class_type_declaration it

end

let translate = Translate.signature

let global_side s =
  let open Eliom_base in
  let r = ref `OCaml in
  let add_side side = match side, !r with
    | Loc l, `OCaml -> r := `Loc l
    | Loc Client, `Loc Server
    | Loc Server, `Loc Client
      -> r := `Mixed
    | _ -> ()
  in
  let it_ident i = add_side @@ Ident.side i in
  let it = make_iterator it_ident in
  it.it_signature it s ;
  !r

let is_mixed s = global_side s = `Mixed



(** Type specialization

    When a side-poly type is used in a client or server scope, we
    specialize the declaration to the appropriate side.

    This allow to type pieces of code such as :

    module%client M = ...
    module%client S = Set.Make(M)
*)
module Specialize = struct

  type 'a t = Eliom_base.side -> 'a -> 'a

  (** Test if we should specialize and return the
      location to specialize to if appropriate.
  *)
  let test ~scope ~idside =
    let open Eliom_base in
    match idside, scope with
    | Poly, Loc l -> Some l
    | _ , Poly -> None
    | Loc _, _ -> None

  let specialization_with copy translate idside x =
    let scope = Eliom_base.get_side () in
    match test ~scope ~idside with
    | Some loc ->
        let x' = copy x in
        translate loc x' ;
        x'
    | None -> x

  let modtype =
    specialization_with
      Subst.(modtype identity)
      Translate.modtype

  let module_declaration =
    specialization_with
      Subst.(module_declaration ~renew:false identity)
      Translate.module_declaration

  let modtype_declaration =
    specialization_with
      Subst.(modtype_declaration ~renew:false identity)
      Translate.modtype_declaration

  let class_declaration =
    specialization_with
      Subst.(class_declaration identity)
      Translate.class_declaration

  let class_type_declaration =
    specialization_with
      Subst.(cltype_declaration identity)
      Translate.class_type_declaration

  let copy_ident scope i =
    let idside = Ident.side i in
    match test ~scope ~idside with
    | None -> i (* Keep the same ident, the typechecker uses == *)
    | Some l -> Ident.with_side (Loc l) i

  let rec copy_path side : Path.t -> Path.t = function
    | Pident id -> Pident (copy_ident side id)
    | Pdot(p, s, pos) -> Pdot (copy_path side p, s, pos)
    | Papply(p1, p2) -> Papply (copy_path side p1, copy_path side p2)

  let ident' scope i =
    if scope = Eliom_base.Poly then i
    else copy_ident scope i

  let ident i = ident' (Eliom_base.get_side ()) i

  let path' scope p =
    if scope = Eliom_base.Poly then p
    else copy_path scope p

  let path p = path' (Eliom_base.get_side ()) p

end

let () =
  Subst.specialize_path := Specialize.path' ;
  Subst.specialize_modtype := Specialize.modtype ;
  ()
