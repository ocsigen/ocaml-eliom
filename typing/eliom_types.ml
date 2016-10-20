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


(* The side of builtin must not be changed, since they have a special path
   (doesn't start with a global module). *)
let is_predef i =
  let rec aux i = function
    | (_, id)::_ when Ident.same id i -> true
    | _::t -> aux i t
    | [] -> false
  in aux i Predef.builtin_idents

let translate side tsig =
  let it_ident i =
    if is_predef i then ()
    else Ident.change_side side i
  in
  let it = make_iterator it_ident in
  it.it_signature it tsig

let global_side s =
  let r = ref `Noside in
  let add_side side = match side, !r with
    | _, `Noside -> r := side
    | `Shared, _
    | `Client, `Server
    | `Server, `Client
      -> r := `Shared
    | _ -> ()
  in
  let it_ident i = add_side @@ Ident.side i in
  let it = make_iterator it_ident in
  it.it_signature it s ;
  !r

let is_mixed s = global_side s = `Shared



(** Signature lifting

    When a base moduled is used in a client or server scope, we
    lift the declaration to the appropriate side.

    This allow to type pieces of code such as :

    module%client M = ...
    module%client S = Set.Make(M)
*)
module Lift = struct

  let refresh ~side id = match side, Ident.side id with
    | (`Client | `Server | `Shared), `Noside ->
        Ident.rename ~side id
    | _ -> id

  let rec signature_component side = function
    | Sig_value(id, d) ->
        Sig_value(refresh ~side id, d)
    | Sig_type(id, d, rs) ->
        Sig_type(refresh ~side id, d, rs)
    | Sig_typext(id, ext, es) ->
        Sig_typext(refresh ~side id, ext, es)
    | Sig_module(id, d, rs) ->
        Sig_module(refresh ~side id, module_decl side d, rs)
    | Sig_modtype(id, d) ->
        Sig_modtype(refresh ~side id, modtype_decl side d)
    | Sig_class(id, d, rs) ->
        Sig_class(refresh ~side id, d, rs)
    | Sig_class_type(id, d, rs) ->
        Sig_class_type(refresh ~side id, d, rs)

  and module_decl side md =
    { md with md_type = module_type side md.md_type }
  and modtype_decl side mtd =
    { mtd with mtd_type = Misc.may_map (module_type side) mtd.mtd_type }

  and signature side x = List.map (signature_component side) x

  and module_type side = function
    | Mty_signature s -> Mty_signature (signature side s)
    | Mty_functor (id, param, res) ->
        let id' = refresh ~side id in
        let param' = Misc.may_map (module_type side) param in
        let res =
          if Ident.same id' id then res
          else Subst.(modtype (add_module id (Path.Pident id') identity)) res
        in
        let res' = module_type side res in
        Mty_functor (id', param', res')

    | Mty_ident _ | Mty_alias _ as mty -> mty


end
