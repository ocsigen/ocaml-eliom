[@@@ocaml.warning "+a-4-9-40-42"]
open Types
open Btype


let unset s ppf _ = Format.pp_print_string ppf ("unset: "^s)
let printer_modtype = ref @@ unset "modtype"
let printer_path = ref @@ unset "path"
let printer_modtype_decl = ref (fun _id -> unset "modtype decl")
let printer_module_decl = ref (fun _id -> unset "module decl")


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

  type 'a t = ?idside:Eliom_base.side -> 'a -> 'a

  (** Test if we should specialize and return the
      location to specialize to if appropriate.
  *)
  let test ~scope ~idside =
    let open Eliom_base in
    match idside, scope with
    | Poly, Loc l -> Some l
    | _ , Poly -> None
    | Loc _, _ -> None

  let copy_ident scope i =
    let idside = Ident.side i in
    match test ~scope ~idside with
    | None -> i (* Keep the same ident, the typechecker uses == *)
    | Some _ when Translate.is_predef i -> i
    | Some l -> Ident.with_side (Eliom_base.Loc l) i

  class copier side = object (self)
    inherit Types_mapper.t
    method! ident = copy_ident side
    method! type_desc _ = assert false

    (* Simplified from Subst.tyexp.
       We must be very careful with sharing, abbreviations and
       a lot of other things, so better just do exactly what the
       rest of the typecheker do.
    *)
    method tyexp ty =
      let ty = repr ty in
      match ty.desc with
        Tvar _ | Tunivar _ as desc ->
          if ty.id < 0 then
            let ty' = newty2 ty.level desc in
            save_desc ty desc; ty.desc <- Tsubst ty'; ty'
          else begin (* when adding a module to the environment *)
            if ty.level < generic_level then
              ty.level <- min ty.level Btype.generic_level;
            ty
          end
      | Tsubst ty ->
          self#tyexp ty
      | _ ->
          let desc = ty.desc in
          save_desc ty desc;
          (* Make a stub *)
          let ty' = newgenvar () in
          ty.desc <- Tsubst ty';
          ty'.desc <-
            begin match desc with
            | Tconstr(p, tl, _abbrev) ->
                Tconstr(self#path p, List.map self#tyexp tl, ref Mnil)
            | Tpackage(p, n, tl) ->
                Tpackage(self#path p, n, List.map self#tyexp tl)
            | Tobject (t1, name) ->
                Tobject (self#tyexp t1,
                  ref (match !name with
                      None -> None
                    | Some (p, tl) ->
                        Some (self#path p, List.map self#tyexp tl)))
            | Tfield (m, k, t1, t2)
              when ty.level < generic_level && m = dummy_method ->
                (* not allowed to lower the level of the dummy method *)
                Tfield (m, k, t1, self#tyexp t2)
            | Tvariant row ->
                let row = row_repr row in
                let more = repr row.row_more in
                (* We must substitute in a subtle way *)
                (* Tsubst takes a tuple containing the row var and the variant *)
                begin match more.desc with
                  Tsubst {desc = Ttuple [_;ty2]} ->
                    (* This variant type has been already copied *)
                    ty.desc <- Tsubst ty2; (* avoid Tlink in the new type *)
                    Tlink ty2
                | _ ->
                    let dup = more.level = generic_level || static_row row ||
                      match more.desc with Tconstr _ -> true | _ -> false in
                    (* Various cases for the row variable *)
                    let more' =
                      match more.desc with
                        Tsubst ty -> ty
                      | Tconstr _ | Tnil -> self#tyexp more
                      | Tunivar _ | Tvar _ ->
                          save_desc more more.desc;
                          if dup && is_Tvar more then newgenty more.desc else more
                      | _ -> assert false
                    in
                    (* Register new type first for recursion *)
                    more.desc <- Tsubst(newgenty(Ttuple[more';ty']));
                    (* Return a new copy *)
                    let row =
                      copy_row self#tyexp true row (not dup) more' in
                    match row.row_name with
                      Some (p, tl) ->
                        Tvariant {row with row_name = Some (self#path p, tl)}
                    | None ->
                        Tvariant row
                end
            | Tfield(_label, kind, _t1, t2) when field_kind_repr kind = Fabsent ->
                Tlink (self#tyexp t2)
            | _ -> copy_type_desc self#tyexp desc
            end;
          ty'

    method! type_expr ty =
      let ty' = self#tyexp ty in
      cleanup_types ();
      ty'
  end

  let copy =
    let open Eliom_base in
    let client_copier = new copier (Loc Client) in
    let server_copier = new copier (Loc Server) in
    let poly_copier = new copier Poly in
    function
    | Loc Client -> client_copier
    | Loc Server -> server_copier
    | Poly -> poly_copier

  let specialization_with ?printer met ?(idside=Eliom_base.Poly) x =
    let scope = Eliom_base.get_side () in
    match test ~scope ~idside with
    | Some loc ->
        begin match printer with None -> () | Some printer ->
          Format.printf
            "Copying from side %a to side %a.@."
            Eliom_base.pp idside Eliom_base.pp scope ;
          Format.printf "Original version: %a@." printer x ;
        end ;
        let x' = met (copy @@ Eliom_base.Loc loc) x in
        begin match printer with None -> () | Some printer ->
          Format.printf "New version: %a@.@." printer x' ;
        end ;
        x'
    | None -> x

  let modtype ?idside x =
    specialization_with
      (* ~printer:(!printer_modtype) *)
      (fun o -> o#module_type)
      ?idside x

  let module_declaration ?idside x =
    specialization_with
      (* ~printer:(!printer_module_decl (Ident.create_persistent "Foo")) *)
      (fun o -> o#module_declaration)
      ?idside x

  let modtype_declaration ?idside x =
    specialization_with
      (* ~printer:(!printer_modtype_decl (Ident.create_persistent "Foo")) *)
      (fun o -> o#modtype_declaration)
      ?idside x

  let class_declaration ?idside x =
    specialization_with
      (fun o -> o#class_declaration)
      ?idside x

  let class_type_declaration ?idside x =
    specialization_with
      (fun o -> o#class_type_declaration)
      ?idside x



  let ident' scope i =
    if scope = Eliom_base.Poly then i
    else copy_ident scope i

  let ident i = ident' (Eliom_base.get_side ()) i

  let path' ~scope p =
    if scope = Eliom_base.Poly then p
    else (copy scope)#path p

  let path p = path' ~scope:(Eliom_base.get_side ()) p

end
