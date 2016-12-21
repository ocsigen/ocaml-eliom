[@@@ocaml.warning "+a-4-9-40-42"]
open Types

module Correspondance = struct

  let rec longident_of_path = function
    | Path.Pident id ->
        Longident.Lident (Ident.name id)
    | Path.Pdot (p,s,_i) ->
        Longident.Ldot (longident_of_path p, s)
    | Path.Papply (p1,p2) ->
        Longident.Lapply (longident_of_path p1, longident_of_path p2)

  exception No_translation of Path.t

  let path lookup env p =
    let lid = longident_of_path p in
    let new_p, _ =
      try lookup ?loc:None lid env
      with Not_found -> raise (No_translation p)
    in
    new_p

  let rec expression env expr = match expr.desc with
    | Tconstr (p,args,_abbrev) ->
        let desc = Tconstr
            (path Env.lookup_type env p,
             List.map (expression env) args,
             ref Mnil)
        in
        {expr with desc}

    | Tlink t -> expression env t

    | Tpackage (p,n,l) ->
        let desc = Tpackage (
            path Env.lookup_modtype env p,
            n, List.map (expression env) l)
        in
        {expr with desc}

    (* Must return the original expression, to preserve sharing *)
    | Tvar _
    | Tunivar _ -> expr

    (* For all the following, we just copy. *)
    | Tvariant _
    | Tpoly (_,_)
    | Ttuple _

    (* Technically, those can't be serialized. *)
    | Tarrow (_,_,_,_)
    | Tobject (_,_)
    | Tfield (_,_,_,_)
    | Tnil

    (* Shouldn't happen, but not important. *)
    | Tsubst _

      as ty ->
        {expr with
         desc = Btype.copy_type_desc ~keep_names:true (expression env) ty
        }


  let go loc env expr =
    Eliom_base.in_loc loc @@ fun () ->
    try Ok (expression env expr)
    with No_translation p -> Error p

end

let find_correspondance = Correspondance.go


module Error_msg = struct

  (* When doing spellchecking, we filter identifiers that are not on the
     same side as the current scope.
  *)
  let filter_add side name path l = match path with
    | Some path when
        Eliom_base.conform ~scope:side ~id:(Ident.side @@ Path.head path)
      -> name::l
    | _ -> l

  exception FoundIn of Eliom_base.side

  let injection ppf path fold env s =
    let side = Eliom_base.get_side () in
    let mside = Eliom_base.mirror side in
    let aux name path acc = match path with
      | Some path ->
          let pside = Ident.side @@ Path.head path in
          if Eliom_base.conform ~scope:pside ~id:mside && name = s
          then raise (FoundIn pside)
          else acc
      | None -> acc
    in
    try ignore (fold aux path env [])
    with FoundIn pside ->
      Format.fprintf ppf
        "@\nHint: The current scope is %s but this identifier is available in %s scope.@?"
        (Eliom_base.to_string side)
        (Eliom_base.to_string pside)

end

module Tast = struct
  open! Typedtree

  let add_stri_attr attr = function
    | Tstr_eval (e, attrs) -> Tstr_eval (e, attr::attrs)
    | Tstr_primitive x ->
        Tstr_primitive {x with val_attributes = attr :: x.val_attributes}
    | Tstr_value (rc,x) ->
        Tstr_value (rc,List.map
            (fun x -> {x with vb_attributes = attr :: x.vb_attributes}) x)
    | Tstr_type (r,l) ->
        Tstr_type (r,List.map
            (fun x -> {x with typ_attributes = attr :: x.typ_attributes}) l)
    | Tstr_typext tex ->
        Tstr_typext {tex with tyext_attributes = attr :: tex.tyext_attributes}
    | Tstr_exception exn ->
        Tstr_exception {exn with ext_attributes = attr :: exn.ext_attributes}
    | Tstr_module mb ->
        Tstr_module {mb with mb_attributes = attr :: mb.mb_attributes}
    | Tstr_recmodule rmb ->
        Tstr_recmodule (List.map
            (fun mb -> {mb with mb_attributes = attr :: mb.mb_attributes}) rmb)
    | Tstr_modtype mt ->
        Tstr_modtype {mt with mtd_attributes = attr :: mt.mtd_attributes}
    | Tstr_open op ->
        Tstr_open {op with open_attributes = attr :: op.open_attributes}
    | Tstr_include ic ->
        Tstr_include {ic with incl_attributes = attr :: ic.incl_attributes}
    | Tstr_class cls ->
        Tstr_class (List.map
            (fun (cl,s) -> {cl with ci_attributes = attr :: cl.ci_attributes}, s) cls)
    | Tstr_class_type clt ->
        Tstr_class_type (List.map
            (fun (id,s,cl) -> id,s,{cl with ci_attributes = attr :: cl.ci_attributes})
            clt)
    | Tstr_attribute at ->
        Tstr_attribute at

  let add_sigi_attr attr = function
    | Sig_value (id,vd) ->
        Sig_value (id,{vd with val_attributes = attr :: vd.val_attributes})
    | Sig_type (id,td,rc) ->
        Sig_type (id,{td with type_attributes = attr :: td.type_attributes},rc)
    | Sig_typext (id,ec,es) ->
        Sig_typext (id,{ec with ext_attributes = attr :: ec.ext_attributes},es)
    | Sig_module (ed,md,rs) ->
        Sig_module (ed,{md with md_attributes = attr :: md.md_attributes},rs)
    | Sig_modtype (id,mtd) ->
        Sig_modtype (id,{mtd with mtd_attributes = attr :: mtd.mtd_attributes})
    | Sig_class (id,cd,rs) ->
        Sig_class (id,{cd with cty_attributes = attr :: cd.cty_attributes},rs)
    | Sig_class_type (id,ctd,rs) ->
        Sig_class_type (id,{ctd with clty_attributes = attr :: ctd.clty_attributes},rs)


  let add_tsigi_attr_desc attr = function
    | Tsig_value x ->
        Tsig_value {x with val_attributes = attr :: x.val_attributes}
    | Tsig_type (r,l) ->
        Tsig_type (r,List.map
            (fun x -> {x with typ_attributes = attr :: x.typ_attributes}) l)
    | Tsig_typext tex ->
        Tsig_typext {tex with tyext_attributes = attr :: tex.tyext_attributes}
    | Tsig_exception exn ->
        Tsig_exception {exn with ext_attributes = attr :: exn.ext_attributes}
    | Tsig_module md ->
        Tsig_module {md with md_attributes = attr :: md.md_attributes}
    | Tsig_recmodule rmd ->
        Tsig_recmodule (List.map
            (fun (md : Typedtree.module_declaration) ->
               {md with md_attributes = attr :: md.md_attributes}) rmd)
    | Tsig_modtype mt ->
        Tsig_modtype {mt with mtd_attributes = attr :: mt.mtd_attributes}
    | Tsig_open op ->
        Tsig_open {op with open_attributes = attr :: op.open_attributes}
    | Tsig_include ic ->
        Tsig_include {ic with incl_attributes = attr :: ic.incl_attributes}
    | Tsig_class cls ->
        Tsig_class (List.map
            (fun cl -> {cl with ci_attributes = attr :: cl.ci_attributes}) cls)
    | Tsig_class_type clt ->
        Tsig_class_type (List.map
            (fun cl -> {cl with ci_attributes = attr :: cl.ci_attributes})
            clt)
    | Tsig_attribute at ->
        Tsig_attribute at

  let add_tsigi_attr attr x =
    {x with sig_desc = add_tsigi_attr_desc attr x.sig_desc}

end



let fragment_lid = Longident.parse "Eliom_runtime.fragment"
let fragment_type = ref `NotResolved
let error ~loc =
  Eliom_base.error ~loc
    "Could not find Eliom_runtime.fragment.@ \
     Please load the server runtime library.@."
let try_resolve loc env = match !fragment_type with
  | `Resolved x -> x
  | `NotResolved -> begin try
        let x = Env.lookup_type ~loc fragment_lid env in
        fragment_type := `Resolved x ;
        x
      with Not_found ->
        fragment_type := `NotFound ;
        error ~loc
    end
  | `NotFound -> error ~loc

let maybe_fragment = function
  | Path.Pdot (Path.Pident id, "fragment", _)
    when Ident.name id = "Eliom_runtime" -> true
  | _ -> false
let is_fragment ~loc ~env p =
  Eliom_base.get_side () = Eliom_base.(Loc Server) &&
  maybe_fragment p &&
  let fragment_path, _ = try_resolve loc env in
  Path.same p fragment_path

let fragment ~loc ~env t =
  let fragment_path, _ = try_resolve loc env in
  Btype.newgenty (Tconstr(fragment_path, [t], ref Mnil))


module Sideness = struct
  open Btype

  type t = Eliom_base.side

  let attr_name = "eliom.sideness"
  let sep = ','

  (** Getting the info out of a type declaration *)

  let rec mk_side_list i acc =
    if i <= 0 then acc else mk_side_list (i-1) (Eliom_base.Poly :: acc)

  let of_tydecl (tydecl : Types.type_declaration) =
    let loc = tydecl.type_loc in
    let attrs = tydecl.type_attributes in
    let attrs =
      List.find_all (fun (x,_) -> x.Location.txt = attr_name) attrs
    in
    match attrs with
    | [] -> mk_side_list tydecl.type_arity []
    | [ _, PStr [{pstr_desc=Pstr_eval (
        {pexp_desc=Pexp_constant (Pconst_string (s,_))}
      ,_)}]] ->
        List.map Eliom_base.of_string @@ Misc.split s sep
    | [_] ->
        Eliom_base.error ~loc "Malformed sideness annotation in cmi file."
    | _ ->
        Eliom_base.error ~loc "Multiple sideness annotation in cmi file."

  let wrap (side:t) f = match side with
    | Poly -> f ()
    | Loc l -> Eliom_base.in_loc l (fun () -> f ())


  (** Checking the side of a type declaration *)

  let find visited ty =
    try Some (TypeMap.find ty !visited)
    with Not_found -> None

  exception Error of {
      ty : Types.type_expr ;
      side : t ;
      scope : t ;
    }
  let error_msg ~loc ~ty:_ ~side ~scope =
    let open Eliom_base in
    let hint ppf (side, scope) = match side, scope with
      | Poly, Loc (Client | Server) ->
          Format.fprintf ppf
            "@\nHint: Add a sideness annotation `[@@%a]` on the type parameter."
            Eliom_base.pp scope
      | _ -> ()
    in
    Location.errorf ~loc
      "In this type definition, the type parameter is \
       declared on %a side but is used on %a side.%a"
      (* Printtyp.type_expr ty *)
      Eliom_base.pp side
      Eliom_base.pp scope
      hint (side, scope)

  let fail ~side ~scope ~ty = raise (Error {ty; side; scope})

  (* Simplified copy of Typedecl.compute_variance_rec *)
  let rec check_rec env visited scope ty =
    let ty = Ctype.repr ty in
    match find visited ty with
    | Some side ->
        if scope = side then ()
        else fail ~ty ~side ~scope
    | None -> begin
        visited := TypeMap.add ty scope !visited;
        let check_same = check_rec env visited scope in
        match ty.desc with
        | Tconstr (path, tl, _) ->
            if tl = [] then () else begin
              try
                let decl = Env.find_type path env in
                let sideness = of_tydecl decl in
                List.iter2 (check_rec env visited) sideness tl
              with Not_found ->
                List.iter check_same tl
            end

        | Tvar _ | Tnil | Tlink _ | Tunivar _ -> ()

        | Tobject (ty, _)
        | Tsubst ty
        | Tpoly (ty, _) ->
            check_same ty

        | Tarrow (_, ty1, ty2, _)
        | Tfield (_, _, ty1, ty2) ->
            check_same ty1; check_same ty2

        | Tpackage (_, _, tl)
        | Ttuple tl ->
            List.iter check_same tl

        | Tvariant row ->
            let row = Btype.row_repr row in
            List.iter
              (fun (_,f) ->
                 match Btype.row_field_repr f with
                   Rpresent (Some ty) ->
                     check_same ty
                 | Reither (_, tyl, _, _) ->
                     List.iter check_same tyl
                 | _ -> ())
              row.row_fields;
            check_same row.row_more

      end

  let gather_tys_variant args res acc =
    let for_constr = function
      | Types.Cstr_tuple l -> l
      | Types.Cstr_record l ->
          List.map (fun {ld_type} -> ld_type) l
    in
    let l = for_constr args @ acc in
    match res with None -> l | Some ty -> ty :: l

  let gather_tys decl =
    let mn =
      match decl.type_manifest with
        None -> []
      | Some ty -> [ty]
    in
    match decl.type_kind with
      Type_abstract | Type_open -> mn
    | Type_variant tll ->
        List.fold_left
          (fun acc v -> gather_tys_variant v.cd_args v.cd_res acc)
          mn tll
    | Type_record (ftl, _) ->
        mn @ List.map (fun {Types.ld_type} -> ld_type) ftl

  let check_internal env loc sides params tys =
    let init_map = List.fold_right2 TypeMap.add params sides TypeMap.empty in
    let visited = ref init_map in
    try
      List.iter (check_rec env visited Eliom_base.Poly) tys
    with Error {ty; side; scope} ->
      raise (Location.Error (error_msg ~loc ~ty ~side ~scope))

  let check env decl =
    let loc = decl.type_loc in
    let sides = of_tydecl decl in
    let tys = gather_tys decl in
    check_internal env loc sides decl.type_params tys

  let check_extension env decl ext =
    let loc = decl.type_loc in
    let sides = of_tydecl decl in
    let tys =
      gather_tys_variant ext.ext_args ext.ext_ret_type []
    in
    check_internal env loc sides ext.ext_type_params tys


  (** Getting the info out of the AST *)

  let get ptyp : t =
    let open! Eliom_base in
    let open Parsetree in
    let sides : loc list ref = ref [] in
    let () =
      List.iter (function
        | {Location.txt = "client"|"eliom.client" }, payload ->
            begin match payload with
            | PStr [] -> sides := Client :: !sides
            | _ -> error ~loc:ptyp.ptyp_loc
                  "Malformed sideness annotation. It should be [@client] \
                   or [@eliom.client]."
            end
        | {Location.txt = "server"|"eliom.server" }, payload ->
            begin match payload with
            | PStr [] -> sides := Server :: !sides
            | _ -> error ~loc:ptyp.ptyp_loc
                  "Malformed sideness annotation. It should be [@server] \
                   or [@eliom.server]."
            end
        | _ -> ())
        ptyp.ptyp_attributes
    in
    match !sides with
    | [] -> Poly
    | [loc] -> Loc loc
    | _ ->
        error ~loc:ptyp.ptyp_loc
          "Multiple sideness annotation. There should only be one \
           sideness annotation per parameter."

  let gets l = List.map (fun (x,_) -> get x) l

  (** Annotating type declarations with sideness info *)

  let to_attr loc l =
    let open Ast_helper in
    let sep = String.make 1 sep in
    let s = String.concat sep @@ List.map Eliom_base.to_string l in
    let payload =
      Str.eval ~loc (Exp.constant ~loc (Const.string s))
    in
    Location.mkloc attr_name loc, Parsetree.PStr [payload]

  let annotate params tydecl =
    let params = gets params in
    if params = [] || List.for_all ((=) Eliom_base.Poly) params
    then tydecl
    else
      let attr = to_attr tydecl.Types.type_loc params in
      {tydecl with type_attributes = attr :: tydecl.type_attributes}

  (** Checking inclusion of sideness *)

  let included (annot1 : t list) annot2 =
    (annot1 = annot2)

end
