[@@@ocaml.warning "+a-4-9-40-42"]
open Types

(* Utilities *)

(* module Build = struct *)

(*   let exp ~env ?(attrs=[]) ?(loc=Location.none) desc ty = *)
(*     { exp_desc = desc; exp_type = ty; *)
(*       exp_loc  = loc; exp_extra = []; *)
(*       exp_attributes = attrs; *)
(*       exp_env  = env } *)

(*   let exp_ident env name = *)
(*     let lid     = Longident.parse name in *)
(*     let (p, vd) = try Env.lookup_value lid env *)
(*       with Not_found -> *)
(*         Misc.fatal_error ("Trx.find_value: " ^ name) in *)
(*     exp ~env (Texp_ident (p,mknoloc lid, vd)) *)
(*       (Ctype.instance env vd.val_type) *)

(*   let exp_apply : expression -> expression list -> expression_desc *)
(*     = fun f args -> *)
(*     Texp_apply(f, List.map (fun arg -> (Nolabel,Some arg)) args) *)

(* end *)

module Translate = struct

  let rec longident_of_path = function
    | Path.Pident id ->
        Longident.Lident (Ident.name id)
    | Path.Pdot (p,s,_i) ->
        Longident.Ldot (longident_of_path p, s)
    | Path.Papply (p1,p2) ->
        Longident.Lapply (longident_of_path p1, longident_of_path p2)

  exception No_translation of Path.t

  let path f p =
    let lid = longident_of_path p in
    let new_p, _ =
      try f lid
      with Not_found -> raise (No_translation p)
    in
    new_p

  let rec expression f expr = match expr.desc with
    | Tconstr (p,args,_abbrev) ->
        let desc = Tconstr
            (f p,
             List.map (expression f) args,
             ref Mnil)
        in
        {expr with desc}

    | Tlink t -> expression f t

    | Tpackage (p,n,l) ->
        let desc = Tpackage (
            f p,
            n, List.map (expression f) l)
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
         desc = Btype.copy_type_desc ~keep_names:true (expression f) ty
        }


  let go env side expr =
    Eliom_base.in_side side @@ fun () ->
    let f = path (fun id -> Env.lookup_type id env) in
    try Ok (expression f expr)
    with No_translation p -> Error p

end

let translate = Translate.go


module Error_msg = struct

  (* When doing spellchecking, we filter identifiers that are not on the
     same side as the current scope.
  *)
  let filter_add side name path l = match path with
    | Some path when
        Eliom_base.conform ~scope:side ~id:(Ident.side @@ Path.head path)
      -> name::l
    | _ -> l

  exception FoundIn of Eliom_base.shside

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

module Tast_helper = struct
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
end
