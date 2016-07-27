[@@@ocaml.warning "+A-4-6-9-40-42-44-48"]
open Typedtree
open Ast_helper
module U = Untypeast
module AM = Ast_mapper

(** Internal attributes *)
let eliom_fragment_attr = "eliom.client"

let lid = Longident.parse
let elid ?(loc = !default_loc) s =
  Exp.ident ~loc (mkloc (lid s) loc)
let app ~loc s args =
  Exp.apply ~loc (elid ~loc s)
    (List.map (fun e -> Asttypes.Nolabel, e) args)

let eunit loc = Exp.construct ~loc (mkloc (lid "()") loc) None
let punit loc = Pat.construct ~loc (mkloc (lid "()") loc) None

let ptuple ~loc = function
  | [] ->  punit loc
  | [e] -> e
  | l -> Pat.tuple ~loc l

let etuple ~loc = function
  | [] ->  eunit loc
  | [e] -> e
  | l -> Exp.tuple ~loc l

let make_sequence ~loc l =
  let f e l = Exp.sequence ~loc:e.Parsetree.pexp_loc e l in
  List.fold_right f l (eunit loc)

let unit_str ~loc e =
  (* let () = [%e e ] *)
  with_default_loc loc @@ fun () ->
  let b = Vb.mk (ptuple ~loc []) e in
  Str.value Nonrecursive [b]

let rec get_attr s = function
  | ({Location.txt}, stri) :: _ when txt = s -> Some stri
  | _ :: t -> get_attr s t
  | [] -> None

let lexing_position ~loc l =
  Exp.tuple ~loc [
    Exp.constant ~loc @@ Const.int @@ l.Lexing.pos_lnum;
    Exp.constant ~loc @@ Const.int @@ l.Lexing.pos_bol;
    Exp.constant ~loc @@ Const.int @@ l.Lexing.pos_cnum ;
  ]

let position loc =
  let start = loc.Location.loc_start in
  let stop = loc.Location.loc_end in
  (* Hopefully, start and end positions are in the same file. *)
  let file = Exp.constant ~loc @@ Const.string @@ start.Lexing.pos_fname in
  (* [%expr *)
  (*   Eliom_runtime.pos [%e file] *)
  (*     [%e lexing_position ~loc start] *)
  (*     [%e lexing_position ~loc stop] *)
  (* ][@metaloc loc] *)
  app ~loc "Eliom_runtime.pos" [
    file ;
    lexing_position ~loc start ;
    lexing_position ~loc stop ;
  ]


(* We use a strong hash (MD5) of the file name.
   We only keep the first 36 bit, which should be well enough: with
   256 files, the likelihood of a collision is about one in two
   millions.
   These bits are encoded using an OCaml-compatible variant of Base
   64, as the hash is used to generate OCaml identifiers. *)
let str_file_hash loc =
  let s = Digest.string loc.Location.loc_start.pos_fname in
  let e = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_'" in
  let o = Bytes.create 6 in
  let g p = Char.code s.[p] in
  for i = 0 to 5 do
    let p = i * 6 / 8 in
    let d = 10 - (i * 6) mod 8 in
    Bytes.set o i e.[(g p lsl 8 + g (p + 1)) lsr d land 63]
  done;
  Bytes.to_string o

let file_hash loc =
  Exp.constant ~loc (Const.string @@ str_file_hash loc)

module Untyp = struct

  let identity_stri =
    Untypeast.default_mapper.structure_item
      Untypeast.default_mapper

  let map_mod_attr f = function
    | Tstr_eval (e, attrs) -> Tstr_eval (e, f attrs)
    | Tstr_primitive x ->
        Tstr_primitive {x with val_attributes = f x.val_attributes}
    | Tstr_value (rc,x) ->
        Tstr_value (rc,List.map
            (fun x -> {x with vb_attributes = f x.vb_attributes}) x)
    | Tstr_type (r,l) ->
        Tstr_type (r,List.map
            (fun x -> {x with typ_attributes = f x.typ_attributes}) l)
    | Tstr_typext tex ->
        Tstr_typext {tex with tyext_attributes = f tex.tyext_attributes}
    | Tstr_exception exn ->
        Tstr_exception {exn with ext_attributes = f exn.ext_attributes}
    | Tstr_module mb ->
        Tstr_module {mb with mb_attributes = f mb.mb_attributes}
    | Tstr_recmodule rmb ->
        Tstr_recmodule (List.map
            (fun mb -> {mb with mb_attributes = f mb.mb_attributes}) rmb)
    | Tstr_modtype mt ->
        Tstr_modtype {mt with mtd_attributes = f mt.mtd_attributes}
    | Tstr_open op ->
        Tstr_open {op with open_attributes = f op.open_attributes}
    | Tstr_include ic ->
        Tstr_include {ic with incl_attributes = f ic.incl_attributes}
    | Tstr_class cls ->
        Tstr_class (List.map
            (fun (cl,s) -> {cl with ci_attributes = f cl.ci_attributes}, s) cls)
    | Tstr_class_type clt ->
        Tstr_class_type (List.map
            (fun (id,s,cl) -> id,s,{cl with ci_attributes = f cl.ci_attributes})
            clt)
    | Tstr_attribute at ->
        Tstr_attribute at

  (* Returns true if a structure item only declares types. *)
  let declaration_kind = function
    | Tstr_type _ | Tstr_modtype _ | Tstr_open _ | Tstr_class_type _
    | Tstr_attribute _  -> `Type

    | Tstr_module _ | Tstr_recmodule _ -> `Module

    | Tstr_eval (_,_) | Tstr_value (_,_) | Tstr_primitive _
    | Tstr_typext _ | Tstr_exception _
    | Tstr_class _ | Tstr_include _ -> `Section

  (** Attribute extraction *)
  module Attr = struct
    let get {Location.txt} = match txt with
      | "eliom.client"  -> Some `Fragment
      | "eliom.injection" -> Some `Injection
      | _ -> None

    let section {Location.txt} = match txt with
      | "eliom.client" -> Some `Client
      | "eliom.server" -> Some `Server
      | "eliom.shared" -> Some `Shared
      | _ -> None

    let rec map f : attributes -> attributes = function
      | [] -> []
      | (id, payload) as h :: t -> match get id with
        | Some x -> (id, f x payload) :: map f t
        | None -> h :: map f t

  end



  (* C style return type, so elegant! *)
  let rec remove_section_attr r = function
    | [] -> []
    | h :: t ->
        match Attr.section (fst h) with
        | None -> h :: remove_section_attr r t
        | Some _ as x -> r := x ; t

  let get_section_side str =
    let r = ref None in
    let str' = map_mod_attr (remove_section_attr r) str in
    str', !r


  (** Utilities on fragments and injections *)

  type eliom_expr_content = {
    attrs : attributes ;
    id : string ;
    expr : expression
  }

  type eliom_expr =
    | Expr of expression
    | Fragment of eliom_expr_content
    | Injection of eliom_expr_content

  (* Take an expression and figure out if it's a fragment, an injection,
     or a normal expression.
  *)
  let unfold_expression expr =
    let build attrs expr payload =
      let open Parsetree in match payload with
      | PStr [{pstr_desc=Pstr_eval
                   ({pexp_desc=Pexp_constant Pconst_string (id,_)},[])}] ->
          { attrs ; id ; expr }
      | _ -> Location.raise_errorf ~loc:expr.exp_loc
            "Eliom ICE: Malformed identifier."
    in
    let rec aux acc = function
      | [] -> Expr {expr with exp_attributes = List.rev acc}
      | (a, payload)::t when Attr.get a = Some `Fragment ->
          Fragment (build (List.rev acc) {expr with exp_attributes = t} payload)
      | (a, payload)::t when Attr.get a = Some `Injection ->
          Injection (build (List.rev acc) {expr with exp_attributes = t} payload)
      | h :: t -> aux (h::acc) t
    in aux [] expr.exp_attributes

  module Collect = struct

    module H = Hashtbl.Make(struct
        include String
        let hash = Hashtbl.hash
      end)

    module Make () = struct
      let tbl = H.create 17

      include TypedtreeIter.MakeIterator(struct
          include TypedtreeIter.DefaultIteratorArgument
          let enter_expression e =
            match unfold_expression e with
            | Expr _ -> ()
            | Injection x -> H.replace tbl x.id x
            | Fragment _ -> assert false
        end)
    end

    let escaped e =
      let module I = Make () in
      I.iter_expression e ;
      H.fold (fun _k v l -> v::l) I.tbl []

    let injections stri =
      let module I = Make () in
      I.iter_structure_item stri ;
      H.fold (fun _k v l -> v::l) I.tbl []

    let fragments str =
      let frags = H.create 17 in
      let module Iter = TypedtreeIter.MakeIterator(struct
          include TypedtreeIter.DefaultIteratorArgument
          let enter_expression e =
            match unfold_expression e with
            | Expr _ -> ()
            | Injection _ -> ()
            | Fragment x -> H.replace frags x.id x
        end)
      in
      Iter.iter_structure_item str ;
      H.fold (fun _k v l -> v::l) frags []


  end

  module CollectMap = struct

    let make f =
      let expr mapper expr = match unfold_expression expr with
        | Injection x -> f x
        | _ ->
            U.default_mapper.expr mapper expr
      in
      {U.default_mapper with expr}

    let escaped f e =
      let m = make f in
      m.expr m e

    let injections f e =
      let m = make f in
      m.structure_item m e
  end

  (** Identifiers generation. *)
  (* This is a global pass on the complete typedtree. It stores each injections,
     escaped values and fragments in a map with a new generated identifier.
     Fragments and injections ids are global to the file (plus the file hash).
     Escaped values are local to the fragment.

     The comparison function is tuned to share identical identifiers. It doesn't
     share identical expressions (due to potential side effects).
  *)
  module Name = struct
    module M = Map.Make(struct
        type t = expression
        let compare x y = match x.exp_desc ,y.exp_desc with
          | Texp_ident (p1,_,_), Texp_ident (p2,_,_) -> compare p1 p2
          | _ -> compare x y (* Not recursive! *)
      end )

    module Map = struct
      type t = { i : int ; map : (int * string) M.t }
      let empty = { i = 0 ; map = M.empty }

      let add make expr {i; map} =
        if M.mem expr map
        then snd @@ M.find expr map, {i ; map}
        else
          let hash = str_file_hash expr.exp_loc in
          let s = make hash i in
          let v = (i,s) in
          let i = i + 1 in
          s, {i ; map = M.add expr v map }
    end

    let escaped_ident_fmt : _ format6 =
      "_eliom_escaped_%d"

    let hash_fmt : _ format6 =
      "%s%s"

    let add_escaped =
      let make _ i = Printf.sprintf escaped_ident_fmt i in
      Map.add make

    let add_injection =
      let make _hash i = string_of_int i in
      Map.add make

    let add_fragment =
      let make hash i = Printf.sprintf "%s%d" hash i in
      Map.add make

    let make_injection_id ~loc txt =
      let hash = str_file_hash loc in
      Exp.constant ~loc @@ Const.string (Printf.sprintf hash_fmt hash txt)

    (* This implementation is complicated by the fact that typedtree iterators
       are not functional. We need to maintain references and stack manually.
    *)
    (* This is probably buggy for multi nested frag/inj. TODO *)
    let annotate str =
      let fragmap = ref Map.empty in
      let injmap = ref Map.empty in
      let escmaps = Stack.create () in
      let add_frag frag =
        Stack.push Map.empty escmaps ;
        let s, m = add_fragment frag !fragmap in fragmap := m ; s
      in
      let add_inj inj =
        if Stack.is_empty escmaps then
          let s, m = add_injection inj !injmap in injmap := m ; s
        else
          let s, m = add_escaped inj @@ Stack.pop escmaps in
          Stack.push m escmaps ; s
      in
      let annotate e x _ = match x with
        | `Injection ->
            Parsetree.PStr [Str.eval (Exp.constant (Const.string @@ add_inj e))]
        | `Fragment ->
            Parsetree.PStr [Str.eval (Exp.constant (Const.string @@ add_frag e))]
      in

      let module M = TypedtreeMap.MakeMap(struct
          include TypedtreeMap.DefaultMapArgument

          let enter_expression e =
            {e with exp_attributes = Attr.map (annotate e) e.exp_attributes}

          let leave_expression e =
            let f x p = match x with
              | `Fragment -> ignore @@ Stack.pop escmaps ; p
              | `Injection -> p
            in
            ignore @@ Attr.map f e.exp_attributes ; e
        end)
      in
      M.map_structure str

  end
end

module Client = struct
  open Untyp

  (** Server sections *)

  let collect_escaped e =
    let tbl = Hashtbl.create 8 in
    let f {id = txt; attrs} =
      let loc = e.exp_loc in
      let lid = Location.mkloc (Longident.Lident txt) loc in
      Hashtbl.replace tbl txt {Location.txt;loc} ;
      Exp.ident ~loc ~attrs lid
    in
    let x = CollectMap.escaped f e in
    let l = Hashtbl.fold (fun _k v l -> v::l) tbl [] in
    x, l

  let register_client_closure {id; expr} =
    let e, args = collect_escaped expr in
    let loc = e.Parsetree.pexp_loc in
    let id = Exp.constant ~loc @@ Const.string id in
    let f lid = Pat.var ~loc lid in
    let args = ptuple ~loc (List.map f args) in
    (* [%expr *)
    (*   Eliom_runtime.register_client_closure [%e id] *)
    (*     (fun [%p args] -> [%e e]) *)
    (* ][@metaloc loc] *)
    app ~loc "Eliom_runtime.register_client_closure"
      [ id ; Exp.fun_ ~loc Nolabel None args e ]

  let client_closures ~loc str =
    let frags = Collect.fragments str in
    let l = List.map register_client_closure frags in
    match l with
    | [] -> []
    | l ->
        (* [%str let () = [%e make_sequence ~loc l ]][@metaloc loc] *)
        [unit_str ~loc (make_sequence ~loc l)]

  let server_section ~loc =
    let e_hash = file_hash loc in
    (* [%stri *)
    (*   let () = Eliom_runtime.close_server_section [%e e_hash] *)
    (* ][@metaloc loc] *)
    unit_str ~loc @@
    app ~loc "Eliom_runtime.close_server_section" [e_hash]


  (** Client sections *)

  let make_injection { id ; expr } =
    let loc = expr.exp_loc in
    let id = Name.make_injection_id ~loc id in
    (* [%expr Eliom_runtime.get_injection [%e id]][@metaloc loc] *)
    (* TODO Emit type annotation to constraint the type of the injection. *)
    app ~loc "Eliom_runtime.get_injection" [id]


  let open_client_section ~loc =
    let e_hash = file_hash loc in
    (* [%stri *)
    (*   let () = Eliom_runtime.open_client_section [%e e_hash] *)
    (* ][@metaloc loc] *)
    unit_str ~loc @@
    app ~loc "Eliom_runtime.open_client_section" [e_hash]

  let client_section ~loc stri = [
    open_client_section ~loc ;
    CollectMap.injections make_injection stri ;
  ]


  (** The mapper *)

  let structure_item mapper orig_stri =
    let loc = orig_stri.str_loc in
    let str_desc, side = get_section_side orig_stri.str_desc in
    let stri = {orig_stri with str_desc} in
    match declaration_kind str_desc, side with
    | `Module, _ -> [
        U.default_mapper.structure_item mapper orig_stri ;
      ]
    | _, (None | Some `Shared) ->
        Location.raise_errorf ~loc "Eliom ICE: Unspecified section."

    (* If a structure is only a type declaration, we can copy it directly. *)
    | `Type, Some (`Server | `Client) -> [
        Untyp.identity_stri orig_stri ;
      ]
    | `Section , Some `Server ->
        client_closures ~loc stri @ [ server_section ~loc ]
    | `Section, Some `Client ->
        client_section ~loc stri

  let structure mapper {str_items} =
    List.flatten @@ List.map (structure_item mapper) str_items

  let mapper =
    { U.default_mapper with
      structure ;
    }

  let structure = mapper.structure mapper

end

module Server = struct
  open Untyp

  let _get_client_fragment e =
    match get_attr eliom_fragment_attr e.exp_attributes with
    | Some PStr [{pstr_desc = Pstr_eval (e,_)}] -> Some e
    | Some _ -> Location.raise_errorf ~loc:e.exp_loc "Eliom ICE"
    | _ -> None

  (** Server sections *)

  let close_server_section ~loc =
    let e_hash = file_hash loc in
    (* [%stri *)
    (*   let () = Eliom_runtime.close_server_section [%e e_hash] *)
    (* ][@metaloc loc] *)
    unit_str ~loc @@
    app ~loc "Eliom_runtime.close_server_section" [e_hash]

  let server_section mapper stri =
    let loc = stri.str_loc in [
      U.default_mapper.structure_item mapper stri ;
      close_server_section ~loc
    ]

  (** Client sections *)

  let client_section mapper stri =
    let loc = stri.str_loc in
    let injs = List.map
        (fun {id;attrs;expr} ->
           let e = U.default_mapper.expr mapper expr in
           id, {e with pexp_attributes = e.pexp_attributes @ attrs} )
        (Collect.injections stri)
    in
    let f e (id, inj) =
      let loc = inj.Parsetree.pexp_loc in
      let id = Exp.constant ~loc @@ Const.integer id in
      let pos = position loc in
      (* [%expr *)
      (*   ([%e id], Eliom_runtime.Poly.make [%e inj], [%e pos]) :: [%e e] *)
      (* ][@metaloc loc] *)
      let (@::) h t =
        Exp.construct ~loc (mkloc (lid "::") loc) (Some (etuple ~loc [h; t]))
      in
      etuple loc [
        id ;
        app ~loc "Eliom_runtime.Poly.make" [ inj ] ;
        pos ;
      ] @:: e
    in
    let nil = Exp.construct ~loc (mkloc (lid "[]") loc) None in
    let l = List.fold_left f nil @@ injs in
    let e_hash = file_hash loc in
    (* [%stri *)
    (*   let () = Eliom_runtime.close_client_section [%e e_hash] [%e l] *)
    (* ][@metaloc loc] *)
    (* TODO lift injection before, with a let-and declaration, to preserve evaluation order. *)
    [ unit_str ~loc @@
      app ~loc "Eliom_runtime.close_client_section" [e_hash ; l]
    ]

  (** Fragments *)

  let fragment ~loc id arg =
    (* [%expr *)
    (*   Eliom_runtime.fragment *)
    (*     ~pos:[%e position loc ] *)
    (*     [%e id] *)
    (*     [%e arg] *)
    (* ][@metaloc loc] *)
    (* TODO Add a type annotation for the fragment. *)
    Exp.apply ~loc (elid ~loc "Eliom_runtime.fragment") [
      Labelled "pos", position loc ;
      Nolabel, id ;
      Nolabel, arg ;
    ]

  let expr mapper e =
    let loc = e.exp_loc in
    let aux = function
      | Expr e  -> U.default_mapper.expr mapper e
      | Injection _ -> Location.raise_errorf ~loc "Eliom ICE: Unexpected injection"
      | Fragment {id ; expr} -> begin
          let injs =
            List.rev_map
              (fun {expr} -> U.default_mapper.expr U.default_mapper expr)
              (Collect.escaped expr)
          in
          fragment ~loc (Exp.constant ~loc @@ Const.string id) (etuple ~loc injs)
        end
    in
    aux @@ unfold_expression e

  let structure_item mapper orig_stri =
    let loc = orig_stri.str_loc in
    let str_desc, side = get_section_side orig_stri.str_desc in
    let stri = {orig_stri with str_desc} in
    match declaration_kind str_desc, side with
    | `Module, _ -> [
        U.default_mapper.structure_item mapper orig_stri ;
      ]
    | _, (None | Some `Shared) ->
        Location.raise_errorf ~loc "Eliom ICE: Unspecified section."

    (* If a structure is only a type declaration, we can copy it directly. *)
    | `Type, Some (`Server | `Client) -> [
        Untyp.identity_stri orig_stri ;
      ]
    | `Section, Some `Server -> server_section mapper stri
    | `Section, Some `Client -> client_section mapper stri

  let structure mapper {str_items} =
    List.concat @@ List.map (structure_item mapper) str_items

  let mapper =
    { U.default_mapper with
      structure ;
      expr
    }


  let structure = mapper.structure mapper
end

type res = {
  server : Parsetree.structure ;
  client : Parsetree.structure ;
}

let untype tstr =
  let tstr = Untyp.Name.annotate tstr in
  let client = Client.structure tstr in
  let server = Server.structure tstr in
  { client ; server }
