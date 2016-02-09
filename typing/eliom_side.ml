[@@@ocaml.warning "+a-4-9-30-40-41-42"]
open Parsetree
open Ast_helper

type side = [
  | `Client
  | `Server
]

type shside = [
  | side
  | `Shared
]

let conform (s1:side) (s2:shside) = match s1, s2 with
  | `Server, `Server
  | `Client, `Client
  | (`Server | `Client), `Shared
    -> true
  | `Client, `Server
  | `Server, `Client -> false

(** Handling of current side *)

let side : side ref = ref `Server

let in_side new_side body =
   let old_side = !side in
   side := (new_side : [<side] :> side ) ;
   try
    let r = body () in
    side := old_side; r
   with e ->
   side := old_side; raise e

let get_side () = (!side : side :> [>side])

(** Utils *)
let exp_add_attr ~attrs e =
  {e with pexp_attributes = attrs @ e.pexp_attributes}

let error f ?sub ?loc =
  Format.ksprintf @@ fun s ->
  f ?loc ?attrs:None @@ Ast_mapper.extension_of_error @@
  Location.error ?loc ?sub s

let exp_error ?sub ~loc = error Exp.extension ?sub ~loc
let str_error ?sub ~loc = error Str.extension ?sub ~loc
(* let sig_error ?sub ~loc = error Sig.extension ?sub ~loc *)

let fragment_name = "eliom.fragment"

let is_fragment e = match e.pexp_desc with
  | Pexp_extension ({txt},payload) when txt = fragment_name ->
      begin match payload with
      | PStr [{pstr_desc = Pstr_eval (_e,_attrs)}] -> true
      | _ -> false (* TODO: Report error *)
      end
  | _ -> false

let get_fragment e = match e.pexp_desc with
  | Pexp_extension ({txt},PStr [{pstr_desc = Pstr_eval (e,attrs)}])
    when txt = fragment_name -> exp_add_attr ~attrs e
  | _ -> exp_error ~loc:e.pexp_loc "Eliom: Not a fragment."


let injection_op = "~%"

let is_injection e = match e.pexp_desc with
  | Pexp_apply ({pexp_desc = Pexp_ident {txt}}, args)
    when txt = Longident.Lident injection_op ->
      begin match args with
      | [Nolabel, _] -> true
      | _ -> false (* TODO: Report error *)
      end
  | _ -> false

let get_injection e =  match e.pexp_desc with
  | Pexp_apply ({pexp_desc=Pexp_ident {txt}}, [Nolabel, e])
    when txt = Longident.Lident injection_op -> e
  | _ -> exp_error ~loc:e.pexp_loc "Eliom: Not an injection."

let client_section = "eliom.client"
let server_section = "eliom.server"

let is_section e = match e.pstr_desc with
  | Pstr_extension (({Location.txt},payload),_)
    when txt = client_section || txt = server_section ->
      begin match payload with
      | PStr [_str] -> true
      | _ -> false (* TODO: Report error *)
      end
  | _ -> false

let get_section e = match e.pstr_desc with
  | Pstr_extension (({Location.txt},PStr [str]),_)
    when txt = client_section -> (`Client, str)
  | Pstr_extension (({Location.txt},PStr [str]),_)
    when txt = server_section -> (`Server, str)
  (* TODO : Drop attributes *)
  | _ -> (`Server, str_error ~loc:e.pstr_loc "Eliom: Not a section")


let fragment = Longident.parse "Eliom_runtime.fragment"

let fragment_attr loc = ({Location.txt=fragment_name; loc},PStr [])
