[@@@ocaml.warning "+a-4-9-40-42"]
open Parsetree
open Ast_helper

type side = [
  | `Client
  | `Server
  | `Shared
]

type shside = [
  | side
  | `Noside
]

let to_string = function
  | `Server -> "server"
  | `Client -> "client"
  | `Shared -> "shared"
  | `Noside -> "none"

(** Check if identifier from side [id] can be used in scope [scope]. *)
let conform ~(scope:shside) ~(id:shside) = match scope, id with
  | `Server, `Server
  | `Client, `Client
  | (`Server | `Client | `Shared), `Shared
  | _, `Noside
    -> true
  | `Client, `Server
  | `Server, `Client
  | `Shared, (`Server | `Client)
  | `Noside, _
    -> false

let mirror = function
  | `Client -> `Server
  | `Server -> `Client
  | `Shared -> `Shared
  | `Noside -> `Noside

(** Handling of current side *)

let side : shside ref = ref `Noside
let get_side () = (!side : shside :> [>shside])
let change_side = function
  | "server" -> side := `Server
  | "client" -> side := `Client
  | "shared" -> side := `Shared
  | _ -> ()


(** In order to report exceptions with the proper scope, we wrap exceptions
    that cross side boundaries with a side annotation.

    The handling mechanism in {!Location} unwraps the exception transparently.
*)
exception Error of (shside * exn)

let in_side new_side body =
   let old_side = !side in
   side := (new_side : [<shside] :> shside ) ;
   try
    let r = body () in
    side := old_side; r
   with e ->
     let e' : exn = Error (!side, e) in
     side := old_side;
     raise e'

let () =
  let handler : exn -> _  = function
    | Error (side, exn) ->
        in_side side (fun () -> Location.error_of_exn exn)
    | _ -> None
  in Location.register_error_of_exn handler



let check ~loc mk_error side message =
  let current_side = get_side () in
  if not @@ conform ~scope:current_side ~id:side then
    raise @@ mk_error @@
    Location.errorf ~loc
      "%s are only allowed in a %s context, \
       but it is used in a %s context."
      message
      (to_string side)
      (to_string current_side)
  else ()

(** Load path utilities *)

let client_load_path = ref []
let server_load_path = ref []

let set_load_path ~client ~server =
  client_load_path := List.rev client ;
  server_load_path := List.rev server ;
  ()

let get_load_path () =
  match get_side () with
  | `Server -> !server_load_path
  | `Client -> !client_load_path
  | `Shared -> !Config.load_path
  | `Noside -> !Config.load_path

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

let is_annotation ~txt base =
  txt = base || txt = ("eliom."^base)


(** Parsetree inspection and emission. *)

module Fragment = struct

  let name = "client"
  let attr loc = ({Location.txt=name; loc},PStr [])

  let check e = match e.pexp_desc with
    | Pexp_extension ({txt},payload) when is_annotation ~txt name ->
        begin match payload with
        | PStr [{pstr_desc = Pstr_eval (_e,_attrs)}] -> true
        | _ -> false (* TODO: Report error *)
        end
    | _ -> false

  let get e = match e.pexp_desc with
    | Pexp_extension ({txt},PStr [{pstr_desc = Pstr_eval (e,attrs)}])
      when txt = name -> exp_add_attr ~attrs e
    | _ -> exp_error ~loc:e.pexp_loc "Eliom: Not a fragment."

end


module Injection = struct

  let op = "~%"

  let check e = match e.pexp_desc with
    | Pexp_apply ({pexp_desc = Pexp_ident {txt}}, args)
      when txt = Longident.Lident op ->
        begin match args with
        | [Nolabel, _] -> true
        | _ -> false (* TODO: Report error *)
        end
    | _ -> false

  let get e =  match e.pexp_desc with
    | Pexp_apply ({pexp_desc=Pexp_ident {txt}}, [Nolabel, e])
      when txt = Longident.Lident op -> e
    | _ -> exp_error ~loc:e.pexp_loc "Eliom: Not an injection."


  let name = "injection"
  let attr loc = ({Location.txt=name; loc},PStr [])

end

module Section = struct

  let client = "client"
  let server = "server"
  let shared = "shared"

  let check e = match e.pstr_desc with
    | Pstr_extension (({Location.txt},payload),_)
      when is_annotation ~txt client ||
           is_annotation ~txt server ||
           is_annotation ~txt shared ->
        begin match payload with
        | PStr [_str] -> true
        | _ -> false (* TODO: Report error *)
        end
    | _ -> false

  let get e = match e.pstr_desc with
    | Pstr_extension (({Location.txt},PStr [str]),_)
      when is_annotation ~txt client -> (`Client, str)
    | Pstr_extension (({Location.txt},PStr [str]),_)
      when is_annotation ~txt server -> (`Server, str)
    | Pstr_extension (({Location.txt},PStr [str]),_)
      when is_annotation ~txt shared -> (`Shared, str)
    (* TODO : Drop attributes *)
    | _ -> (`Server, str_error ~loc:e.pstr_loc "Eliom: Not a section")

  let attr side loc =
    let txt = match side with
      | `Client -> client
      | `Server -> server
      | `Shared -> shared
    in ({Location.txt; loc},PStr [])

end
