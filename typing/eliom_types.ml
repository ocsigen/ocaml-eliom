[@@@ocaml.warning "+a-4-9-40-42"]
open Types
open Btype

(** New magic number for eliom-specific interfaces. *)
let cmi_magic_number = "Eliom000I020"
let () = assert
  (String.length cmi_magic_number = String.length Config.cmi_magic_number)

let make_iterator it_ident =
  let it_path p = List.iter it_ident @@ Path.heads p in
  let it_type_expr it ty =
    it.it_do_type_expr it ty ;
    let () = match ty.desc with
      | Tconstr (_, _, abbrev) ->
          iter_abbrev (it.it_type_expr it) !abbrev
      | _ -> ()
    in
    ()
  in
  { type_iterators with
    it_path ; it_ident ; it_type_expr ;
  }

let translate =
  let it_ident i = Ident.change_side (Eliom_base.get_side ()) i in
  let it = make_iterator it_ident in
  it.it_signature it

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
