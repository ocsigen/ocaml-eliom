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
