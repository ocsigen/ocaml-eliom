(** Utilities for eliom typing.

    This file is *after* Types and Typedtree in the dependency chain.
*)

[@@@ocaml.warning "+a-4-9-40-42"]

val translate :
  Eliom_base.loc ->
  Env.t -> Types.type_expr -> (Types.type_expr, Path.t) result

(** Utilities to improve error messages.

    Used in {!Typetexp}.
*)
module Error_msg : sig

  val filter_add :
    Eliom_base.side -> 'a -> Path.t option -> 'a list -> 'a list

  val injection :
    Format.formatter ->
    'a ->
    (('b -> Path.t option -> 'c -> 'c) -> 'a -> 'd -> 'e list -> 'f) ->
    'd -> 'b -> unit

end

(** Like {!Ast_helper} but for Types/Typedtree. *)
module Tast : sig

  val add_stri_attr :
    Typedtree.attribute ->
    Typedtree.structure_item_desc -> Typedtree.structure_item_desc

  val add_tsigi_attr :
    Parsetree.attribute ->
    Typedtree.signature_item -> Typedtree.signature_item

  val add_sigi_attr :
    Parsetree.attribute ->
    Types.signature_item -> Types.signature_item

end


val is_fragment : loc:Location.t -> env:Env.t -> Path.t -> bool
val fragment :
  loc:Location.t -> env:Env.t -> Types.type_expr -> Types.type_expr
