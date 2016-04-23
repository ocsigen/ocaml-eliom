(** Utilities for eliom typing.

    This file is *after* Types and Typedtree in the dependency chain.
*)

[@@@ocaml.warning "+a-4-9-40-42"]

val translate :
  Env.t ->
  [< Eliom_base.shside ] ->
  Types.type_expr -> (Types.type_expr, Path.t) result

(** Utilities to improve error messages.

    Used in {!Typetexp}.
*)
module Error_msg : sig

  val filter_add :
    Eliom_base.shside -> 'a -> Path.t option -> 'a list -> 'a list

  val injection :
    Format.formatter ->
    'a ->
    (('b -> Path.t option -> 'c -> 'c) -> 'a -> 'd -> 'e list -> 'f) ->
    'd -> 'b -> unit

end

(** Like {!Ast_helper} but for Types/Typedtree. *)
module Tast_helper : sig

  val add_stri_attr :
    Typedtree.attribute ->
    Typedtree.structure_item_desc -> Typedtree.structure_item_desc

  val add_sigi_attr :
    Parsetree.attribute ->
    Types.signature_item -> Types.signature_item

end
