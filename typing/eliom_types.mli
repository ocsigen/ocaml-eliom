[@@@ocaml.warning "+a-4-9-40-42"]

val cmi_magic_number : string

val translate : [< Eliom_base.shside] -> Types.signature -> unit

val is_mixed : Types.signature -> bool

module Lift : sig
  val modtype_decl :
    Eliom_base.shside -> Types.modtype_declaration -> Types.modtype_declaration
  val module_decl :
    Eliom_base.shside -> Types.module_declaration -> Types.module_declaration
  val module_type :
    Eliom_base.shside -> Types.module_type -> Types.module_type
end
