[@@@ocaml.warning "+a-4-9-40-42"]
open Eliom_base

val cmi_magic_number : string

val translate : loc -> Types.signature -> unit

val is_mixed : Types.signature -> bool

module Specialize : sig

  val modtype_declaration :
    scope:side -> id:side ->
    Types.modtype_declaration -> Types.modtype_declaration

  val module_declaration :
    scope:side -> id:side ->
    Types.module_declaration -> Types.module_declaration

end
