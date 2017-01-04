[@@@ocaml.warning "+a-4-9-40-42"]

val cmi_magic_number : string

val translate : Eliom_base.loc -> Types.signature -> unit

val is_mixed : Types.signature -> bool

module Specialize : sig

  type 'a t = ?idside:Eliom_base.side -> 'a -> 'a

  val modtype : Types.module_type t
  val modtype_declaration : Types.modtype_declaration t
  val module_declaration : Types.module_declaration t
  val class_declaration : Types.class_declaration t
  val class_type_declaration : Types.class_type_declaration t

  val path' : scope:Eliom_base.side -> Path.t -> Path.t

  val ident : Ident.t -> Ident.t
  val path : Path.t -> Path.t
end

val printer_path : (Format.formatter -> Path.t -> unit) ref
val printer_modtype : (Format.formatter -> Types.module_type -> unit) ref
val printer_modtype_decl : (Ident.t -> Format.formatter -> Types.modtype_declaration -> unit) ref
val printer_module_decl : (Ident.t -> Format.formatter -> Types.module_declaration -> unit) ref
