
type side = [
  | `Client
  | `Server
]

type shside = [
  | side
  | `Shared
]

val conform : shside -> shside -> bool

val in_side : [<shside] -> (unit -> 'a) -> 'a
val get_side : unit -> [>shside]

val is_fragment : Parsetree.expression -> bool
val get_fragment : Parsetree.expression -> Parsetree.expression

val is_injection : Parsetree.expression -> bool
val get_injection : Parsetree.expression -> Parsetree.expression

val is_section : Parsetree.structure_item -> bool
val get_section : Parsetree.structure_item -> (side * Parsetree.structure_item)


val fragment : Longident.t
val fragment_attr : Location.t -> Parsetree.attribute
