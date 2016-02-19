
type side = [
  | `Client
  | `Server
]

type shside = [
  | side
  | `Shared
]

val to_string : [<shside] -> string

val conform : shside -> shside -> bool
val mirror : [<shside] -> [>shside]
val check :
  loc:Location.t ->
  (Location.error -> exn) -> shside -> string -> unit

val in_side : [<shside] -> (unit -> 'a) -> 'a
val get_side : unit -> [>shside]

val set_load_path : client:string list -> server:string list -> unit
val get_load_path : unit -> string list

val is_fragment : Parsetree.expression -> bool
val get_fragment : Parsetree.expression -> Parsetree.expression

val is_injection : Parsetree.expression -> bool
val get_injection : Parsetree.expression -> Parsetree.expression

val is_section : Parsetree.structure_item -> bool
val get_section : Parsetree.structure_item -> (side * Parsetree.structure_item)


val fragment : Longident.t
val fragment_attr : Location.t -> Parsetree.attribute
val injection_attr : Location.t -> Parsetree.attribute
