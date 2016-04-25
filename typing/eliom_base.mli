[@@@ocaml.warning "+a-4-9-40-42"]

(** Side utilities. *)

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
val change_side : string -> unit

(** Handling of client/server load path. *)

val set_load_path : client:string list -> server:string list -> unit
val get_load_path : unit -> string list

(** Parsetree inspection and emission. *)

module Fragment : sig
  val check : Parsetree.expression -> bool
  val get : Parsetree.expression -> Parsetree.expression
  val attr : Location.t -> Parsetree.attribute
end

module Injection : sig
  val check : Parsetree.expression -> bool
  val get : Parsetree.expression -> Parsetree.expression
  val attr : Location.t -> Parsetree.attribute
end

module Section : sig
  val check : Parsetree.structure_item -> bool
  val get : Parsetree.structure_item -> (side * Parsetree.structure_item)
  val attr : [<shside] -> Location.t -> Parsetree.attribute
end
