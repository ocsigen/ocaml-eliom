[@@@ocaml.warning "+a-4-9-40-42"]

(** Side utilities. *)

type side = [
  | `Client
  | `Server
  | `Shared
]

type shside = [
  | side
  | `Noside
]

val to_string : [<shside] -> string
val pp : Format.formatter -> [<shside] -> unit

(** [Check if identifier from side [id] can be used in scope [scope]. *)
val conform : scope:shside -> id:shside -> bool

val mirror : [<shside] -> [>shside]
val check :
  loc:Location.t ->
  (Location.error -> exn) -> shside -> string -> unit

val in_side : [<shside] -> (unit -> 'a) -> 'a
val get_side : unit -> [>shside]

(** Handling of client/server load path. *)

val change_side : string -> unit
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
  val check_sig : Parsetree.signature_item -> bool
  val get_sig : Parsetree.signature_item -> (side * Parsetree.signature)
  val attr : [<side] -> Location.t -> Parsetree.attribute
end
