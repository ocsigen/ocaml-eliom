[@@@ocaml.warning "+a-4-9-40-42"]

(** Compilation mode *)

type mode =
  | OCaml
  | Eliom
  | Client
  | Server

val set_mode : mode -> unit
val mode_of_string : string -> mode

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


val get_mode_as_side : unit -> [>shside]

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

val set_load_path : client:string list -> server:string list -> unit

(** Try to find the given file with the given extension.
    When [mode] is [OCaml] [Server] or [Client, behaves like
    {!Misc.find_in_path_uncap} with the appropriate path.

    When in [Eliom] mode, tries to magically find where the file lives.
    For example, [find_in_load_path "Foo" ".cmi"] when [get_side () = `Client]
    will try, in this order:
    - foo.cmo and Foo.cmo in main path (-I)
    - foo.cmo and Foo.cmo in client path (-client-I)
    - Foo.client.cmo and foo.client.cmo in main path (-I)
*)
val find_in_load_path : string -> ext:string -> string * shside

(** Error *)

val error :
  loc:Location.t ->
  ('a, Format.formatter, unit, unit, unit, 'b) format6 -> 'a

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
  val split : Parsetree.structure -> Parsetree.structure
  val check_sig : Parsetree.signature_item -> bool
  val get_sig : Parsetree.signature_item -> (side * Parsetree.signature)
  val attr : [<side] -> Location.t -> Parsetree.attribute
end

(* Compmisc utils *)
val client_include_dirs : string list ref
val server_include_dirs : string list ref

(** Sideness annotations. *)
module Sideness : sig

  type t = Same | Client

  val get : Parsetree.core_type -> t
  val gets : (Parsetree.core_type * _) list -> t list
  val wrap : t -> ('a -> 'b) -> 'a -> 'b

end
