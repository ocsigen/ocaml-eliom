[@@@ocaml.warning "+a-4-9-40-42"]

(** Possible locations *)
type loc =
  | Client
  | Server

(** Compilation mode *)
type mode =
  | OCaml
  | Eliom
  | OneSide of loc
  | Splitted of loc

val set_mode : mode -> unit
val mode_of_string : string -> mode

(** Side utilities. *)

(** Some code is either polymorphic on side, or at a specific location. *)
type side =
  | Loc of loc
  | Poly

val get_mode_as_side : unit -> side

(** String annotated with a side. *)
module SideString : sig
  type t = Consistbl.elt

  val to_string : t -> string
  val of_string : string -> t

  val compare : t -> t -> int

  val make : string -> side -> t
  val get: t -> string * side
end

(** Allow to replace StringSet when we want side annotations.
    Use a loose equivalence for side to register conflicts properly.
*)
module SideSet : Set.S with type elt = SideString.t

val to_string : side -> string
val of_string : string -> side
val pp : Format.formatter -> side -> unit

(** [Check if identifier from side [id] can be used in scope [scope]. *)
val conform : scope:side -> id:side -> bool

val mirror : side -> side
val check :
  loc:Location.t ->
  (Location.error -> exn) -> side -> string -> unit

val in_side : side -> (unit -> 'a) -> 'a
val in_loc : loc -> (unit -> 'a) -> 'a
val get_side : unit -> side

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
val find_in_load_path : string -> ext:string -> string * side

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
  val get : Parsetree.structure_item -> (loc * Parsetree.structure_item)
  val split : Parsetree.structure -> Parsetree.structure
  val check_sig : Parsetree.signature_item -> bool
  val get_sig : Parsetree.signature_item -> (loc * Parsetree.signature)
  val attr : loc -> Location.t -> Parsetree.attribute
end

(* Compmisc utils *)
val client_include_dirs : string list ref
val server_include_dirs : string list ref
