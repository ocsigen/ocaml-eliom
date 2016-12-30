(** Test correctness of the sideness check and of the inclusion check *)

type foo

type (_[@client]) c
type (_[@server]) s

module type S = sig
  type 'a t = 'a
  type 'a tc = 'a * foo c
  type ('a[@client], 'b) tc' = 'a c * 'b * foo
  type ('a[@server], 'b) ts' = 'a s * 'b
  type ('a[@client], 'b[@client]) t'' = ('a * 'b) c
  type ('a[@client]) tx = 'a c s
end

module M : S = struct
  type 'a t = 'a
  type 'a tc = 'a * foo c
  type ('a[@client], 'b) tc' = 'a c * 'b * foo
  type ('a[@server], 'b) ts' = 'a s * 'b
  type ('a[@client], 'b[@client]) t'' = ('a * 'b) c
  type ('a[@client]) tx = 'a c s
end

module%client MC
  : sig include S end
  = struct include M end
module%server MS
  : sig include S end
  = struct include M end
