
type foo

type bar = foo
type%server (_[@client]) frag
type%server s = foo frag

module type%server S = sig
  type t = bar frag
end

module%server M : S = struct
  type t = s
end

module type T = sig type t end
module%server M2 : T with type t = bar frag = struct
  type t = s
end
