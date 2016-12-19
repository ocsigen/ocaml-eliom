
module%client M = struct
  type t = int
  let compare = compare
end

module%client N = Set.Make(M)



module%client F (A : Set.OrderedType) =
  module type T = module type of A
end

module type%client X = F(String).T
