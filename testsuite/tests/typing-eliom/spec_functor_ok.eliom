
module%client M = struct
  type t = int
  let compare = compare
end

module%client N = Set.Make(M)
