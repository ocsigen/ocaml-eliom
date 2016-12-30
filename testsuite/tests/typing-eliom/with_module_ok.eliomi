
module M : sig
  type m
  module N : sig
    type n
  end
end

module type MT = sig
  type m2
  module N : sig
    type n
  end
end

module type%client S = MT with module N = M.N
