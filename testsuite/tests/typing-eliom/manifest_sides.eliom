
module type S = sig
  module M : sig
    type x = A
  end

  module%client M2 : sig
    type x = M.x
  end
end

module M : S = struct

  module M = struct
    type x = A
  end

  module%client M2 = struct
    type x = M.x
  end
end
