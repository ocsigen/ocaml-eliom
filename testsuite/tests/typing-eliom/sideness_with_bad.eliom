module type S = sig
  type 'a[@client] x
end

module type SX =
  S with type 'a[@client] x = 'a
