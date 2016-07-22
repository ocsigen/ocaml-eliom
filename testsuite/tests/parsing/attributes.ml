[@@@foo]

let (x[@foo]) : unit [@foo] = ()[@foo]
  [@@foo]

type 'a[@foo] t =
  | Foo of ('a t[@foo]) [@foo]
[@@foo]

[@@@foo]


module M = struct
  type ('a[@foo], 'b) t = {
    l : (('a,'b) t [@foo]) [@foo]
  }
    [@@foo]
    [@@foo]

  [@@@foo]
end[@foo]
[@@foo]

module type S = sig

  include (module type of (M[@foo]))[@foo] with type ('a, 'b) t := ('a, 'b) M.t[@foo]
    [@@foo]

  [@@@foo]

end[@foo]
[@@foo]

[@@@foo]
