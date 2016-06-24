# Eliom compiler

This is an extension of the OCaml compiler to typecheck Eliom programs. This is WIP.

On `.ml` programs, the behavior is the same as the normal compiler.

In `eliom` programs, several extensions are available
which are quickly explained here: http://ocsigen.org/eliom/5.0/manual/ppx-syntax


```ocaml
let%server x = 3 (* declaration on the server *)

let%client y = 4 (* declaration on the client *)

let%server z : int fragment = [%client ~%y + 1 ]
(* piece of client code that is manipulated on the server *)

let%client a =
  ~%x (* Injections from the server *)
  + ~%z (* Injections from a fragment. *)
```
