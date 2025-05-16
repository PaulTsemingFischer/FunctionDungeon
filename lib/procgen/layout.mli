type t = bool array array
(* [t] is a rectangular grid of booleans *)

val gen_layout: int -> t
(* [gen_layout n] is a layout with n connected squares being true and the rest false *)