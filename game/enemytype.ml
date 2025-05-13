open Engine.Utils

type enemy =
  | Jailer of int * int
    (* fence player in r radius from their current position for t turns *)
  | Thief
    (* Randomly take one of player's items; CURRENTLY JUST TAKING FIRST IN THE
       ACTIONS MODIFIER LIST BUT MAY CHANGE TO RANDOM LATER *)
  | Blinder of
      int (* player can't see any of the board/must go by memory for t turns *)
  | Fog_Cloud of int * int
  (* player can't see more than r radius around current position for t turns *)
  | Variable_Range of int (* can attack within r range *)
  | Variable_Damage of float (* attacks with d damage *)
  | Variable_Range_and_Damage of
      int
      * float (* extra powerful enemy, attacks with d damage within r range *)

(** [string_of_enemy] is the string representation describing each type of enemy
*)
let string_of_enemy (e : enemy) =
  match e with
  | Jailer (r, t) -> "jailer"
  | Thief -> "thief"
  | Blinder t -> "blinder"
  | Fog_Cloud (r, t) -> "fog cloud"
  | Variable_Range r -> "variable range enemy"
  | Variable_Damage d -> "variable damage enemy"
  | Variable_Range_and_Damage (r, d) -> "variable range and damage enemy"

let jailer_small = Jailer (5, 3)
let jailer_medium = Jailer (10, 5)
let jailer_large = Jailer (15, 7)

let double_range_enemy = Variable_Range 2
let long_range_enemy = Variable_Range 5

let double_damage_enemy = Variable_Damage 2.
let super_damage_enemy = Variable_Damage 5.

let slightly_extra_powerful_enemy = Variable_Range_and_Damage (2,2.)
let super_powerful_enemy = Variable_Range_and_Damage (5, 3.)