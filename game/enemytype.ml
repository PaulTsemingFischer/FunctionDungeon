open Engine.Utils

type enemy =
  | Jailer of int * int
    (* fence player in r radius from their current position for t turns *)
  | Thief
    (* Randomly take one of player's items; CURRENTLY JUST TAKING FIRST IN THE
       ACTIONS MODIFIER LIST BUT MAY CHANGE TO RANDOM LATER *)
  | Fog_Cloud of int * int
  (* player can't see more than r radius around current position for f frames *)
  | Variable_Range_and_Damage of
      int
      * float (* extra powerful enemy, attacks with d damage within r range *)

(** [string_of_enemy] is the string representation describing each type of enemy
*)
let string_of_enemy (e : enemy) =
  match e with
  | Jailer (r, t) -> "jailer"
  | Thief -> "thief"
  | Fog_Cloud (r, f) -> "fog cloud"
  | Variable_Range_and_Damage (r, d) -> "enemy"

let jailer_small = Jailer (3, 5)
let jailer_medium = Jailer (5, 10)
let jailer_large = Jailer (7, 12)
let small_fog_cloud = Fog_Cloud (5, 240)
let large_fog_cloud = Fog_Cloud (10, 720)
