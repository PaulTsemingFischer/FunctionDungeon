open Engine.Utils

type enemy =
  | Jailer of int * int
    (* fence player in r radius from their current position for t turns *)
  | Thief
    (* Randomly take one of player's items; CURRENTLY JUST TAKING FIRST IN THE
       ACTIONS MODIFIER LIST BUT MAY CHANGE TO RANDOM LATER *)
  | Fog_Cloud of int * int
  (* player can't see more than r radius around current position for t turns *)
  | Variable_Range_and_Damage of
      int
      * float (* extra powerful enemy, attacks with d damage within r range *)

val string_of_enemy : enemy -> string
(** [string_of_enemy] is the string representation describing each type of enemy*)

(* Discrete enemies with set parameters *)

val jailer_small : enemy
val jailer_medium : enemy
val jailer_large : enemy
val small_fog_cloud : enemy
val large_fog_cloud : enemy
