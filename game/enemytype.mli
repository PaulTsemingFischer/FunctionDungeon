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

val string_of_enemy : enemy -> string
(** [string_of_enemy] is the string representation describing each type of enemy*)

(* Discrete enemies with set parameters *)

val jailer_small : enemy
(** A jailer with small radius (5) and duration (3 turns) *)

val jailer_medium : enemy
(** A jailer with medium radius (10) and duration (5 turns) *)

val jailer_large : enemy
(** A jailer with large radius (15) and duration (7 turns) *)

val double_range_enemy : enemy
(** An enemy that can attack with double range (2) *)

val long_range_enemy : enemy
(** An enemy that can attack with long range (5) *)

val double_damage_enemy : enemy
(** An enemy that deals double damage (2.0) *)

val super_damage_enemy : enemy
(** An enemy that deals super damage (5.0) *)

val slightly_extra_powerful_enemy : enemy
(** A slightly more powerful enemy that attacks with double range (2) and double
    damage (2.0) *)

val super_powerful_enemy : enemy
(** A super powerful enemy that attacks with long range (5) and increased damage
    (3.0) *)
