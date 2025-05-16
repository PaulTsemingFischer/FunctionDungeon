open Engine.Utils

type enemy =
  | Jailer of int * int
    (* fence player in r radius from their current position for t turns *)
  | Thief
  (* Take's one of the player's action modifiers *)
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
  | Variable_Range_and_Damage (r, d) ->
      "enemy: range=" ^ string_of_int r ^ " damage=" ^ Printf.sprintf "%.2f" d

let jailer_small = Jailer (3, 5)
let jailer_medium = Jailer (5, 10)
let jailer_large = Jailer (7, 12)
let small_fog_cloud = Fog_Cloud (4, 140)
let large_fog_cloud = Fog_Cloud (8, 520)
