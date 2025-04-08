open Root
open Engine.Utils

type enemy =
  | Jailer of int * int
    (* fence player in r radius from their current position for t turns *)
  | Thief (* Randomly take one of player's items *)
  | Blinder of
      int (* player can't see any of the board/must go by memory for t turns *)
  | Fog_Cloud of int * int
(* player can't see more than r radius around current position for t turns *)

let string_of_enemy (e : enemy) =
  match e with
  | Jailer (r, t) -> "jailer"
  | Thief -> "thief"
  | Blinder t -> "blinder"
  | Fog_Cloud (r, t) -> "fog cloud"

let positions_in_radius (center : vec2) (radius : int) : vec2 list =
  let center_x, center_y = center in
  let positions = ref [] in

  for x = center_x - radius to center_x + radius do
    for y = center_y - radius to center_y + radius do
      let pos = (x, y) in

      let dx = x - center_x in
      let dy = y - center_y in
      let distance_squared = (dx * dx) + (dy * dy) in

      if distance_squared <= radius * radius then positions := pos :: !positions
    done
  done;

  !positions

let build_barrier (state : GameState.t) (world : GameWorld.t) (center : vec2)
    (radius : int) (objects : Obstacles.obstacle) =
  let positions = positions_in_radius center radius in
  List.fold_left
    (fun current_state p ->
      Obstacles.add_obstacle_to_world current_state world p objects)
    state positions

let enemy_attack_type (e : enemy) =
  match e with
  | Jailer (r, t) -> "build_barrier (player's position) (r) (fence)"
  | Thief -> "thief"
  | Blinder t -> "build_barrier (player's position) (set radius) (blinder)"
  | Fog_Cloud (r, t) -> "fog clouds"
