open Engine.Utils

type obstacle =
  | Spreading_Fire of int * int * int
  (* fire patch is currently centered at c with radius r and growing at g
     rate *)
  | Fence

let string_of_obstacle (o : obstacle) =
  match o with
  | Spreading_Fire (c, r, g) -> "spreading fire"
  | Fence -> "fence"

let add_obstacle_to_world state world pos obstacle_type =
  let new_obstacle = create_default_at Obstacle pos in
  GameState.update_world state (GameWorld.put_entity world new_obstacle)
