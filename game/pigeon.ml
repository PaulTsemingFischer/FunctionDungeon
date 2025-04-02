open Root
open Engine.Utils

let pigeon_action (state : GameState.t) (entity : GameEntity.t) _ =
  let random_direction = random_element entity.stats.base_moves in
  let target_position = add_vec2 random_direction entity.pos in
  if GameWorld.mem_pos (GameState.get_world state) target_position then
    Transformations.say state entity "<pigeon noises>"
  else Transformations.apply_move state entity random_direction
