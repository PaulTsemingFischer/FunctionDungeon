open Root
open Engine.Utils

let pigeon_action (state : GameState.t) (entity : GameEntity.t) _ =
  let updated_state, possible_moves =
    GameState.activate_move_modifiers state entity entity.stats.base_moves
  in
  let random_direction = random_element possible_moves in
  let target_position = add_vec2 random_direction entity.pos in
  if GameWorld.mem_pos (GameState.get_world updated_state) target_position then
    Transformations.say updated_state entity "<pigeon noises>"
  else Transformations.apply_move updated_state entity random_direction
