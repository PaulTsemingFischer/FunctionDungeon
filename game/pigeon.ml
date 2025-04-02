open Root
open Engine.Utils

let pigeon_action (state : GameState.t) (entity : GameEntity.t) _ =
  let random_direction : vec2 = (1 - Random.int 3, 1 - Random.int 3) in
  let target_pos = add_vec2 random_direction entity.pos in
  if GameWorld.mem_pos (GameState.get_world state) target_pos then
    Transformations.move_entity state entity target_pos
  else
    let state_1 = Transformations.say state entity "<pigeon noises>" in
    Transformations.move_entity state_1 entity target_pos
