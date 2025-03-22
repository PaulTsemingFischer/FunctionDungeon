open Root
open Engine.Utils
open Transformations

let pigeon_action_generator (state : GameState.t) (entity : GameEntity.t) _ :
    GameState.transition option =
  print_endline "Pigeon Action!";
  let random_direction : vec2 = (1 - Random.int 3, 1 - Random.int 3) in
  let target_pos = add_vec2 random_direction entity.pos in
  if GameWorld.mem_pos state.world target_pos then
    Some (say 1 state entity "<LOUD pigeon noises>")
  else
    Some
      (Transformations.say 1 state entity "<pigeon noises>"
      <& move_entity 1 state entity target_pos)
