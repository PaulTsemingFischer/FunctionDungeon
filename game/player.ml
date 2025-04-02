open Root
open Engine.Utils
open Transformations

let player_action (state : GameState.t) (entity : GameEntity.t) input =
  match input with
  | MovePlayer dir ->
      let target_pos = add_vec2 entity.pos dir in
      if GameWorld.mem_pos (GameState.get_world state) target_pos then
        raise (Invalid_input input)
      else move_entity state entity target_pos
  | Wait -> state
