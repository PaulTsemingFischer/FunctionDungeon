open Root
open Engine.Utils
open Transformations

type GameState.input += MovePlayer of vec2

let player_action (state : GameState.t) (entity : GameEntity.t) input =
  match input with
  | MovePlayer dir ->
      let target_pos = add_vec2 entity.pos dir in
      if GameWorld.mem_pos state.world target_pos then
        raise (GameState.Invalid_input input)
      else move_entity state entity target_pos
  | _ -> state
