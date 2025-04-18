open Entities

(**[entity_action_runner state entity input] executes the actions associated
   with some entity on the given [state]*)
let entity_action_runner (state : GameState.t) (entity : GameEntity.t)
    (input : GameState.input) =
  match entity.entity_type with
  | Player -> Player.player_action state entity input
  | Pigeon -> Pigeon.pigeon_action state entity input
  | Wall -> state
  | Door -> state
