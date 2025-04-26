open GameDefinitions
open Engine.Utils
open Transformations

let player_action (state : GameState.t) (entity : GameEntity.t)
    (input : GameState.input) =
  match input with
  | MovePlayer dir -> (
      let target_pos = add_vec2 entity.pos dir in
      match GameWorld.query_pos (GameState.get_world state) target_pos with
      | Some e -> (
          match e.entity_type with
          | Wall | Rock | Water | Lava ->
              GameState.(raise (Invalid_input input))
          | Door -> generate_normal_room state entity
          | _ -> apply_action_to state e (DealDamage 1.))
      | None -> apply_move state entity dir)
  | Wait -> state
  | Attack ->
      apply_attack_to state entity.pos
        (GameState.activate_action_modifiers state Player
           Modifiers.base_cross_actions)
