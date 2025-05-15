open GameDefinitions
open Engine.Utils
open Transformations

let player_action (state : GameState.t) (entity : GameEntity.t)
    (input : GameState.input) =
  match input with
  | MovePlayer dir -> (
      let target_pos = add_vec2 entity.pos dir in
      match GameWorld.query_pos (GameState.room state) target_pos with
      | Some e -> (
          match e.entity_type with
          | Wall | Rock | Water | Obstacle _ ->
              GameState.(raise (Invalid_input input))
          | Door (i, loc) ->
              let moved_state = GameState.move_room state i in
              GameState.set_room moved_state
                (GameWorld.put_entity
                   (GameState.room moved_state)
                   (GameEntity.set_pos (GameState.get_player state) loc))
          | ModifierItem m -> apply_pickup_move state entity dir
          | SpecialItem -> apply_pickup_move state entity dir
          | Lava | Fire -> apply_action_to state entity (DealDamage 1.)
          | _ -> apply_action_to state e (DealDamage 1.))
      | None -> apply_move state entity dir)
  | Wait -> state
  | Act target -> (
      let possible_actions =
        GameState.activate_action_modifiers state Player
          entity.stats.base_actions
      in
      let action_opt =
        List.find_opt
          (fun (pos, _) -> add_vec2 pos entity.pos = target)
          possible_actions
      in
      match action_opt with
      | None -> GameState.(raise (Invalid_input input))
      | Some (action_pos, actions) -> (
          let e_opt =
            GameWorld.query_pos (GameState.room state)
              (add_vec2 action_pos entity.pos)
          in
          match e_opt with
          | None -> GameState.(raise (Invalid_input input))
          | Some e -> apply_actions_to state e actions))
  | Attack ->
      apply_attack_to state entity.pos
        (GameState.activate_action_modifiers state Player
           Modifiers.base_cross_actions)
