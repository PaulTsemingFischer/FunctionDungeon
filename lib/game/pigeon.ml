open GameDefinitions
open Engine.Utils

let pigeon_action (state : GameState.t) (entity : GameEntity.t) _ =
  let possible_attacks =
    GameState.activate_action_modifiers state
      (to_entity_types entity.entity_type)
      (to_entity_stats entity.stats).base_actions
  in
  let nearby_entity_action_pairs :
      (GameEntity.t * Modifiers.possible_action) option =
    List.fold_left
      (fun player_opt (pos, attack) ->
        match
          GameWorld.query_pos (GameState.room state) (add_vec2 pos entity.pos)
        with
        | Some e -> (
            match to_entity_types (to_gameentity_type e).entity_type with
            | Player ->
                Some (to_gameentity_type e, (add_vec2 pos entity.pos, attack))
            | _ -> None)
        | None -> None)
      None possible_attacks
  in

  match nearby_entity_action_pairs with
  | Some (p, (_, a)) ->
      Transformations.apply_attack_to_entity state (to_gameworld_type p) a
  | None ->
      let updated_state, possible_moves =
        GameState.apply_move_modifiers state entity
          (to_entity_stats entity.stats).base_moves
      in
      let random_direction = random_element possible_moves in
      let target_position = add_vec2 random_direction entity.pos in
      if GameWorld.mem_pos (GameState.room updated_state) target_position then
        Transformations.say updated_state (to_gameworld_type entity)
          "<pigeon noises>"
      else
        Transformations.apply_move updated_state (to_gameworld_type entity)
          random_direction
