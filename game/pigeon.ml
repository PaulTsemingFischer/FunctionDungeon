open Root
open Engine.Utils

let pigeon_action (state : GameState.t) (entity : GameEntity.t) _ =
  let possible_attacks =
    GameState.apply_action_modifiers state entity.entity_type
      entity.stats.base_actions
  in
  let nearby_entity_action_pairs :
      (GameEntity.t * Modifiers.possible_action) option =
    List.fold_left
      (fun player_opt (pos, attack) ->
        match
          GameWorld.query_pos
            (GameState.get_world state)
            (add_vec2 pos entity.pos)
        with
        | Some e -> (
            match e.entity_type with
            | Player -> Some (e, (add_vec2 pos entity.pos, attack))
            | _ -> None)
        | None -> player_opt)
      None possible_attacks
  in

  match nearby_entity_action_pairs with
  | Some (p, (_, a)) -> Transformations.apply_attack_to_entity state p a
  | None ->
      let updated_state, possible_moves =
        GameState.activate_move_modifiers state entity entity.stats.base_moves
      in
      let random_direction = random_element possible_moves in
      let target_position = add_vec2 random_direction entity.pos in
      if GameWorld.mem_pos (GameState.get_world updated_state) target_position
      then Transformations.say updated_state entity "<pigeon noises>"
      else Transformations.apply_move updated_state entity random_direction
