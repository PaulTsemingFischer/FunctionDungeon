open GameDefinitions
open GameState
open Engine.Utils

(** [enemy_action] is the updated state after an [entity] of a certain enemy
    type [enemy] takes an action *)
let enemy_action (state : GameState.t) (entity : GameEntity.t)
    (enemy : Enemytype.enemy) _ =
  let possible_attacks = Modifiers.enemy_cross_actions enemy in
  let nearby_entity_action_pairs :
      (GameEntity.t * Modifiers.possible_action) option =
    List.fold_left
      (fun player_opt (pos, attack) ->
        match
          GameWorld.query_pos
            (GameState.room state)
            (add_vec2 pos entity.pos)
        with
        | Some e -> (
            match e.entity_type with
            | Player -> Some (e, (add_vec2 pos entity.pos, attack))
            | _ -> None)
        | None -> None)
      None possible_attacks
  in

  match nearby_entity_action_pairs with
  | Some (p, (_, a)) -> Transformations.apply_attack_to_entity state p a
  | None ->
      let updated_state, possible_moves =
        GameState.apply_move_modifiers state entity entity.stats.base_moves
      in
      let random_direction = random_element possible_moves in
      let target_position = add_vec2 random_direction entity.pos in
      if GameWorld.mem_pos (GameState.room updated_state) target_position
      then Transformations.say updated_state entity "<enemy noises>"
      else Transformations.apply_move updated_state entity random_direction
