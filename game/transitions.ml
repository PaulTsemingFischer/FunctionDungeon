open GameDefinitions

let entity_status_runner (state : GameState.t) (entity : GameWorld.e_t)
    (input : GameState.input) =
  let entity = to_gameentity_type entity in
  let filtered_statuses =
    List.filter
      (fun status ->
        match status with
        | Fire (_, x) -> x > 0)
      (List.map (fun x -> to_status_effects x) entity.statuses)
  in
  let decremented_statuses =
    List.map
      (fun status ->
        to_status_effect
          (match status with
          | Fire (dmg, x) -> Fire (dmg, x - 1)))
      filtered_statuses
  in
  let updated_entity = GameEntity.update_statuses entity decremented_statuses in
  let world = GameState.room state in
  let new_state =
    GameState.set_room state
      (GameWorld.put_entity world (to_gameworld_type updated_entity))
  in
  List.fold_left
    (fun state_acc status ->
      match status with
      | Fire (dmg, _) ->
          Transformations.apply_action_to state_acc
            (to_gameworld_type updated_entity)
            (DealFireDamage dmg))
    new_state
    (List.map (fun x -> to_status_effects x) decremented_statuses)

let entity_action_runner (state : GameState.t) (entity : GameWorld.e_t)
    (input : GameState.input) =
  let entity = to_gameentity_type entity in
  match GameDefinitions.to_entity_types entity.entity_type with
  | Player -> Player.player_action state entity input
  | Pigeon -> Pigeon.pigeon_action state entity input
  | Wall -> state
  | Rock -> state
  | Water -> state
  | Lava -> state
  | Fire -> state
  | Door _ -> state
  | Enemy e -> Enemyaction.enemy_action state entity e input
  | Obstacle o -> Obstacleaction.obstacle_action state entity o input
  | ModifierItem _ -> state
  | SpecialItem -> state
  | HealthItem _ -> state
  | _ -> state
