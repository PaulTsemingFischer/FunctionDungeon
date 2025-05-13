open GameDefinitions

(** [entity_status_runner state entity input] applies the status effects
    associated with [entity] on the given [state]. *)
let entity_status_runner (state : GameState.t) (entity : GameEntity.t)
    (input : GameState.input) =
  let filtered_statuses =
    List.filter
      (fun status ->
        match status with
        | Fire x -> x > 0)
      entity.statuses
  in
  let decremented_statuses =
    List.map
      (fun status ->
        match status with
        | Fire x -> Fire (x - 1))
      filtered_statuses
  in
  let updated_entity = GameEntity.update_statuses entity decremented_statuses in
  let world = GameState.room state in
  let new_state =
    GameState.set_room state (GameWorld.put_entity world updated_entity)
  in
  List.fold_left
    (fun state_acc status ->
      match status with
      | Fire _ ->
          Transformations.apply_action_to state_acc updated_entity
            DealFireDamage)
    new_state decremented_statuses

(**[entity_action_runner state entity input] executes the actions associated
   with some entity on the given [state]*)
let entity_action_runner (state : GameState.t) (entity : GameEntity.t)
    (input : GameState.input) =
  match entity.entity_type with
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
  | HorizontalBouncer _ -> Bouncers.bouncer_action state entity input
  | ModifierItem _ -> state
