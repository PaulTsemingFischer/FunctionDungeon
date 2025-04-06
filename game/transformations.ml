open Root
open Modifiers
open Engine.Utils

(**[apply_move state entity possible_move] moves an entity to [possible_move]
   relative to its current position. If the entity is not in the world when this
   is run, or the location is not empty, then this transformation will return an
   unchanged state*)

let apply_move (state : GameState.t) (entity : GameEntity.t)
    (move : possible_move) =
  let target_pos = add_vec2 entity.pos move in
  let world = GameState.get_world state in
  if not (GameWorld.mem_id world entity.id) then state
  else if GameWorld.mem_pos world target_pos then state
  else
    let updated_entity = GameEntity.set_pos entity target_pos in
    let updated_state =
      GameWorld.put_entity world updated_entity |> GameState.update_world state
    in
    GameState.add_event updated_state (Move (entity, entity.pos, target_pos))

(**[say priority state entity message] makes an entity say something (cosmetic
   effect for events)*)
let say (state : GameState.t) (entity : GameEntity.t) (message : string) =
  GameState.add_event state (Say (entity, message))

exception Entity_not_found of GameEntity.t

let apply_action_to (state : GameState.t) (entity : GameEntity.t)
    (action : Modifiers.action) =
  let world = GameState.get_world state in
  if GameWorld.query_id world entity.id = None then
    raise (Entity_not_found entity)
  else
    match action with
    | DealDamage x ->
        let updated_state =
          GameState.update_world state
            (GameWorld.put_entity world
               (GameEntity.update_stats entity
                  {
                    health = entity.stats.health -. x;
                    base_moves = entity.stats.base_moves;
                    base_actions = entity.stats.base_actions;
                  }))
        in
        GameState.add_event updated_state (ChangeHealth (entity, -.x))
    | ApplyFire x -> state

let generate_normal_room (state : GameState.t) (player : GameEntity.t) =
  let world =
    GameWorld.put_entity
      (GameWorld.put_entity GameWorld.empty (GameEntity.set_pos player (0, 0)))
      (create_default_at Door (-5, -5))
  in
  GameState.update_world state world
