open Root
open Engine.Utils

(**[move_entity state entity] moves an entity to some location. If the entity is
   not in the world when this is run, or the location is not empty, then this
   transformation will return an unchanged state*)
let move_entity (state : GameState.t) (entity : GameEntity.t) (target : vec2) :
    GameState.t =
  let world = GameState.get_world state in
  if not (GameWorld.mem_id world entity.id) then state
  else if GameWorld.mem_pos world target then state
  else
    let updated_entity = GameEntity.set_pos entity target in
    let updated_state =
      GameWorld.put_entity world updated_entity |> GameState.update_world state
    in
    GameState.add_event updated_state (Move (entity, entity.pos, target))

(**[say priority state entity message] makes an entity say something (cosmetic
   effect for events)*)
let say (state : GameState.t) (entity : GameEntity.t) (message : string) =
  GameState.add_event state (Say (entity, message))
