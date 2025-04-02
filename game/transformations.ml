open Root
open Engine.Utils

(**[move_entity state entity] moves an entity to some location. If the entity is
   not in the world when this is run, or the location is not empty, then this
   transformation will return an unchanged state*)
let move_entity (state : GameState.t) (entity : GameEntity.t) (target : vec2) :
    GameState.t =
  if not (GameWorld.mem_id state.world entity.id) then state
  else if GameWorld.mem_pos state.world target then state
  else
    let updated_entity = GameEntity.set_pos entity target in
    let updated_world = GameWorld.put_entity state.world updated_entity in
    {
      world = updated_world;
      transitions = state.transitions;
      turn = state.turn;
      events = Move (entity, entity.pos, target) :: state.events;
      player = state.player;
    }

(**[say priority state entity message] makes an entity say something (cosmetic
   effect for events)*)
let say (state : GameState.t) (entity : GameEntity.t) (message : string) :
    GameState.t =
  {
    world = state.world;
    transitions = state.transitions;
    turn = state.turn;
    events = Say (entity, message) :: state.events;
    player = state.player;
  }
