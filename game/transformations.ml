open Root
open Engine.Utils

(**[move_entity priority state entity] creates a transformation with the given
   [priority] that moves an entity to some location. If the entity is not in the
   world when this is run, or the location is not empty, then this
   transformation will return an unchanged state*)
let move_entity (priorty : int) (state : GameState.t) (entity : GameEntity.t)
    (target : vec2) =
  ( priorty,
    fun (state : GameState.t) ->
      if not (GameWorld.mem_id state.world entity.id) then state
      else if GameWorld.mem_pos state.world target then state
      else
        let updated_entity = GameEntity.set_pos entity target in
        let updated_world = GameWorld.put_entity state.world updated_entity in
        {
          world = updated_world;
          generators = state.generators;
          turn = state.turn;
          events = state.events;
        } )
