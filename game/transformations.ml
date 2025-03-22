open Root
open Engine.Utils

type GameState.event += | Move of GameEntity.t * vec2 * vec2 | Say of GameEntity.t * string

let string_of_event event =
  match event with
  | Move (entity, startpos, endpos) ->
      Printf.sprintf "Entity %s moved from %s to %s"
        (GameEntity.string_of_id entity.id)
        (string_of_vec startpos) (string_of_vec endpos)
  | Say (entity, message) ->
      Printf.sprintf "Entity %s says: %s"
        (GameEntity.string_of_id entity.id)
        message
  | _ -> failwith "event error: print unsupported event"

(**[compose transition1 transition2 ] composes two transformations with priority being that of [transition1]. Applies [transition2] then [transition1]*)
let ( <& ) (t1: GameState.transition) (t2: GameState.transition): GameState.transition = (fst t1, fun (state: GameState.t) -> (snd t1 )((snd t2) state))

(**[move_entity priority state entity] creates a transformation with the given
   [priority] that moves an entity to some location. If the entity is not in the
   world when this is run, or the location is not empty, then this
   transformation will return an unchanged state*)
let move_entity (priority : int) (state : GameState.t) (entity : GameEntity.t)
    (target : vec2) =
  ( priority,
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
          events = Move (entity, entity.pos, target) :: state.events;
        } )
(**[say priority state entity message] makes an entity say something (cosmetic effect for events)*)
let say (priority : int) (state : GameState.t) (entity : GameEntity.t)
    (message: string) =
  ( priority,
    fun (state : GameState.t) : GameState.t->
        {
          world = state.world;
          generators = state.generators;
          turn = state.turn;
          events = (Say (entity, message)) :: state.events;
        } )
