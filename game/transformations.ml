open GameDefinitions
open Modifiers
open Engine.Utils
open Procgen

(**[apply_move state entity possible_move] moves an entity to [possible_move]
   relative to its current position. If the entity is not in the world when this
   is run, or the location is not empty, then this transformation will return an
   unchanged state*)

let apply_move (state : GameState.t) (entity : GameEntity.t)
    (move : possible_move) =
  let target_pos = add_vec2 entity.pos move in
  let world = GameState.room state in
  if not (GameWorld.mem_id world entity.id) then state
  else if GameWorld.mem_pos world target_pos then state
  else
    let updated_entity = GameEntity.set_pos entity target_pos in
    let updated_state =
      GameWorld.put_entity world updated_entity |> GameState.set_room state
    in
    GameState.add_event updated_state (Move (entity, entity.pos, target_pos))

(**[say priority state entity message] makes an entity say something (cosmetic
   effect for events)*)
let say (state : GameState.t) (entity : GameEntity.t) (message : string) =
  GameState.add_event state (Say (entity, message))

exception Entity_not_found of GameEntity.t

(**[apply_action_to state entity action] applies [action] to [entity], returning
   an updated [state] with the changed entity*)
let apply_action_to (state : GameState.t) (entity : GameEntity.t)
    (action : Modifiers.action) =
  let world = GameState.room state in
  if GameWorld.query_id world entity.id = None then
    raise (Entity_not_found entity)
  else
    match action with
    | DealDamage x -> (
        match entity.entity_type with
        | Wall | Door _ -> state
        | _ ->
            let updated_entity =
              GameEntity.update_stats entity
                {
                  health = entity.stats.health -. x;
                  base_moves = entity.stats.base_moves;
                  base_actions = entity.stats.base_actions;
                }
            in
            if updated_entity.stats.health <= 0. then
              let updated_state =
                GameState.set_room state
                  (GameWorld.remove_entity world entity.id)
              in
              GameState.add_event updated_state (EntityDeath entity)
            else
              let updated_state =
                GameState.set_room state
                  (GameWorld.put_entity world updated_entity)
              in

              GameState.add_event updated_state (ChangeHealth (entity, -.x)))
    | ApplyFire x -> state
    | BarrierAttack (r, o) -> GameState.build_barrier state world entity.pos r o
    | StealAttack -> GameState.remove_actions_modifier state entity.entity_type

(**[normal_room state player] is a new room with the given player*)
let normal_room (player : GameEntity.t) generated_room =
  let world = GameWorld.empty in

  let tiles = GameTiles.empty in

  print_endline (Pgworld.string_of_genworld generated_room);

  let source_entity_tile_pairs = Pgworld.to_tile_list generated_room in
  let entity_world, tile_world =
    List.fold_left
      (fun ((acc_world, acc_tiles) : GameWorld.t * GameTiles.t)
           (((ground, entity), pos) : Pgworld.tile * vec2) ->
        let updated_world, update_tiles =
          match entity with
          | Pgworld.Wall ->
              ( GameWorld.put_entity acc_world (create_default_at Wall pos),
                acc_tiles )
          | Pgworld.Door i ->
              ( GameWorld.put_entity acc_world (create_default_at (Door i) pos),
                acc_tiles )
          | Pgworld.Rock ->
              ( GameWorld.put_entity acc_world (create_default_at Rock pos),
                acc_tiles )
          | Pgworld.Water ->
              ( GameWorld.put_entity acc_world (create_default_at Water pos),
                acc_tiles )
          | Pgworld.Lava ->
              ( GameWorld.put_entity acc_world (create_default_at Lava pos),
                acc_tiles )
          | Pgworld.Fire ->
              ( GameWorld.put_entity acc_world (create_default_at Fire pos),
                acc_tiles )
          | Pgworld.(WeakMob Pigeon) ->
              ( GameWorld.put_entity acc_world (create_default_at Pigeon pos),
                acc_tiles )
          | Pgworld.Player ->
              ( GameWorld.put_entity acc_world (GameEntity.set_pos player pos),
                acc_tiles )
          | _ -> (acc_world, acc_tiles)
        in
        ( updated_world,
          match ground with
          | Mud -> GameTiles.put_entity update_tiles (create_tile_at Mud pos)
          | Ground ->
              GameTiles.put_entity update_tiles (create_tile_at Ground pos)
          | _ -> update_tiles ))
      (world, tiles) source_entity_tile_pairs
  in
  entity_world

(**[generate_world player settings] is a floor with the given [settings] and
   [player] *)
let generate_floor (player : GameEntity.t)
    (settings : Pgworld.room_gen_settings)
    (entity_action_runner :
      GameState.t -> GameWorld.e_t -> GameState.input -> GameState.t) =
  let player_room_id, proc_gen = Pgworld.generate_floor settings in
  let real_rooms = List.map (normal_room player) proc_gen in
  GameState.create real_rooms [ entity_action_runner ]

(** [apply_attack_to_entity] applies a single list of actions onto [entity] and
    returns the updated game state. *)
let rec apply_attack_to_entity (state : GameState.t) (entity : GameEntity.t)
    (effects : action list) =
  match effects with
  | [] -> state
  | h :: t -> apply_attack_to_entity (apply_action_to state entity h) entity t

(** [apply_attack_to state actions] applies all actions in [actions] to the game
    state [state]. Since attack coordinates are relative to player position, the
    value of [player_pos] determines the actual tiles affected. Attacks cannot
    hit the player itself; that is, any action on tile (0,0) will be skipped. *)
let rec apply_attack_to (state : GameState.t) (player_pos : vec2)
    (actions : possible_action list) =
  match actions with
  | [] -> state
  | h :: t -> (
      if fst h = (0, 0) then apply_attack_to state player_pos t
      else
        match
          GameWorld.query_pos (GameState.room state)
            (add_vec2 (fst h) player_pos)
        with
        | None -> apply_attack_to state player_pos t
        | Some x ->
            apply_attack_to
              (apply_attack_to_entity state x (snd h))
              player_pos t)
