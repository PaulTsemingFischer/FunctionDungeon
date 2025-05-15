open GameDefinitions
open Modifiers
open Engine.Utils
open Procgen

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

let apply_pickup_move (state : GameState.t) (entity : GameEntity.t)
    (move : possible_move) =
  let target_pos = add_vec2 entity.pos move in
  let world = GameState.room state in
  match GameWorld.query_pos world target_pos with
  | Some e -> (
      match e.entity_type with
      | ModifierItem m ->
          let item_removed_state =
            GameState.set_room state
              (GameWorld.remove_entity (GameState.room state) e.id)
          in
          let modifier_added_state =
            GameState.add_actions_modifier item_removed_state m
              entity.entity_type
          in
          let new_state =
            GameState.add_event modifier_added_state
              (PickUpModifier (entity, m))
          in
          apply_move new_state entity move
      | SpecialItem ->
          let item_removed_state =
            GameState.set_room state
              (GameWorld.remove_entity (GameState.room state) e.id)
          in
          let progress_incr_state =
            GameState.increment_progress item_removed_state
          in
          let new_state =
            GameState.add_event progress_incr_state
              (PickUpSpecial (entity, GameState.get_progress progress_incr_state))
          in
          apply_move new_state entity move
      | _ -> apply_move state entity move)
  | _ -> apply_move state entity move

let say (state : GameState.t) (entity : GameEntity.t) (message : string) =
  GameState.add_event state (Say (entity, message))

exception Entity_not_found of GameEntity.t

let apply_action_to (state : GameState.t) (entity : GameEntity.t)
    (action : Modifiers.action) =
  let world = GameState.room state in
  if GameWorld.query_id world entity.id = None then state
  else
    match action with
    | DealDamage x ->
        if is_killable_entity entity.entity_type then
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
              GameState.set_room state (GameWorld.remove_entity world entity.id)
            in
            GameState.add_event updated_state (EntityDeath entity)
          else
            let updated_state =
              GameState.set_room state
                (GameWorld.put_entity world updated_entity)
            in

            GameState.add_event updated_state (ChangeHealth (entity, -.x))
        else state
    | DealFireDamage x ->
        if is_killable_entity entity.entity_type then
          let updated_entity =
            GameEntity.update_stats entity
              {
                health = entity.stats.health -. x;
                base_moves = entity.stats.base_moves;
                base_actions = entity.stats.base_actions;
              }
          in
          let fire_state = GameState.add_event state (TakeFireDamage entity) in
          if updated_entity.stats.health <= 0. then
            let updated_state =
              GameState.set_room fire_state
                (GameWorld.remove_entity world entity.id)
            in
            GameState.add_event updated_state (EntityDeath entity)
          else
            let updated_state =
              GameState.set_room fire_state
                (GameWorld.put_entity world updated_entity)
            in

            GameState.add_event updated_state (ChangeHealth (entity, -.x))
        else state
    | ApplyFire (x, y) ->
        if is_killable_entity entity.entity_type then
          let updated_entity =
            GameEntity.update_statuses entity
              (GameDefinitions.Fire (x, y) :: entity.statuses)
          in
          let updated_state =
            GameState.set_room state (GameWorld.put_entity world updated_entity)
          in
          GameState.add_event updated_state (ApplyFire (entity, (x, y)))
        else state
    | BarrierAttack (r, o) -> GameState.build_barrier state world entity.pos r o
    | StealAttack -> GameState.remove_actions_modifier state entity.entity_type
    | FogAttack (r, f) -> GameState.add_event state (FogCloud (entity, r, f))

(**[apply_actions_to state entity actions] applies [actions] to [entity]*)
let apply_actions_to (state : GameState.t) (entity : GameEntity.t)
    (actions : Modifiers.action list) =
  print_endline "Apply";
  List.fold_left
    (fun cur_state cur_action ->
      let entity_opt =
        GameWorld.query_id (GameState.room cur_state) entity.id
      in
      match entity_opt with
      | None -> cur_state
      | Some cur_entity -> apply_action_to cur_state cur_entity cur_action)
    state actions

(**[normal_room state player] is a new entity world, tile world pair with the
   given player*)
let normal_room (player : GameEntity.t) generated_room =
  let world = GameWorld.empty in

  let tiles = GameTiles.empty in

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
          | Pgworld.Door (i, spawn_loc) ->
              ( GameWorld.put_entity acc_world
                  (create_default_at (Door (i, spawn_loc)) pos),
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
          | Pgworld.(Item (ScaleAction i)) ->
              ( GameWorld.put_entity acc_world
                  (create_default_at (ModifierItem (ScaleAction i)) pos),
                acc_tiles )
          | Pgworld.(Item (AddFire (f, i))) ->
              ( GameWorld.put_entity acc_world
                  (create_default_at (ModifierItem (AddFire (f, i))) pos),
                acc_tiles )
          | Pgworld.(Item (AddDamage f)) ->
              ( GameWorld.put_entity acc_world
                  (create_default_at (ModifierItem (AddDamage f)) pos),
                acc_tiles )
          | Pgworld.(Item AugmentToAdjacent) ->
              ( GameWorld.put_entity acc_world
                  (create_default_at (ModifierItem AugmentToAdjacent) pos),
                acc_tiles )
          | Pgworld.Player ->
              print_endline "Adding player";
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
  (entity_world, tile_world)

let generate_floor (player : GameEntity.t)
    (settings : Pgworld.room_gen_settings)
    (entity_action_runner :
      (GameState.t -> GameWorld.e_t -> GameState.input -> GameState.t) list) =
  let player_room_id, proc_gen = Pgworld.generate_floor settings in
  print_endline ("Generating " ^ string_of_int (List.length proc_gen) ^ " rooms");
  let real_rooms = List.map (normal_room player) proc_gen in
  let real_entities, real_tiles =
    (List.map fst real_rooms, List.map snd real_rooms)
  in
  print_endline "Floor generated";
  print_endline (Pgworld.string_of_genworld (List.nth proc_gen player_room_id));
  GameState.create real_entities real_tiles entity_action_runner player
    player_room_id

let rec apply_attack_to_entity (state : GameState.t) (entity : GameEntity.t)
    (effects : action list) =
  match effects with
  | [] -> state
  | h :: t -> (
      let updated_state = apply_action_to state entity h in
      let world = GameState.room updated_state in
      match GameWorld.query_id world entity.id with
      | None -> updated_state
      | Some e -> apply_attack_to_entity updated_state e t)

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
