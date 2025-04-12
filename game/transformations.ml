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

(**[apply_action_to state entity action] applies [action] to [entity], returning
   an updated [state] with the changed entity*)
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

(** [apply_attack_to_entity] applies a single list of actions onto [entity] and
    returns the updated game state. *)
let rec apply_attack_to_entity (state : GameState.t) (entity : GameEntity.t)
    (effects : action list) =
  match effects with
  | [] -> state
  | h :: t -> apply_attack_to_entity (apply_action_to state entity h) entity t

(** [apply_attack_to state actions] applies all actions in [actions] to the game
    state [state]. Since attack coordinates are relative to player position, the
    value of [player_pos] determines the actual tiles affected. *)
let rec apply_attack_to (state : GameState.t) (player_pos : vec2)
    (actions : possible_action list) =
  match actions with
  | [] -> state
  | h :: t -> (
      match
        GameWorld.query_pos
          (GameState.get_world state)
          (add_vec2 (fst h) player_pos)
      with
      | None -> state
      | Some x ->
          apply_attack_to (apply_attack_to_entity state x (snd h)) player_pos t)

(**[generate_normal_room state player] creates a new room with the given player*)
let generate_normal_room (state : GameState.t) (player : GameEntity.t) =
  let world =
    GameWorld.put_entity
      (GameWorld.put_entity GameWorld.empty player)
      (create_default_at Door
         (add_vec2
            ( random_element [ 1; -1 ] * (Random.int 3 + 1),
              random_element [ 1; -1 ] * (Random.int 3 + 1) )
            player.pos))
  in

  let room_radius = 5 + Random.int 7 in

  let updated_world =
    List.fold_left
      (fun (acc : GameWorld.t) (current_x : int) ->
        let pos_y =
          sqrt
            (float_of_int (room_radius * room_radius)
            -. float_of_int (current_x * current_x))
        in
        GameWorld.put_entity
          (GameWorld.put_entity acc
             (create_default_at Wall
                (add_vec2 player.pos
                   (current_x, int_of_float (Float.round pos_y)))))
          (create_default_at Wall
             (add_vec2 player.pos
                (current_x, -int_of_float (Float.round pos_y)))))
      world
      (List.init ((room_radius * 2) + 1) (fun x -> x - room_radius))
  in
  GameState.update_world state updated_world
