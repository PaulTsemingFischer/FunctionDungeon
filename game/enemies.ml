open Root
open Engine.Utils

type enemy =
  | Jailer of int * int
    (* fence player in r radius from their current position for t turns *)
  | Thief (* Randomly take one of player's items *)
  | Blinder of
      int (* player can't see any of the board/must go by memory for t turns *)
  | Fog_Cloud of int * int
(* player can't see more than r radius around current position for t turns *)

let string_of_enemy (e : enemy) =
  match e with
  | Jailer (r, t) -> "jailer"
  | Thief -> "thief"
  | Blinder t -> "blinder"
  | Fog_Cloud (r, t) -> "fog cloud"

let enemy_action (state : GameState.t) (entity : GameEntity.t)
    (enemy_type : enemy) _ =
  let random_direction : vec2 = (1 - Random.int 3, 1 - Random.int 3) in
  let target_pos = add_vec2 random_direction entity.pos in
  match enemy_type with
  | Jailer (r, t) ->
      if GameWorld.mem_pos state.world target_pos then
        Transformations.move_entity state entity target_pos
      else
        let state_1 = Transformations.say state entity "<jailer noises>" in
        Transformations.move_entity state_1 entity target_pos
  | Thief ->
      if GameWorld.mem_pos state.world target_pos then
        Transformations.move_entity state entity target_pos
      else
        let state_1 = Transformations.say state entity "<thief noises>" in
        Transformations.move_entity state_1 entity target_pos
  | Blinder t ->
      if GameWorld.mem_pos state.world target_pos then
        Transformations.move_entity state entity target_pos
      else
        let state_1 = Transformations.say state entity "<blinder noises>" in
        Transformations.move_entity state_1 entity target_pos
  | Fog_Cloud (r, t) ->
      if GameWorld.mem_pos state.world target_pos then
        Transformations.move_entity state entity target_pos
      else
        let state_1 = Transformations.say state entity "<fog cloud noises>" in
        Transformations.move_entity state_1 entity target_pos
