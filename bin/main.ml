open Engine
open Engine.Utils
open Game
open Game.GameDefinitions
open Game.Transitions
open Procgen
open Raylib
open Rendering

let generate_starting_state () =
  let player = create_default_at Player (0, 0) in
  let world = GameWorld.put_entity GameWorld.empty player in
  let state = GameState.create [world] [ entity_action_runner ] in
  let generated_state = Transformations.generate_normal_room state player in
  GameState.add_moves_modifier generated_state (ScaleMove 1) Pigeon
  |> GameState.query_update_player

let setup () =
  Raylib.init_window 1000 1000 "raylib [core] example - basic window";
  Raylib.set_target_fps 60

let () =
  let state = generate_starting_state () in
  let renderer = Renderer.make_from_state state in
  Renderer.loop renderer state GameState.step
