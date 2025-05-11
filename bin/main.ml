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
  Transformations.generate_floor player Pgworld.default_room_gen_settings
    [ Transitions.entity_status_runner; Transitions.entity_action_runner ]

let setup () =
  Raylib.init_window 1000 1000 "raylib [core] example - basic window";
  Raylib.set_target_fps 60

let () =
  setup ();
  let state = generate_starting_state () in
  let renderer = Renderer.make_from_state state in
  Renderer.loop renderer state GameState.step
