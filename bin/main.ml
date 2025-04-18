open Engine
open Engine.Utils
open Game
open Game.Entities
open Game.Transitions
open Procgen
open Raylib
open Rendering

let generate_starting_state () =
  let player = create_default_at Player (0, 0) in
  let pigeon = create_default_at Pigeon (3, 3) in
  let wall = create_default_at Door (-5, -5) in
  let world =
    GameWorld.put_entity
      (GameWorld.put_entity
         (GameWorld.put_entity
            (Procgen.Rwalk.world_from_genworld
               (Rwalk.generate ~printing:false 20 20))
            pigeon)
         player)
      wall
  in
  let state = GameState.create world [ entity_action_runner ] in
  GameState.add_moves_modifier state (ScaleMove 1) Pigeon

let setup () =
  Raylib.init_window 1000 1000 "raylib [core] example - basic window";
  Raylib.set_target_fps 60

let () =
  let state = generate_starting_state () in
  let renderer = Renderer.make_from_state state in
  Renderer.loop renderer state GameState.step
