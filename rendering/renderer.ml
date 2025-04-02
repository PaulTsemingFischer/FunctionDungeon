open Game
open Game.Root
open Raylib
open Engine.Utils

type renderable = {
  source_entity : GameEntity.t;
  rendered_pos : vec2f;
}

module ComparableRenderable = struct
  type t = renderable

  let compare (r1 : renderable) (r2 : renderable) =
    GameEntity.compare r1.source_entity r2.source_entity
end

module RenderableSet = Set.Make (ComparableRenderable)

type t = {
  renderables : RenderableSet.t;
  source_state : GameState.t;
}

type input_handler = GameState.t -> GameState.input -> GameState.t

let update_render_state (renderer : t) (game_state : GameState.t) =
  {
    renderables =
      GameWorld.all_entities game_state.world
      |> List.fold_left
           (fun (updated_renderer : RenderableSet.t) (entity : GameEntity.t) ->
             let renderer_opt =
               RenderableSet.find_first_opt
                 (fun (current_renderable : renderable) ->
                   current_renderable.source_entity.id >= entity.id)
                 updated_renderer
             in
             match renderer_opt with
             | None ->
                 RenderableSet.add
                   { source_entity = entity; rendered_pos = (0., 0.) }
                   updated_renderer
             | Some existing_renderable ->
                 let temp_renderer =
                   RenderableSet.remove existing_renderable updated_renderer
                 in
                 RenderableSet.add
                   {
                     source_entity = entity;
                     rendered_pos = existing_renderable.rendered_pos;
                   }
                   temp_renderer)
           renderer.renderables;
    source_state = renderer.source_state;
  }

let font_scaling_factor = 32.0

let tick (renderer : t) =
  {
    renderables =
      RenderableSet.map
        (fun current_renderable ->
          {
            source_entity = current_renderable.source_entity;
            rendered_pos =
              lerp_vec current_renderable.rendered_pos
                (vec2f_of_vec2 current_renderable.source_entity.pos)
                0.5;
          })
        renderer.renderables;
    source_state = renderer.source_state;
  }

let draw_frame w =
  Raylib.draw_rectangle_lines_ex
    (Raylib.Rectangle.create 0. 0.
       (float_of_int (Raylib.get_screen_width ()))
       (float_of_int (Raylib.get_screen_height ())))
    (w *. 2.) Color.black

let render (renderer : t) =
  Raylib.begin_drawing ();
  Raylib.clear_background Color.white;
  RenderableSet.to_list renderer.renderables
  |> List.iter (fun (r : renderable) ->
         let screen_space_position =
           ( float_of_int (Raylib.get_screen_width () / 2),
             float_of_int (Raylib.get_screen_height () / 2) )
           |> add_vec2f
                (mul_vec2f
                   (scale_vec2f r.rendered_pos font_scaling_factor)
                   (1.0, -1.0))
           |> add_vec2f
                (neg_vec2f
                   (font_scaling_factor /. 2.0, font_scaling_factor /. 2.0))
           |> vec2_of_vec2f
         in
         match r.source_entity.entity_type with
         | Player ->
             Raylib.draw_text "@"
               (fst screen_space_position)
               (snd screen_space_position)
               (int_of_float font_scaling_factor)
               Color.black
         | Pigeon _ ->
             Raylib.draw_text "p"
               (fst screen_space_position)
               (snd screen_space_position)
               (int_of_float font_scaling_factor)
               Color.black
         | Wall ->
             Raylib.draw_text "#"
               (fst screen_space_position)
               (snd screen_space_position)
               (int_of_float font_scaling_factor)
               Color.black);
  draw_frame 80.;
  Raylib.end_drawing ()

let rec loop_aux (renderer : t) (game_state : GameState.t)
    (input_handler : input_handler) =
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    let frame_input_opt =
      match Raylib.get_key_pressed () with
      | Raylib.Key.W -> Some (GameState.MovePlayer (0, 1))
      | Raylib.Key.A -> Some (MovePlayer (-1, 0))
      | Raylib.Key.S -> Some (MovePlayer (0, -1))
      | Raylib.Key.D -> Some (MovePlayer (1, 0))
      | _ -> None
    in
    match frame_input_opt with
    | None ->
        let updated_renderer = tick renderer in
        render updated_renderer;
        loop_aux updated_renderer game_state input_handler
    | Some input -> (
        try
          let updated_game_state = input_handler game_state input in
          let updated_renderer =
            update_render_state renderer updated_game_state
          in
          let ticked_renderer = tick updated_renderer in
          render ticked_renderer;
          loop_aux ticked_renderer updated_game_state input_handler
        with GameState.Invalid_input _ ->
          let updated_renderer = tick renderer in
          render updated_renderer;
          loop_aux updated_renderer game_state input_handler)

let loop (renderer : t) (game_state : GameState.t)
    (input_handler : input_handler) =
  Raylib.init_window 1000 1000 "raylib [core] example - basic window";
  Raylib.set_target_fps 60;
  loop_aux renderer game_state input_handler

let make_from_state (game_state : GameState.t) =
  {
    renderables =
      GameWorld.all_entities game_state.world
      |> List.map (fun (entity : GameEntity.t) ->
             { source_entity = entity; rendered_pos = vec2f_of_vec2 entity.pos })
      |> RenderableSet.of_list;
    source_state = game_state;
  }
