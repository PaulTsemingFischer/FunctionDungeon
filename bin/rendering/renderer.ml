open Game
open Raylib
open Engine.Utils
open GameDefinitions

type renderable = {
  source_entity : GameEntity.t;
  rendered_pos : vec2f;
}

(**[overlay] represents various graphical effects like text animations, damage
   indicators, etc. [TextRise (target, current, message, damage)] is a rising
   text overlay that is removed after [target] frames. Text color is red if
   [damage] is true and green otherwise *)
type overlay_type =
  | TextRise of string * bool
  | Circle of {
      radius : float;
      color : Color.t;
    }

type overlay = {
  duration : int;
  current : int;
  pos : vec2f;
  overlay_type : overlay_type;
}

module ComparableRenderable = struct
  type t = renderable

  let compare (r1 : renderable) (r2 : renderable) =
    GameEntity.compare r1.source_entity r2.source_entity
end

module RenderableSet = Set.Make (ComparableRenderable)

type mode =
  | Act
  | Default

type t = {
  renderables : RenderableSet.t;
  overlays : overlay list;
  source_state : GameState.t;
  camera : Raylib.Camera2D.t;
  camera_target : Vector2.t;
  mode : mode;
}

type input_handler = GameState.t -> GameState.input -> GameState.t

let screen_width = 1000
let screen_height = 800
let padding = 100
let tile_scaling_factor = 24.0
let ui_font_size = 24

let draw_text_right (value : string) (x : int) (y : int) (fontsize : int)
    (color : Color.t) =
  draw_text value (x - measure_text value fontsize) y fontsize color

let draw_metric (label : string) (value : string) (x : int) (y : int)
    (fontsize : int) (color : Color.t) =
  let metric_gap = 8 in
  let label_width = measure_text label ui_font_size in
  draw_text label x y fontsize (Color.create 255 255 255 100);
  draw_text value (x + label_width + metric_gap) y fontsize color

let draw_metric_right (label : string) (value : string) (x : int) (y : int)
    (fontsize : int) (color : Color.t) =
  let metric_gap = 8 in
  let label_width = measure_text label ui_font_size in
  let metric_width = measure_text value ui_font_size in
  draw_text label
    (x - label_width - metric_gap - metric_width)
    y fontsize
    (Color.create 255 255 255 100);
  draw_text value (x - metric_width) y fontsize color

let draw_ui (renderer : t) =
  let bottom_panel_height = 250 in
  let panel_padding = 32 in

  Raylib.draw_rectangle_lines_ex
    (Raylib.Rectangle.create 0. 0.
       (float_of_int (Raylib.get_screen_width ()))
       (float_of_int (Raylib.get_screen_height ())))
    (float_of_int padding) Color.black;

  Raylib.draw_rectangle 0
    (get_screen_height () - bottom_panel_height)
    (get_screen_width ()) (get_screen_height ()) Color.black;

  let action_modifiers, _ =
    GameState.get_modifiers renderer.source_state (to_entity_type Player)
  in

  (* action modifs *)
  List.iteri
    (fun idx modifier ->
      Raylib.draw_text
        Modifiers.(
          match modifier with
          | ScaleAction _ -> "x"
          | AddFire _ -> "F"
          | AddDamage _ -> "!"
          | AugmentToAdjacent -> "+")
        (padding + (24 * idx))
        60
        (int_of_float tile_scaling_factor)
        Color.white)
    action_modifiers;

  draw_metric "HEALTH: "
    (Printf.sprintf "%.2f"
       (to_entity_stats
          (to_gameentity_type (GameState.get_player renderer.source_state))
            .stats)
         .health)
    padding
    (get_screen_height () - (bottom_panel_height - panel_padding))
    ui_font_size Color.red;

  draw_metric_right "TURN: "
    (Printf.sprintf "%d" (GameState.get_turn renderer.source_state))
    (get_screen_width () - padding)
    (get_screen_height () - (bottom_panel_height - panel_padding))
    ui_font_size Color.white;

  let render_n_entities (entities : GameEntity.t list) (n : int) =
    let rec render_n_entities_aux (entities : GameEntity.t list) (n : int)
        (current : int) =
      if n <= current then ()
      else
        match entities with
        | [] -> ()
        | h :: t -> begin
            match to_entity_types h.entity_type with
            | Wall | Door _ -> render_n_entities_aux t n current
            | x -> (
                let entry_height =
                  get_screen_height ()
                  - (bottom_panel_height - (2 * panel_padding)
                    - ((current + 1) * ui_font_size))
                in
                Raylib.draw_text
                  (string_of_type (to_entity_types h.entity_type))
                  padding entry_height ui_font_size Color.white;
                render_n_entities_aux t n (current + 1);
                match to_entity_types h.entity_type with
                | Enemy _ | Pigeon ->
                    draw_metric_right "HEALTH: "
                      (Printf.sprintf "%.2f" (to_entity_stats h.stats).health)
                      (get_screen_width () - padding)
                      entry_height ui_font_size Color.red
                | _ -> ())
          end
    in
    render_n_entities_aux entities 5 0
  in
  let player = GameState.get_player renderer.source_state in
  let sorted_entities =
    List.sort
      (fun (e1 : GameEntity.t) (e2 : GameEntity.t) ->
        let e1_dist_from_player =
          length_squared (sub_vec2 e1.pos (to_gameentity_type player).pos)
        in
        let e2_dist_from_player =
          length_squared (sub_vec2 e2.pos (to_gameentity_type player).pos)
        in
        e1_dist_from_player - e2_dist_from_player)
      (List.map
         (fun x -> to_gameentity_type x)
         (GameWorld.all_entities (GameState.room renderer.source_state)))
  in

  render_n_entities sorted_entities 3

(* let render_n_events (event_list : (int * event) list) (n : int) =
    let rec render_n_events_aux (event_list : (int * event) list) (n : int)
        (current : int) =
      if n <= current then ()
      else
        match event_list with
        | [] -> ()
        | event_entry :: t ->
            let entry_height =
              get_screen_height ()
              - (bottom_panel_height - (2 * panel_padding)
                - ((current + 1) * ui_font_size))
            in

            Raylib.draw_text
              (string_of_event (snd event_entry))
              padding entry_height ui_font_size
              (Color.create 200 200 200 (max (200 - (200 / n * current)) 0));
            render_n_events_aux t n (current + 1);
            draw_metric_right "TURN: "
              (string_of_int (fst event_entry))
              (screen_width - padding) entry_height ui_font_size Color.gray
    in
    render_n_events_aux event_list n 0
  in *)
(* render_n_events (GameState.get_events renderer.source_state) 4 *)

let compute_camera_target ((x, y) : vec2) =
  Vector2.scale
    (Vector2.create (float_of_int x) (float_of_int (-y + 3)))
    tile_scaling_factor

(**[get_latest_events state] returns all events that occurred in the last turn*)
let get_latest_events (state : GameState.t) =
  List.filter
    (fun (turn, _) -> turn = GameState.get_turn state - 1)
    (GameState.get_events state)

(**[set_mode renderer mode] sets [renderer]'s mode to [mode]*)
let set_mode renderer mode =
  {
    renderables = renderer.renderables;
    source_state = renderer.source_state;
    camera = renderer.camera;
    camera_target = renderer.camera_target;
    overlays = renderer.overlays;
    mode;
  }

(**[update_render_state renderer state] updates the renderer to render the given
   [state]*)
let update_render_state (renderer : t) (entity_state : GameState.t) =
  let all_entitities = GameWorld.all_entities (GameState.room entity_state) in
  let filtered_renderables =
    RenderableSet.filter
      (fun renderable ->
        GameWorld.mem_id
          (GameState.room entity_state)
          (to_entity_id renderable.source_entity.id))
      renderer.renderables
  in
  (* print_newline (); *)
  (* print_endline (string_of_int (RenderableSet.cardinal filtered_renderables)); *)
  let updated_renderables =
    List.fold_left
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
      filtered_renderables
      (List.map (fun x -> to_gameentity_type x) all_entitities)
  in
  {
    renderables = updated_renderables;
    source_state = entity_state;
    camera = renderer.camera;
    camera_target =
      compute_camera_target
        (to_gameentity_type (GameState.get_player entity_state)).pos;
    overlays =
      renderer.overlays
      @ List.filter_map
          (fun (_, event) ->
            match event with
            | GameState.ChangeHealth (e, amt) ->
                Some
                  {
                    duration = 30;
                    current = 0;
                    pos = vec2f_of_vec2 e.pos;
                    overlay_type = TextRise (Printf.sprintf "%.2f" amt, amt < 0.);
                  }
            | GameState.FogCloud (entity, radius, frames) ->
                Some
                  {
                    duration = frames;
                    current = 0;
                    pos = vec2f_of_vec2 entity.pos;
                    overlay_type =
                      Circle
                        { radius = float_of_int radius; color = Color.gray };
                  }
            | _ -> None)
          (get_latest_events entity_state);
    mode = renderer.mode;
  }

let tick (renderer : t) =
  Camera2D.set_target renderer.camera
    (Vector2.lerp (Camera2D.target renderer.camera) renderer.camera_target 0.05);
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
    camera = renderer.camera;
    camera_target = renderer.camera_target;
    overlays =
      List.map
        (fun ovly : overlay ->
          {
            duration = ovly.duration;
            current = ovly.current + 1;
            pos = ovly.pos;
            overlay_type = ovly.overlay_type;
          })
        renderer.overlays
      |> List.filter (fun ovly -> ovly.current <= ovly.duration);
    mode = renderer.mode;
  }

let render_floor (renderer : t) =
  List.iter
    (fun (tile : TileEntity.t) ->
      let screen_space_position =
        ( float_of_int (Raylib.get_screen_width () / 2),
          float_of_int (Raylib.get_screen_height () / 2) )
        |> add_vec2f
             (mul_vec2f
                (scale_vec2f (vec2f_of_vec2 tile.pos) tile_scaling_factor)
                (1.0, -1.0))
        |> add_vec2f
             (neg_vec2f
                (tile_scaling_factor /. 2.0, tile_scaling_factor /. 2.0))
        |> vec2_of_vec2f
      in
      match to_tile_types tile.entity_type with
      | Mud ->
          Raylib.draw_circle
            (fst screen_space_position
            + int_of_float (tile_scaling_factor /. 2.0))
            (snd screen_space_position
            + int_of_float (tile_scaling_factor /. 2.0))
            (float_of_int (int_of_float (tile_scaling_factor *. 0.09)))
            Color.brown
      | Ground ->
          Raylib.draw_circle
            (fst screen_space_position
            + int_of_float (tile_scaling_factor /. 2.0))
            (snd screen_space_position
            + int_of_float (tile_scaling_factor /. 2.0))
            (float_of_int (int_of_float (tile_scaling_factor *. 0.08)))
            Color.lightgray)
    (List.map
       (fun x -> to_tileentity_type x)
       (GameTiles.all_entities (GameState.get_tiles renderer.source_state)))

(**[game_to_rendered_pos game_pos] converts a [vec2f] position, [game_pos], into
   a position in the rendered world*)
let game_to_rendered_pos game_pos =
  ( float_of_int (Raylib.get_screen_width () / 2),
    float_of_int (Raylib.get_screen_height () / 2) )
  |> add_vec2f
       (mul_vec2f (scale_vec2f game_pos tile_scaling_factor) (1.0, -1.0))
  |> add_vec2f
       (neg_vec2f (tile_scaling_factor /. 2.0, tile_scaling_factor /. 2.0))
  |> vec2_of_vec2f

(**[world_to_game_pos world_pos] converts a [vec2f] world (rendered) position,
   [world_pos] into a position in the game map*)
let rendered_to_game_pos world_pos =
  let a =
    sub_vec2f world_pos
      (neg_vec2f (tile_scaling_factor /. 2.0, tile_scaling_factor /. -2.0))
  in
  let b =
    sub_vec2f a
      ( float_of_int (Raylib.get_screen_width () / 2),
        float_of_int (Raylib.get_screen_height () / 2) )
  in
  mul_vec2f (scale_vec2f b (1. /. tile_scaling_factor)) (1.0, -1.)

let render (renderer : t) =
  begin_mode_2d renderer.camera;
  clear_background Color.white;
  render_floor renderer;
  RenderableSet.to_list renderer.renderables
  |> List.iter (fun (r : renderable) ->
         let screen_space_position = game_to_rendered_pos r.rendered_pos in
         let draw c color =
           let font_size = 24 in
           let text_width = Raylib.measure_text c font_size in
           let text_height = font_size in
           let x_centered =
             fst screen_space_position - (text_width / 2)
             + int_of_float (tile_scaling_factor /. 2.0)
           in
           let y_centered =
             snd screen_space_position - (text_height / 2)
             + int_of_float (tile_scaling_factor /. 2.0)
           in
           Raylib.draw_text c x_centered y_centered font_size color
         in
         match to_entity_types r.source_entity.entity_type with
         | Player -> draw "@" Color.black
         | Pigeon -> draw "p" Color.orange
         | Wall -> draw "#" Color.black
         | Door _ -> draw ">" Color.darkpurple
         | Enemy e -> (
             let create_range_circle radius =
               let center_x =
                 fst screen_space_position
                 + int_of_float (tile_scaling_factor /. 2.)
               in
               let center_y =
                 snd screen_space_position
                 + int_of_float (tile_scaling_factor /. 2.)
               in
               let radius_int = float_of_int radius *. tile_scaling_factor in
               let range_color = Raylib.Color.create 100 100 255 80 in
               Raylib.draw_circle center_x center_y radius_int range_color
             in
             match e with
             | Jailer (r, t) ->
                 create_range_circle 1;
                 draw "j" Color.red
             | Thief ->
                 create_range_circle 1;
                 draw "t" Color.red
             | Fog_Cloud (r, t) ->
                 create_range_circle 1;
                 draw "c" Color.red
             | Variable_Range_and_Damage (r, d) ->
                 create_range_circle r;

                 let text_color =
                   if d < 2. then Color.yellow
                   else if d < 4. then Color.orange
                   else Color.red
                 in

                 draw "e" text_color)
         | Obstacle o -> (
             match o with
             | Spreading_Fire (c, r, g) -> draw "♨" Color.red
             | Fence t -> draw "#" Color.brown)
         | Water -> draw "~" Color.blue
         | Lava -> draw "~" Color.orange
         | Fire -> draw "♨" Color.red
         | Rock -> draw "o" Color.gray
         | HorizontalBouncer is_moving_right ->
             Raylib.draw_text
               (if is_moving_right then ">" else "<")
               (fst screen_space_position)
               (snd screen_space_position)
               (int_of_float tile_scaling_factor)
               Color.black
         | ModifierItem m ->
             draw
               (match m with
               | ScaleAction _ -> "x"
               | AddFire _ -> "F"
               | AddDamage _ -> "!"
               | AugmentToAdjacent -> "+")
               Color.magenta
         | HealthItem _ -> draw "H" Color.green);

  List.iter
    (fun ovly ->
      match ovly.overlay_type with
      | TextRise (msg, damage) ->
          let progress =
            float_of_int ovly.current /. float_of_int ovly.duration
          in
          let eased_progress = 1. -. ((1. -. progress) *. (1. -. progress)) in
          let rise_pos =
            lerp_vec ovly.pos (add_vec2f ovly.pos (0.0, 2.)) eased_progress
          in
          let screen_space_position =
            ( float_of_int (Raylib.get_screen_width () / 2),
              float_of_int (Raylib.get_screen_height () / 2) )
            |> add_vec2f
                 (mul_vec2f
                    (scale_vec2f rise_pos tile_scaling_factor)
                    (1.0, -1.0))
            |> add_vec2f
                 (neg_vec2f
                    (tile_scaling_factor /. 2.0, tile_scaling_factor /. 2.0))
            |> vec2_of_vec2f
          in
          Raylib.draw_text msg
            (fst screen_space_position)
            (snd screen_space_position)
            ui_font_size
            (Raylib.Color.create
               (if damage then 255 else 0)
               (if damage then 0 else 255)
               0
               (int_of_float (256. *. (1.0 -. eased_progress))))
      | Circle { radius; color } ->
          let screen_space_position =
            ( float_of_int (Raylib.get_screen_width () / 2),
              float_of_int (Raylib.get_screen_height () / 2) )
            |> add_vec2f
                 (mul_vec2f
                    (scale_vec2f ovly.pos tile_scaling_factor)
                    (1.0, -1.0))
            |> add_vec2f
                 (neg_vec2f
                    (tile_scaling_factor /. 2.0, tile_scaling_factor /. 2.0))
            |> vec2_of_vec2f
          in
          Raylib.draw_circle
            (fst screen_space_position)
            (snd screen_space_position)
            (radius *. tile_scaling_factor)
            Color.gray)
    renderer.overlays;
  end_mode_2d ()

let rec loop_aux (renderer : t) (entity_state : GameState.t)
    (input_handler : input_handler) =
  if Raylib.window_should_close () then Raylib.close_window ()
  else if
    GameWorld.query_id
      (GameState.room renderer.source_state)
      (to_entity_id (to_gameentity_type (GameState.get_player entity_state)).id)
    = None
    && GameWorld.query_id
         (GameState.room entity_state)
         (to_entity_id
            (to_gameentity_type (GameState.get_player entity_state)).id)
       = None
  then (
    Raylib.begin_drawing ();
    Raylib.clear_background Raylib.Color.white;
    let w =
      Raylib.measure_text "You Died. Press [R] to start a new game" ui_font_size
    in
    Raylib.draw_text "You Died. Press [R] to Start a New Game"
      ((Raylib.get_screen_width () / 2) - (w / 2))
      300 ui_font_size Raylib.Color.black;
    if Raylib.is_key_pressed Raylib.Key.R then (
      let player = create_default_at Player (0, 0) in
      let generated_state =
        Transformations.generate_floor (to_gameworld_type player)
          Procgen.Pgworld.default_room_gen_settings
          [ Transitions.entity_status_runner; Transitions.entity_action_runner ]
      in
      let new_state =
        GameState.add_moves_modifier generated_state (ScaleMove 1) Pigeon
        |> GameState.query_update_player
      in

      Raylib.end_drawing ();
      let updated_renderer = update_render_state renderer new_state in
      loop_aux updated_renderer new_state input_handler)
    else Raylib.end_drawing ();
    loop_aux renderer entity_state input_handler)
  else
    let key_pressed = Raylib.get_key_pressed () in
    match renderer.mode with
    | Act ->
        if key_pressed = Raylib.Key.E then
          loop_aux (set_mode renderer Default) entity_state input_handler
        else
          let ticked_renderer = tick renderer in
          Raylib.begin_drawing ();
          render ticked_renderer;
          let world_pos =
            Raylib.get_screen_to_world_2d
              (Raylib.get_mouse_position ())
              renderer.camera
          in
          let world_vec2f : vec2f =
            (Vector2.x world_pos, Vector2.y world_pos)
          in
          let game_vec2f = rendered_to_game_pos world_vec2f in
          let action_target = vec2_of_vec2f game_vec2f in
          let render_tile_corner =
            game_to_rendered_pos (vec2f_of_vec2 (vec2_of_vec2f game_vec2f))
          in
          (* let render_tile_center = add_vec2 render_tile_corner ( int_of_float
             (tile_scaling_factor /. 2.), int_of_float (tile_scaling_factor /.
             2.) ) in *)
          Raylib.begin_mode_2d renderer.camera;
          Raylib.draw_rectangle_lines (fst render_tile_corner)
            (snd render_tile_corner)
            (int_of_float tile_scaling_factor)
            (int_of_float tile_scaling_factor)
            Color.black;
          (* Raylib.draw_circle (fst render_tile_center) (snd
             render_tile_center) *)
          (* 10. Raylib.Color.black; *)
          let player = to_gameentity_type (GameState.get_player entity_state) in
          let possible_actions =
            GameState.activate_action_modifiers entity_state Player
              (to_entity_stats player.stats).base_actions
          in
          List.iter
            (fun (action_pos, actions) ->
              let screen_space_pos =
                add_vec2
                  (game_to_rendered_pos
                     (vec2f_of_vec2 (add_vec2 action_pos player.pos)))
                  ( int_of_float (tile_scaling_factor /. 2.),
                    int_of_float (tile_scaling_factor /. 2.) )
              in
              if action_target = add_vec2 action_pos player.pos then
                List.iteri
                  (fun idx action ->
                    let render_color =
                      match action with
                      | Modifiers.DealDamage _ -> Raylib.Color.blue
                      | StealAttack
                      | BarrierAttack _
                      | DealFireDamage _
                      | FogAttack _
                      | ApplyFire _ -> Raylib.Color.red
                    in
                    Raylib.draw_circle (fst screen_space_pos)
                      (snd screen_space_pos - (5 * idx))
                      10.
                      (Raylib.Color.create
                         (Raylib.Color.r render_color)
                         (Raylib.Color.g render_color)
                         (Raylib.Color.b render_color)
                         (max (100 - (10 * idx)) 0)))
                  actions
              else
                Raylib.draw_circle (fst screen_space_pos) (snd screen_space_pos)
                  10.
                  (Raylib.Color.create 0 0 0 50))
            possible_actions;
          Raylib.end_mode_2d ();
          draw_ui renderer;
          Raylib.end_drawing ();
          if Raylib.is_mouse_button_pressed MouseButton.Left then
            let input = GameState.Act action_target in
            try
              let updated_entity_state = input_handler entity_state input in
              let updated_renderer =
                update_render_state renderer updated_entity_state
              in
              let ticked_renderer_inner = tick updated_renderer in
              loop_aux ticked_renderer_inner updated_entity_state input_handler
            with GameState.Invalid_input _ ->
              loop_aux ticked_renderer entity_state input_handler
          else loop_aux ticked_renderer entity_state input_handler
    | Default -> (
        if key_pressed = Raylib.Key.E then
          loop_aux (set_mode renderer Act) entity_state input_handler
        else
          let frame_input_opt =
            GameState.(
              match key_pressed with
              | Raylib.Key.W -> Some (MovePlayer (0, 1))
              | Raylib.Key.A -> Some (MovePlayer (-1, 0))
              | Raylib.Key.S -> Some (MovePlayer (0, -1))
              | Raylib.Key.D -> Some (MovePlayer (1, 0))
              | Raylib.Key.Z -> Some Attack
              | _ -> None)
          in
          match frame_input_opt with
          | None ->
              Raylib.begin_drawing ();
              let updated_renderer = tick renderer in
              render updated_renderer;
              draw_ui renderer;
              Raylib.end_drawing ();
              loop_aux updated_renderer entity_state input_handler
          | Some input -> (
              try
                let updated_entity_state = input_handler entity_state input in
                let updated_renderer =
                  update_render_state renderer updated_entity_state
                in
                let ticked_renderer = tick updated_renderer in

                Raylib.begin_drawing ();
                render ticked_renderer;
                draw_ui renderer;
                Raylib.end_drawing ();
                loop_aux ticked_renderer updated_entity_state input_handler
              with GameState.Invalid_input _ ->
                let updated_renderer = tick renderer in
                render updated_renderer;
                loop_aux updated_renderer entity_state input_handler))

let loop (renderer : t) (entity_state : GameState.t)
    (input_handler : input_handler) =
  Raylib.init_window screen_width screen_height "Function Dungeon";
  Raylib.set_target_fps 60;
  loop_aux renderer entity_state input_handler

let make_from_state (entity_state : GameState.t) =
  let source =
    {
      renderables =
        List.map
          (fun x -> to_gameentity_type x)
          (GameWorld.all_entities (GameState.room entity_state))
        |> List.map (fun (entity : GameEntity.t) ->
               {
                 source_entity = entity;
                 rendered_pos = vec2f_of_vec2 entity.pos;
               })
        |> RenderableSet.of_list;
      source_state = entity_state;
      camera = Camera2D.create (Vector2.zero ()) (Vector2.zero ()) 0.0 1.0;
      camera_target =
        compute_camera_target
          (to_gameentity_type (GameState.get_player entity_state)).pos;
      overlays = [];
      mode = Default;
    }
  in
  update_render_state source entity_state
