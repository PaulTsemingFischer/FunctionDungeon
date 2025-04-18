open GameDefinitions
open Engine.Utils

let bouncer_action (state : GameState.t) (bouncer : GameEntity.t)
    (_ : GameState.input) =
  match bouncer.entity_type with
  | HorizontalBouncer is_moving_right -> (
      match
        GameWorld.query_pos
          (GameState.get_world state)
          (add_vec2 bouncer.pos (if is_moving_right then (1, 0) else (-1, 0)))
      with
      | Some e ->
          let reversed_bouncer =
            GameEntity.update_type bouncer
              (HorizontalBouncer (not is_moving_right))
          in
          GameState.update_world state
            (GameWorld.put_entity (GameState.get_world state) reversed_bouncer)
      | None ->
          Transformations.apply_move state bouncer
            (if is_moving_right then (1, 0) else (-1, 0)))
  | _ ->
      failwith
        ("entity is not a bouncer: " ^ GameEntity.string_of_entity bouncer)
