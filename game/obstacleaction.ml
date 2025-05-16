open GameDefinitions
open GameState

let obstacle_action (state : GameState.t) (entity : GameEntity.t)
    (obstacle : Obstacles.obstacle) _ =
  match obstacle with
  | Fence t ->
      let this_world = room state in
      let new_world =
        if t = 0 then
          GameWorld.remove_entity this_world (to_entity_id entity.id)
        else
          let new_obstacle = Obstacles.update_obstacle_age obstacle in
          let new_entity =
            GameEntity.update_type entity
              (to_entity_type (Obstacle new_obstacle))
          in
          GameWorld.put_entity this_world (to_gameworld_type new_entity)
      in
      set_room state new_world
  | Spreading_Fire (c, r, g) ->
      let this_world = room state in
      let new_entity_world =
        let new_obstacle = Obstacles.grow_fire obstacle in
        let new_entity =
          GameEntity.update_type entity (to_entity_type (Obstacle new_obstacle))
        in
        GameWorld.put_entity this_world (to_gameworld_type new_entity)
      in
      GameState.build_barrier state new_entity_world c (r * g)
        (Obstacles.grow_fire obstacle)
