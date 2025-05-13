open GameDefinitions
open GameState

let obstacle_action (state : GameState.t) (entity : GameEntity.t)
    (obstacle : Obstacles.obstacle) _ =
  match obstacle with
  | Fence t ->
      let this_world = room state in
      let new_world =
        if t = 0 then GameWorld.remove_entity this_world entity.id
        else
          let new_obstacle = Obstacles.update_obstacle_age obstacle in
          let new_entity =
            GameEntity.update_type entity (Obstacle new_obstacle)
          in
          GameWorld.put_entity this_world new_entity
      in
      update_world state new_world
  | Spreading_Fire (c, r, g) ->
      let this_world = get_world state in
      let new_entity_world =
        let new_obstacle = Obstacles.grow_fire obstacle in
        let new_entity =
          GameEntity.update_type entity (Obstacle new_obstacle)
        in
        GameWorld.put_entity this_world new_entity
      in
      GameState.build_barrier state new_entity_world c (r * g)
        (Obstacles.grow_fire obstacle)
      set_room state new_world
  | _ -> state
