open Engine

type game_stat = { health : float }

module BaseGameStat : Entity.StatType with type t = game_stat = struct
  type t = game_stat

  let zeroed_stats = { health = 0.0 }
  let string_of_stats stat = Printf.sprintf "health: %f" stat.health
end

module GameEntity = Entity.Make (BaseGameStat)
module GameWorld = World.Make (GameEntity)
module GameState = State.Make (GameWorld)
