module EntitySet = Set.Make (Entity)

type t = EntitySet.t

let make_placeholder_entity (e_id : Entity.id) : Entity.t =
  {
    id = e_id;
    pos = (0, 0);
    stats = Entity.zeroed_stats;
    entity_type = Entity.Wall;
    rendering = Entity.Ascii 'P';
    statuses = [];
  }

let empty = EntitySet.empty
let query_pos world pos = EntitySet.find_first_opt (fun e -> e.pos = pos) world

let query_id world e_id =
  EntitySet.find_opt
    {
      id = e_id;
      pos = (0, 0);
      stats = Entity.zeroed_stats;
      entity_type = Entity.Wall;
      rendering = Entity.Ascii '.';
      statuses = [];
    }
    world

let all_entities (world : t) : Entity.t list = EntitySet.to_list world

let put_entity (world : t) (e : Entity.t) =
  if EntitySet.mem e world then EntitySet.add e (EntitySet.remove e world)
  else EntitySet.add e world

let remove_entity (world : t) (e_id : Entity.id) =
  EntitySet.remove (make_placeholder_entity e_id) world
