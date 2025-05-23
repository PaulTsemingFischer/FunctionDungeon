open Utils

module type EntityData = sig
  type t
  type entity_type
  type status_effect

  val string_of_stats : t -> string
  val string_of_type : entity_type -> string
  val string_of_status : status_effect -> string
end

module type S = sig
  type id = int

  val string_of_id : id -> string

  type stats
  type entity_type
  type status_effect

  type t = {
    id : id;
    pos : vec2;
    stats : stats;
    entity_type : entity_type;
    statuses : status_effect list;
  }

  val create : stats -> entity_type -> status_effect list -> vec2 -> t
  val set_pos : t -> vec2 -> t
  val update_stats : t -> stats -> t
  val update_statuses : t -> status_effect list -> t
  val update_type : t -> entity_type -> t
  val string_of_entity : t -> string

  include Set.OrderedType with type t := t
end

module Make (ED : EntityData) :
  S
    with type stats = ED.t
     and type entity_type = ED.entity_type
     and type status_effect = ED.status_effect = struct
  type id = int

  let string_of_id = string_of_int

  type stats = ED.t
  type entity_type = ED.entity_type
  type status_effect = ED.status_effect

  type t = {
    id : int;
    pos : vec2;
    stats : stats;
    entity_type : entity_type;
    statuses : status_effect list;
  }

  let update_stats (e : t) (stats : stats) =
    {
      id = e.id;
      pos = e.pos;
      stats;
      entity_type = e.entity_type;
      statuses = e.statuses;
    }

  let update_statuses (e : t) (statuses : status_effect list) =
    {
      id = e.id;
      pos = e.pos;
      stats = e.stats;
      entity_type = e.entity_type;
      statuses;
    }

  let update_type (e : t) (e_type : entity_type) =
    {
      id = e.id;
      pos = e.pos;
      stats = e.stats;
      entity_type = e_type;
      statuses = e.statuses;
    }

  (*a ref containing the last id assigned to a entity*)
  let next_available_id = ref 0

  (*a function that generates the next available id*)
  let gen_next_id () =
    let current_id = !next_available_id in
    incr next_available_id;
    current_id

  let create e_stats e_type e_statuses pos =
    {
      id = gen_next_id ();
      pos;
      stats = e_stats;
      entity_type = e_type;
      statuses = e_statuses;
    }

  let set_pos (e : t) (x, y) =
    {
      id = e.id;
      pos = (x, y);
      stats = e.stats;
      entity_type = e.entity_type;
      statuses = e.statuses;
    }

  let string_of_entity e =
    Printf.sprintf
      "{\n\
      \  id: %d,\n\
      \  pos: %s,\n\
      \  stats: %s\n\
      \  entity_type: \"%s\",\n\
      \  statuses: \"%s\",\n\
       }"
      e.id (string_of_vec2 e.pos)
      (ED.string_of_stats e.stats)
      (ED.string_of_type e.entity_type)
      (List.fold_left
         (fun acc x -> acc ^ ", " ^ ED.string_of_status x)
         "" e.statuses)

  let compare (e1 : t) (e2 : t) = e1.id - e2.id
end
