open Utils

module type EntityData = sig
  type t
  type entity_type

  val zeroed_stats : t
  val string_of_stats : t -> string
  val string_of_type : entity_type -> string
end

module type S = sig
  type id

  val string_of_id : id -> string

  type stats

  val zeroed_stats : stats

  type entity_type
  type rendering = ..
  type status = ..

  type t = {
    id : id;
    pos : vec2;
    stats : stats;
    entity_type : entity_type;
    rendering : rendering;
    statuses : status list;
  }

  val create : stats -> entity_type -> rendering -> status list -> vec2 -> t
  val set_pos : t -> vec2 -> t
  val update_stats : t -> stats -> t

  val string_of_entity :
    (rendering -> string) -> (status -> string) -> t -> string

  include Set.OrderedType with type t := t
end

module Make (ED : EntityData) :
  S with type stats = ED.t and type entity_type = ED.entity_type = struct
  type id = int

  let string_of_id = string_of_int

  type stats = ED.t

  let zeroed_stats = ED.zeroed_stats

  type entity_type = ED.entity_type
  type rendering = ..
  type status = ..

  type t = {
    id : int;
    pos : vec2;
    stats : stats;
    entity_type : entity_type;
    rendering : rendering;
    statuses : status list;
  }

  let update_stats (e : t) (stats : stats) =
    {
      id = e.id;
      pos = e.pos;
      stats;
      entity_type = e.entity_type;
      rendering = e.rendering;
      statuses = e.statuses;
    }

  (*a ref containing the last id assigned to a entity*)
  let next_available_id = ref 0

  (*a function that generates the next available id*)
  let gen_next_id () =
    let current_id = !next_available_id in
    incr next_available_id;
    current_id

  let create e_stats e_type e_rendering e_statuses pos =
    {
      id = gen_next_id ();
      pos;
      stats = e_stats;
      entity_type = e_type;
      rendering = e_rendering;
      statuses = e_statuses;
    }

  let set_pos (e : t) (x, y) =
    {
      id = e.id;
      pos = (x, y);
      stats = e.stats;
      entity_type = e.entity_type;
      rendering = e.rendering;
      statuses = e.statuses;
    }

  let string_of_entity (string_of_rendering : rendering -> string)
      (string_of_status : status -> string) e =
    Printf.sprintf
      "{\n\
      \  id: %d,\n\
      \  pos: %s,\n\
      \  stats: %s\n\
      \  entity_type: \"%s\",\n\
      \  rendering: \"%s\",\n\
      \  statuses: \"%s\",\n\
       }"
      e.id (string_of_vec e.pos)
      (ED.string_of_stats e.stats)
      (ED.string_of_type e.entity_type)
      (string_of_rendering e.rendering)
      (List.fold_left
         (fun acc x -> acc ^ ", " ^ string_of_status x)
         "" e.statuses)

  let compare (e1 : t) (e2 : t) = e1.id - e2.id
end
