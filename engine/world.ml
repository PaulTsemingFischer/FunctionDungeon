module type S = sig
  type t
  type e_t
  type e_id

  val empty : t
  val query_pos : t -> Entity.vec2 -> e_t option
  val query_empty : t -> Entity.vec2 -> bool
  val query_id : t -> e_id -> e_t option
  val all_entities : t -> e_t list
  val put_entity : t -> e_t -> t
  val remove_entity : t -> e_id -> t
end

module Make (E : Entity.S) : S with type e_t = E.t and type e_id = E.id = struct
  module EntitySet = Set.Make (E)

  type t = EntitySet.t
  type e_t = E.t
  type e_id = E.id
  type E.entity_type += Placeholder
  type E.rendering += RPlaceholder

  let make_placeholder_entity (e_id : e_id) : e_t =
    {
      id = e_id;
      pos = (0, 0);
      stats = E.zeroed_stats;
      entity_type = Placeholder;
      rendering = RPlaceholder;
      statuses = [];
    }

  let empty = EntitySet.empty
  let all_entities (world : t) : e_t list = EntitySet.to_list world

  let query_pos world (pos : Entity.vec2) =
    List.find_opt (fun (e : e_t) -> e.pos = pos) (all_entities world)

  let query_empty world pos =
    match query_pos world pos with
    | Some _ -> false
    | None -> true

  let query_id world e_id =
    EntitySet.find_opt
      {
        id = e_id;
        pos = (0, 0);
        stats = E.zeroed_stats;
        entity_type = Placeholder;
        rendering = RPlaceholder;
        statuses = [];
      }
      world

  let put_entity (world : t) (e : e_t) =
    if EntitySet.mem e world then EntitySet.add e (EntitySet.remove e world)
    else EntitySet.add e world

  let remove_entity (world : t) (e_id : e_id) =
    EntitySet.remove (make_placeholder_entity e_id) world
end
