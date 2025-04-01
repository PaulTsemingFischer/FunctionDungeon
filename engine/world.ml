open Utils

module type S = sig
  type t
  type e_t
  type e_id

  val empty : t
  val query_pos : t -> vec2 -> e_t option
  val query_id : t -> e_id -> e_t option
  val mem_pos : t -> vec2 -> bool
  val mem_id : t -> e_id -> bool
  val all_entities : t -> e_t list
  val put_entity : t -> e_t -> t
  val remove_entity : t -> e_id -> t
end

module Make (E : Entity.S) : S with type e_t = E.t and type e_id = E.id = struct
  module EntitySet = Set.Make (E)

  type t = EntitySet.t
  type e_t = E.t
  type e_id = E.id

  let empty = EntitySet.empty
  let all_entities (world : t) : e_t list = EntitySet.to_list world

  let query_pos world (pos : vec2) =
    List.find_opt (fun (e : e_t) -> e.pos = pos) (all_entities world)

  let query_id world e_id =
    EntitySet.find_first_opt (fun entity -> entity.id >= e_id) world

  let mem_pos world pos =
    match query_pos world pos with
    | Some _ -> true
    | None -> false

  let mem_id world e_id =
    match query_id world e_id with
    | Some _ -> true
    | None -> false

  let put_entity (world : t) (e : e_t) =
    if EntitySet.mem e world then EntitySet.add e (EntitySet.remove e world)
    else EntitySet.add e world

  let remove_entity (world : t) (e_id : e_id) =
    match query_id world e_id with
    | Some e -> EntitySet.remove e world
    | None -> world
end
