module type S = sig
  
  (**[t] represents the world type*)
  type t

  (**[e_t] represents the stored entity type in the world*)
  type e_t

  (**[e_id] represents the stored entity id type in the world*)
  type e_id

  (**[empty] is an empty world*)
  val empty : t

  (**[query_pos world pos] returns [Some x] if entity [x] exists at the cartesian point given by [pos], and [None] otherwise*)
  val query_pos : t -> Entity.vec2 -> e_t option

  (**[pos_empty world pos] returns [true] if no entity exists at [pos] and false otherwise*)
  val query_empty : t -> Entity.vec2 -> bool

  (**[query_id : world e_id] returns [Some x] if there exists some entity [x] with [x.id = e_id], and [None] otherwise*)
  val query_id : t -> e_id -> e_t option

  (**[all_entities world] returns a list of all entities within the world*)
  val all_entities : t -> e_t list

  (**[put_entity world entity] adds the entity to the world if it does not already exist, or replaces an old entity with the same id*)
  val put_entity : t -> e_t -> t

  (**[remove_entity world id] removes the entity associated with id [id], or does nothing if the world does not contain that entity*)
  val remove_entity : t -> e_id -> t

end

module Make (E: Entity.S): S with type e_t = E.t and type e_id = E.id