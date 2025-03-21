(**[t] represents the world type*)
type t

(**[empty] is an empty world*)
val empty : t

(**[query_at world pos] returns [Some x] if entity [x] exists at the cartesian point given by [pos], and [None] otherwise*)
val query_at : t -> int * int -> Entity.t option

(**[query_id : world e_id] returns [Some x] if there exists some entity [x] with [x.id = e_id], and [None] otherwise*)
val query_id : t -> Entity.id -> Entity.t option

(**[all_entities world] returns a list of all entities within the world*)
val all_entities : t -> Entity.t list

(**[put_entity world entity] adds the entity to the world if it does not already exist, or replaces an old entity with the same id*)
val put_entity : t -> Entity.t -> t