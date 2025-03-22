module type StatType = sig
  type t
  (**[t] is the type of the entity's stats*)

  val zeroed_stats : t
  (**[zeroed_stats] is the default 'zeroed' instance of the given stat type*)

  val string_of_stats : t -> string
  (**[string_of_stats] is a function that converts [t] into a string*)
end

type vec2 = int * int
(**[vec2] describes the location of an entity in 2D cartesian space*)

val add_vec2 : vec2 -> vec2 -> vec2
(**[add_vec2 vec1 vec2] returns the sum of two positions*)

val neg_vec2 : vec2 -> vec2
(**[neg_vec2 vec] returns the negated version of [vec]*)

val sub_vec2 : vec2 -> vec2 -> vec2
(**[sub_vec2 vec1] returns [(fst vec1 - fst vec2, snd vec1 - snd vec2)]*)

val string_of_vec : vec2 -> string
(**[string_of_vec vec] converts [vec] into a string form*)

module type S = sig

  type id
  (**[id] represents the type of an entity's id. Entities are considered identical if they have the same id*)

  val string_of_id : id -> string
  (**[string_of_id] returns a human-readable string representation of the entity's id*)

  type stats
  (**[stats] describes basic entity information common to all entities*)

  val zeroed_stats : stats
  (**[starter_stats] is a utility stats record with all stats set to zero*)

  type entity_type = ..
  (**[entity_type] describes the behavior of entities in game and optionally
    contains state related to that entity type*)


  type rendering = ..
  (**[rendering] describes how an entity should be rendered, and optionally
    contain rendering state*)


  type status = ..
  (**[status] describes a status affect applied to an entity*)

  type t = {
      id : id;
      pos: vec2;
      stats : stats;
      entity_type : entity_type;
      rendering : rendering;
      statuses : status list;
    }
    (**[t] is the type of an entity, containing its id, stats, entity type,
      rendering rules, and statuses*)

  val create : stats -> entity_type -> rendering -> status list -> vec2 -> t
  (**[create stats entity_type rendering statuses pos] creates an entity with a unique id and the given entity
      information*)

  val set_pos : t -> vec2 -> t
  (**[set_pos entity pos] changes an entity's position to the [pos]*)

  val update_stats: t -> stats -> t
  (**[update_stats source stats] is a utility method that returns an entity with stats equal to [stats] and other parameters being equal to that of [source]*)


  val string_of_entity : (entity_type -> string) -> (rendering -> string) -> (status -> string) -> t -> string
  (**[string_of_entity entity string_of_type string_of_rendering string_of_status] converts [entity] into a string*)

  include Set.OrderedType with type t := t

end

module Make (ST: StatType) : S with type stats = ST.t