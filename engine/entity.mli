open Utils

module type EntityData = sig
  type t
  (**[t] is the type of the entity's stats*)

  type entity_type
  (**[entity_type] describes the behavior of entities in game and optionally
     contains state related to that entity type*)

  type status_effect
  (**[status_effect] includes effects like fire, slow, poison, etc. that may
     last over several turns when applied to an entity*)

  val string_of_stats : t -> string
  (**[string_of_stats] returns a string representation of the type of stats, [t]*)

  val string_of_type : entity_type -> string
  (**[string_of_type] returns a string representation of some [entity_type]*)

  val string_of_status : status_effect -> string
  (**[string_of_status status_effect] returns a string representation of
     [status_effect]*)
end

module type S = sig
  type id = int
  (**[id] represents the type of an entity's id. Entities are considered
     identical if they have the same id*)

  val string_of_id : id -> string
  (**[string_of_id] returns a human-readable string representation of the
     entity's id*)

  type stats
  (**[stats] describes basic entity information common to all entities*)

  type entity_type
  (**[entity_type] describes the behavior of entities in game and optionally
     contains state related to that entity type*)

  type status_effect
  (**[status_effect] describes a status affect applied to an entity*)

  type t = {
    id : id;
    pos : vec2;
    stats : stats;
    entity_type : entity_type;
    statuses : status_effect list;
  }
  (**[t] is the type of an entity, containing its id, stats, entity type,
     rendering rules, and statuses*)

  val create : stats -> entity_type -> status_effect list -> vec2 -> t
  (**[create stats entity_type statuses pos] creates an entity with a unique id
     and the given entity information*)

  val set_pos : t -> vec2 -> t
  (**[set_pos entity pos] changes an entity's position to the [pos]*)

  val update_stats : t -> stats -> t
  (**[update_stats source stats] is a utility method that returns an entity with
     stats equal to [stats] and other parameters being equal to that of [source]*)

  val update_type : t -> entity_type -> t
  (**[update_type source e_type] changes the [source] entity's type to [e_type]*)

  val string_of_entity : t -> string
  (**[string_of_entity entity string_of_type string_of_rendering
      string_of_status] converts [entity] into a string*)

  include Set.OrderedType with type t := t
end

module Make (ED : EntityData) :
  S
    with type stats = ED.t
     and type entity_type = ED.entity_type
     and type status_effect = ED.status_effect
