open Utils

module type EntityData = sig
  type t
  (**[t] is the type of the entity's stats*)

  type entity_type
  (**[entity_type] describes the behavior of entities in game and optionally
     contains state related to that entity type*)

  val zeroed_stats : t
  (**[zeroed_stats] is the default 'zeroed' instance of the given stat type*)

  val string_of_stats : t -> string
  (**[string_of_stats] is a function that converts [t] into a string*)

   val string_of_type : entity_type -> string

end

module type S = sig
  type id
  (**[id] represents the type of an entity's id. Entities are considered
     identical if they have the same id*)

  val string_of_id : id -> string
  (**[string_of_id] returns a human-readable string representation of the
     entity's id*)

  type stats
  (**[stats] describes basic entity information common to all entities*)

  val zeroed_stats : stats
  (**[starter_stats] is a utility stats record with all stats set to zero*)

  type entity_type
  (**[entity_type] describes the behavior of entities in game and optionally
     contains state related to that entity type*)

  type status = ..
  (**[status] describes a status affect applied to an entity*)

  type t = {
    id : id;
    pos : vec2;
    stats : stats;
    entity_type : entity_type;
    statuses : status list;
  }
  (**[t] is the type of an entity, containing its id, stats, entity type,
     rendering rules, and statuses*)

  val create : stats -> entity_type -> status list -> vec2 -> t
  (**[create stats entity_type rendering statuses pos] creates an entity with a
     unique id and the given entity information*)

  val set_pos : t -> vec2 -> t
  (**[set_pos entity pos] changes an entity's position to the [pos]*)

  val update_stats : t -> stats -> t
  (**[update_stats source stats] is a utility method that returns an entity with
     stats equal to [stats] and other parameters being equal to that of [source]*)

  val string_of_entity :
    (status -> string) ->
    t ->
    string
  (**[string_of_entity entity string_of_type string_of_rendering
      string_of_status] converts [entity] into a string*)

  include Set.OrderedType with type t := t
end

module Make (ED : EntityData) : S with type stats = ED.t and type entity_type = ED.entity_type
