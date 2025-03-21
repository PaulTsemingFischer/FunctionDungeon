
(**[id] represents the type of an entity's id. Entities are considered identical if they have the same id*)
type id

(**[string_of_id] returns a human-readable string representation of the entity's id*)
val string_of_id : id -> string


(**[vec2] describes the location of an entity in 2D cartesian space*)
type vec2 = int * int

(**[add_vec2 vec1 vec2] returns the sum of two positions*)
val add_vec2 : vec2 -> vec2 -> vec2

(**[string_of_vec vec] converts [vec] into a string form*)
val string_of_vec : vec2 -> string

(**[stats] describes basic entity information common to all entities*)
type stats = { health : float }

(**[starter_stats] is a utility stats record with all stats set to zero*)
val zeroed_stats : stats

(**[entity_type] describes the behavior of entities in game and optionally
   contains state related to that entity type*)
type entity_type =
  | Player
  | Wall

(**[rendering] describes how an entity should be rendered, and optionally
   contain rendering state*)
type rendering = |Ascii of char
| Id_debug

(**[status] describes a status affect applied to an entity*)
type status =
  | Poisoned of int
  | Invisible of int

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

(**[set_pos entity pos] changes an entity's position to the [pos]*)
val set_pos : t -> vec2 -> t


(**[string_of_entity entity] converts [entity] into a string*)
val string_of_entity : t -> string

include Set.OrderedType with type t := t