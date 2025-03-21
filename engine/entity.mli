
(**[id] represents the type of an entity's id. Entities are considered identical if they have the same id*)
type id

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
type rendering = Ascii of char

(**[status] describes a status affect applied to an entity*)
type status =
  | Poisoned of int
  | Invisible of int

type t = {
    id : id;
    pos: int * int;
    stats : stats;
    entity_type : entity_type;
    rendering : rendering;
    statuses : status list;
  }
  (**[t] is the type of an entity, containing its id, stats, entity type,
     rendering rules, and statuses*)

val create : stats -> entity_type -> rendering -> status list -> int * int -> t
(**[create stats entity_type rendering statuses pos] creates an entity with a unique id and the given entity
    information*)

(**[set_pos entity pos] changes an entity's position to the [pos]*)
val set_pos : t -> int * int -> t


(**[string_of_entity entity] converts [entity] into a string*)
val string_of_entity : t -> string

include Set.OrderedType with type t := t