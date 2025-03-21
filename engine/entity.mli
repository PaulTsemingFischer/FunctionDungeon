(**[id] represents the type of an entity's id*)
type id

(**[stats] describes basic entity information common to all entities*)
type stats = { health : float }

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
    stats : stats;
    entity_type : entity_type;
    rendering : rendering;
    statuses : status list;
  }
  (**[t] is the type of an entity, containing its id, stats, entity type,
     rendering rules, and statuses*)

val create : stats -> entity_type -> rendering -> status list -> t
(**[create] creates an entity with a unique id and the given entity
    information*)
