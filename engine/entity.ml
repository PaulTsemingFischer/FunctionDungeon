type id = int

let string_of_id = string_of_int

type vec2 = int * int

let add_vec2 (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

let string_of_vec vec =
  "(" ^ string_of_id (fst vec) ^ "," ^ string_of_int (snd vec) ^ ")"

type stats = { health : float }

let zeroed_stats = { health = 0. }

type entity_type =
  | Player
  | Wall

type rendering =
  | Ascii of char
  | Id_debug

type status =
  | Poisoned of int
  | Invisible of int

type t = {
  id : int;
  pos : vec2;
  stats : stats;
  entity_type : entity_type;
  rendering : rendering;
  statuses : status list;
}

(*a ref containing the last id assigned to a entity*)
let next_available_id = ref 0

(*a function that generates the next available id*)
let gen_next_id () =
  let current_id = !next_available_id in
  incr next_available_id;
  current_id

let create e_stats e_type e_rendering e_statuses pos =
  {
    id = gen_next_id ();
    pos;
    stats = e_stats;
    entity_type = e_type;
    rendering = e_rendering;
    statuses = e_statuses;
  }

let set_pos (e : t) (x, y) =
  {
    id = e.id;
    pos = (x, y);
    stats = e.stats;
    entity_type = e.entity_type;
    rendering = e.rendering;
    statuses = e.statuses;
  }

let string_of_entity_type (e_type : entity_type) =
  match e_type with
  | Player -> "player"
  | Wall -> "wall"

let string_of_stats (e_stats : stats) =
  Printf.sprintf "{ health: %f }" e_stats.health

let string_of_status (e_status : status) =
  match e_status with
  | Poisoned x -> "poison (" ^ string_of_int x ^ ")"
  | Invisible x -> "invisible (" ^ string_of_int x ^ ")"

let string_of_rendering (e_rendering : rendering) =
  match e_rendering with
  | Ascii x -> "ascii (" ^ String.make 1 x ^ ")"
  | Id_debug -> "id_debug"

let string_of_entity e =
  Printf.sprintf
    "{\n\
    \     id: %d,\n\
    \     pos: (%d, %d),\n\
    \     stats: %s\n\
    \     entity_type: \"%s\",\n\
    \     rendering: \"%s\",\n\
    \     statuses: \"%s\",\n\
    \   }"
    e.id (fst e.pos) (snd e.pos) (string_of_stats e.stats)
    (string_of_entity_type e.entity_type)
    (string_of_rendering e.rendering)
    (List.fold_left
       (fun acc x -> acc ^ ", " ^ string_of_status x)
       "" e.statuses)

let compare (e1 : t) (e2 : t) = e1.id - e2.id
