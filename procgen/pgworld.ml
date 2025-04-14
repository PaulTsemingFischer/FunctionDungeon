open Engine.Utils

(*TYPES*)
type ground =
  | Void
  | Ground
  | Water
  | Mud

type weak_mob = PlaceHolderWeakMob
type strong_mob = PlaceHolderStrongMob

type entity =
  | Empty
  | Fire
  | Wall
  | Rock
  | WeakMob of weak_mob
  | StrongMob of strong_mob

type tile = ground * entity
type t = tile array array

let default_entity : tile = (Void, Empty)

type room_gen_settings = {
  gen_weak_mob : unit -> weak_mob;
  gen_strong_mob : unit -> strong_mob;
  weak_mob_rate : float;
  strong_mob_rate : float;
  room_width : int;
  room_height : int;
  num_doors : int;
}

let default_room_gen_settings =
  {
    gen_weak_mob = (fun () -> PlaceHolderWeakMob);
    gen_strong_mob = (fun () -> PlaceHolderStrongMob);
    weak_mob_rate = 0.0;
    strong_mob_rate = 0.0;
    room_width = 30;
    room_height = 30;
    num_doors = 0;
  }

type private_gen_settings = { noise_room_wall_chance : float }

let private_gen_settings = { noise_room_wall_chance = 0.4 }

(*HELPERS*)

(**[room_iteration f room] creates a new room with tiles created from applying
   [f] on the tiles of the old room.*)
let room_iteration (f : t -> vec2 -> tile) room =
  let width = Array.length room in
  let height = if width > 0 then Array.length room.(0) else 0 in
  Array.init width (fun x -> Array.init height (fun y -> f room (x, y)))

(**[principal_neighbor_items room spot] is the list of items in the principal
   neighbors of [spot]. Out of bounds accesses are ignored. *)
let principal_neighbor_items room spot =
  let neighbors = principal_neighbors spot in
  List.filter_map (get_at_vec_opt room) neighbors

let string_of_genworld (arr : t) =
  let string_of_tile = function
    | _, Wall -> "☐"
    | _ -> "·"
  in
  String.concat "\n"
    (Array.map
       (fun row ->
         String.concat " " (Array.map string_of_tile row |> Array.to_list))
       arr
    |> Array.to_list)
  ^ "\n"

(*PROC GEN*)

(**[noise_room width height wall_chance] is a room with width [width] and height
   [height] that contains default tiles that are walls with chance
   [wall_chance]. Spots can be accessed with [noise_room.(w).(h)]. *)
let noise_room width height wall_chance =
  let spot () =
    if Random.float 1.0 < wall_chance then (Void, Wall) else (Void, Empty)
  in
  Array.init width (fun _ -> Array.init height (fun _ -> spot ()))

(**[cave_merge room] is a room with cellular-automata cave merge applied. *)
let cave_merge room =
  let num_neighbor_walls room spot =
    principal_neighbor_items room spot
    |> List.filter (fun (_, tile) -> tile = Wall)
    |> List.length
  in

  let rule_one room spot =
    let x = num_neighbor_walls room spot in
    (get_at_vec room spot |> fst, if x >= 5 || x <= 2 then Wall else Empty)
  in
  let rule_two room spot =
    let x = num_neighbor_walls room spot in
    (get_at_vec room spot |> fst, if x >= 5 then Wall else Empty)
  in
  let room_ref = ref room in
  for _ = 1 to 4 do
    room_ref := room_iteration rule_one !room_ref
  done;
  for _ = 1 to 3 do
    room_ref := room_iteration rule_two !room_ref
  done;
  !room_ref

let generate_room (settings : room_gen_settings) : t =
  let room =
    noise_room settings.room_width settings.room_height
      private_gen_settings.noise_room_wall_chance
  in
  cave_merge room
