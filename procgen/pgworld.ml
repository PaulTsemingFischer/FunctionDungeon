open Engine.Utils

(*TYPES*)
type ground =
  | Void
  | Ground
  | Mud

type weak_mob = PlaceHolderWeakMob
type strong_mob = PlaceHolderStrongMob

type entity =
  | Empty
  | Water
  | Lava
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
  min_room_coverage : float;
  island_liquify_chance : float;
  island_lava_chance : float;
  island_rock_chance : float;
  num_doors : int;
}

type private_gen_settings = {
  noise_room_wall_chance : float;
  rule_one_cave_merge_runs : int;
  rule_two_cave_merge_runs : int;
}

let default_room_gen_settings =
  {
    gen_weak_mob = (fun () -> PlaceHolderWeakMob);
    gen_strong_mob = (fun () -> PlaceHolderStrongMob);
    weak_mob_rate = 0.0;
    strong_mob_rate = 0.0;
    room_width = 30;
    room_height = 30;
    min_room_coverage = 0.2;
    island_liquify_chance = 0.5;
    island_lava_chance = 0.2;
    island_rock_chance = 0.3;
    num_doors = 0;
  }

let private_gen_settings =
  {
    noise_room_wall_chance = 0.45;
    rule_one_cave_merge_runs = 4;
    rule_two_cave_merge_runs = 1;
  }

(*HELPERS*)

(**[room_map f room] creates a new room with tiles created from applying [f] on
   the tiles of the old room.*)
let room_map (f : t -> vec2 -> tile) room =
  let width, height = dimensions room in
  Array.init width (fun x -> Array.init height (fun y -> f room (x, y)))

(**[principal_neighbor_items room spot] is the list of items in the principal
   neighbors of [spot]. Out of bounds accesses are ignored. *)
let principal_neighbor_items room spot =
  let neighbors = principal_neighbors spot in
  List.filter_map (get_at_vec_opt room) neighbors

(**[principal_neighbor_gen2_items room spot] is the list of items in the gen2
   principal neighbors of [spot]. Out of bounds accesses are ignored. *)
let principal_neighbor_gen2_items room spot =
  let neighbors = principal_neighbors_gen2 spot in
  List.filter_map (get_at_vec_opt room) neighbors

let rec flood_fill_classification_aux group seen spot (room : tile array array)
    =
  if get_at_vec seen spot then group
  else (
    set_at_vec seen spot true;
    group := spot :: !group;
    (*[cn] are all neighbors of the same type not already in a group. *)
    let group_type = get_at_vec room spot in
    let cn =
      cardinal_neighbors spot
      |> List.filter (fun x -> get_at_vec_opt seen x = Some false)
      |> List.filter (fun x -> get_at_vec room x = group_type)
    in
    List.iter
      (fun s -> ignore (flood_fill_classification_aux group seen s room))
      cn;
    group)

(**[flood_fill_classification room] is a list of all groups of identical tile
   chords that are touching each other in one of the 4 cardinal directions. Each
   tile chord shows up in exactly one group.*)
let flood_fill_classification (room : tile array array) =
  let groups = ref [] in
  let width, height = dimensions room in
  let seen = Array.make_matrix width height false in
  Array.iteri
    (fun x arr ->
      Array.iteri
        (fun y _ ->
          if not (get_at_vec seen (x, y)) then (
            let new_group = ref [ (x, y) ] in
            ignore (flood_fill_classification_aux new_group seen (x, y) room);
            groups := !new_group :: !groups))
        arr)
    room;
  !groups

let string_of_genworld (world : t) =
  let string_of_tile = function
    | Void, Wall -> "#"
    | Void, Water -> "≈"
    | Void, Lava -> "♨"
    | Ground, Empty -> "·"
    | _, Rock -> "♦"
    | _ -> " "
  in
  String.concat "\n"
    (Array.map
       (fun row ->
         String.concat " " (Array.map string_of_tile row |> Array.to_list))
       world
    |> Array.to_list)
  ^ "\n"

(*PROC GEN*)

(**[noise_room width height wall_chance] is a room with width [width] and height
   [height] that contains default tiles that are walls with chance
   [wall_chance]. Spots can be accessed with [noise_room.(w).(h)]. *)
let noise_room width height wall_chance =
  let spot () =
    if Random.float 1.0 < wall_chance then (Void, Wall) else (Ground, Empty)
  in
  Array.init width (fun _ -> Array.init height (fun _ -> spot ()))

(**[cave_merge room settings] is a room with cellular-automata cave merge
   applied. All Empty tiles are guarenteed to be connected. *)
let rec cave_merge original_room settings =
  let self_wall room spot =
    if get_at_vec room spot = (Void, Wall) then 1 else 0
  in
  let num_neighbor_walls room spot =
    8
    - (principal_neighbor_items room spot
      |> List.filter (fun (tile, _) -> tile = Ground)
      |> List.length)
  in
  let num_second_neighbor_walls room spot =
    16
    - (principal_neighbor_gen2_items room spot
      |> List.filter (fun (tile, _) -> tile = Ground)
      |> List.length)
  in
  let rule_one (room : t) (spot : vec2) : tile =
    let n0 = self_wall room spot in
    let n1 = num_neighbor_walls room spot + n0 in
    let n2 = num_second_neighbor_walls room spot + n1 in
    if n1 >= 5 || n2 <= 2 then (Void, Wall) else (Ground, Empty)
  in
  let rule_two (room : t) (spot : vec2) : tile =
    let n1 = num_neighbor_walls room spot in
    if n1 >= 5 then (Void, Wall) else (Ground, Empty)
  in
  let room_ref = ref original_room in
  (*Perform the merges*)
  for _ = 1 to private_gen_settings.rule_one_cave_merge_runs do
    room_ref := room_map rule_one !room_ref
  done;
  for _ = 1 to private_gen_settings.rule_two_cave_merge_runs do
    room_ref := room_map rule_two !room_ref
  done;
  let room = !room_ref in
  let classification = flood_fill_classification room in
  let sorted_classification =
    List.sort
      (fun l1 l2 -> compare (List.length l1) (List.length l2) |> ( ~- ))
      classification
  in
  let open_areas =
    List.filter (*Only look at areas full of [Empty] tiles *)
      (fun lst -> lst |> List.hd |> get_at_vec room = (Ground, Empty))
      sorted_classification
    @ [ []; [] ]
  in
  (*Dummy areas to prevent out of bounds access *)
  let width, height = dimensions room in
  let desired_area =
    float_of_int (width * height) *. settings.min_room_coverage
    |> floor |> int_of_float
  in
  if List.length (List.hd open_areas) >= desired_area then (
    List.iter
      (fun lst_to_clear ->
        apply_at_vecs room lst_to_clear (fun _ _ -> (Void, Wall))
        (*Clear disconnected areas*))
      (List.tl open_areas);
    room)
  else cave_merge original_room settings (*Not enough open area, try again*)

let liquify_islands room settings =
  let room = room_map get_at_vec room in
  (*Copy room*)
  let classification = flood_fill_classification room in
  let walls =
    List.filter
      (fun lst -> lst |> List.hd |> get_at_vec room |> snd = Wall)
      classification
  in
  let width, height = dimensions room in
  List.iter
    (fun lst ->
      if
        List.exists
          (fun (x, y) -> x = 0 || y = 0 || x = width - 1 || y = height - 1)
          lst
        |> not
      then
        let random = Random.float 1.0 in
        let replacement =
          if random < settings.island_liquify_chance then
            (Void, Water)
          else if random < settings.island_liquify_chance +. settings.island_rock_chance then (Ground, Rock)
          else if random < settings.island_liquify_chance +. settings.island_rock_chance +. settings.island_lava_chance then(Void, Lava)
          else (Void, Empty)
        in
        apply_at_vecs room lst (fun _ _ -> replacement))
    walls;
  room

let border_wall room =
  let width, height = dimensions room in
  room_map
    (fun room (x, y) ->
      if x = 0 || y = 0 || x = width - 1 || y = height - 1 then (Void, Wall)
      else get_at_vec room (x, y))
    room

let remove_redundant_walls =
  room_map (fun room spot ->
      if
        principal_neighbor_items room spot
        |> List.filter (fun (_, e) -> e != Wall)
        |> List.length = 0
      then (Void, Empty)
      else get_at_vec room spot)

let generate_room (settings : room_gen_settings) : t =
  let room =
    noise_room settings.room_width settings.room_height
      private_gen_settings.noise_room_wall_chance
  in
  let room = cave_merge room settings in
  let room = liquify_islands room settings in
  let room = border_wall room in
  let room = remove_redundant_walls room in
  room
