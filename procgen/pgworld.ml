open Engine.Utils
open Layout

(*TYPES*)
type ground =
  | Void
  | Ground
  | Mud

type weak_mob =
  | Variable_Range_and_Damage of int * float
  | Pigeon

type strong_mob =
  | Small_Jailer
  | Medium_Jailer
  | Large_Jailer
  | Thief
  | Small_Fog
  | Large_Fog

type item =
  | ScaleAction of int
  | AddFire of float * int
  | AddDamage of float
  | AugmentToAdjacent
  | HealthItem of float

(** [rand_item] is a random item*)
let rand_item () =
  match Random.int 7 with
  | 0 -> ScaleAction (2 + Random.int 4)
  | 1 -> AddFire (0.1 +. Random.float 1.9, 1 + Random.int 5)
  | 3 -> AddDamage (0.1 +. Random.float 1.9)
  | 4 | 5 | 6 -> HealthItem (Random.float 10.0)
  | _ -> AugmentToAdjacent

let rand_weak_mob () =
  if Random.float 1.0 > 0.7 then Pigeon
  else
    let damage = 0.2 +. ((Random.float 1.0 ** 3.0) *. 6.0) in
    let range = 2 + int_of_float ((Random.float 1.0 ** 1.5) *. 6.0) in
    Variable_Range_and_Damage (range, damage)

let rand_strong_mob () =
  match Random.int 11 with
  | 0 | 1 | 2 -> Small_Jailer
  | 3 | 4 -> Medium_Jailer
  | 5 -> Large_Jailer
  | 6 | 7 -> Thief
  | 8 | 9 -> Small_Fog
  | _ -> Large_Fog

type entity =
  | Empty
  | Water
  | Lava
  | Fire
  | Wall
  | Door of int * vec2
  | Rock
  | WeakMob of weak_mob
  | StrongMob of strong_mob
  | Item of item
  | Player

type tile = ground * entity
type t = tile array array
type world = t list

type room_gen_settings = {
  gen_weak_mob : unit -> weak_mob;
  gen_strong_mob : unit -> strong_mob;
  gen_item : unit -> item;
  weak_mob_rate : float;
  strong_mob_rate : float;
  item_rate : float;
  room_width : int * int;
  room_height : int * int;
  min_room_coverage : float;
  island_liquify_chance : float;
  island_lava_chance : float;
  island_rock_chance : float;
  min_void_size : int;
  noise_room_wall_chance : float;
  rule_one_cave_merge_runs : int;
  rule_two_cave_merge_runs : int;
  num_rooms : int;
}

let default_room_gen_settings =
  {
    gen_weak_mob = (fun () -> rand_weak_mob ());
    gen_strong_mob = (fun () -> rand_strong_mob ());
    gen_item = (fun () -> rand_item ());
    weak_mob_rate = 0.0015;
    strong_mob_rate = 0.001;
    item_rate = 0.0007;
    room_width = (20, 70);
    room_height = (10, 50);
    min_room_coverage = 0.2;
    island_liquify_chance = 0.3;
    island_lava_chance = 0.2;
    island_rock_chance = 0.2;
    min_void_size = 14;
    noise_room_wall_chance = 0.45;
    rule_one_cave_merge_runs = 4;
    rule_two_cave_merge_runs = 1;
    num_rooms = 10;
  }
(*HELPERS*)

(**[room_map f room] creates a new room with tiles created from applying [f] on
   the tiles of the old room.*)
let room_map (f : t -> vec2 -> tile) room =
  let rows, cols = dimensions room in
  Array.init rows (fun x -> Array.init cols (fun y -> f room (x, y)))

(** [copy_room room] is a shallow copy of [room]. *)
let copy_room = room_map get_at_vec

(** [room_tiles room] is a list of pairs of all [(tile coord, tile)] in the
    room. *)
let room_tiles (room : t) : (vec2 * tile) list =
  room |> Array.to_list
  |> List.mapi (fun i row ->
         row |> Array.to_list |> List.mapi (fun j cell -> ((i, j), cell)))
  |> List.flatten

(**[principal_neighbor_items room spot] is the list of items in the principal
   neighbors of [spot]. Out of bounds accesses are ignored. *)
let principal_neighbor_items room spot =
  let neighbors = principal_neighbors spot in
  List.filter_map (get_at_vec_opt room) neighbors

(**[cardinal_neighbor_items room spot] is the list of items in the cardinal
   neighbors of [spot]. Out of bounds accesses are ignored. *)
let cardinal_neighbor_items room spot =
  let neighbors = cardinal_neighbors spot in
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
    | Ground, WeakMob Pigeon -> "P"
    | _, Rock -> "♦"
    | _, Door _ -> ">"
    | _, Player -> "@"
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

(** [liquify_islands room settings] is the room [room] with all isolated islands
    of walls replaced by rocks, water, lave, or void*)
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
          if List.length lst > settings.min_void_size && Random.float 1.0 < 0.9
          then (Void, Wall) (*10% chance to turn each big island into a void.*)
          else if random < settings.island_liquify_chance then (Void, Water)
          else if
            random
            < settings.island_liquify_chance +. settings.island_rock_chance
          then (Ground, Rock)
          else if
            random
            < settings.island_liquify_chance +. settings.island_rock_chance
              +. settings.island_lava_chance
          then (Void, Lava)
          else (Void, Wall)
        in
        apply_at_vecs room lst (fun _ _ -> replacement))
    walls;
  room

let add_mud =
  room_map (fun room spot ->
      let spot_data = get_at_vec room spot in
      let card_neighbors = cardinal_neighbor_items room spot in
      if
        spot_data = (Ground, Empty)
        && List.length
             (List.filter
                (fun tile ->
                  match tile with
                  | _, Water -> true
                  | _ -> false)
                card_neighbors)
           > 0
      then (Mud, Empty)
      else spot_data)

(** [border_wall room] is the room [room] with a border of walls around the
    outside of the map *)
let border_wall room =
  let width, height = dimensions room in
  room_map
    (fun room (x, y) ->
      if x = 0 || y = 0 || x = width - 1 || y = height - 1 then (Void, Wall)
      else get_at_vec room (x, y))
    room

(** [remove_redundant_walls room] is the room [room] with all walls touching
    only other walls replaced by void *)
let remove_redundant_walls =
  room_map (fun room spot ->
      if
        cardinal_neighbor_items room spot
        |> List.filter (fun (_, e) -> e != Wall)
        |> List.length = 0
      then (Void, Empty)
      else get_at_vec room spot)

(** [weak_mob_count room locs] is the number of weak mobs in [room] on the given
    squares [locs].*)
let weak_mob_count room locs =
  locs
  |> List.filter_map (fun vec ->
         get_at_vec_opt room vec |> fun x ->
         match x with
         | Some (_, WeakMob _) -> Some ()
         | _ -> None)
  |> List.length

(** [strong_mob_count room locs] is the number of strong mobs in [room] on the
    given squares [locs].*)
let strong_mob_count room locs =
  locs
  |> List.filter_map (fun vec ->
         get_at_vec_opt room vec |> fun x ->
         match x with
         | Some (_, StrongMob _) -> Some ()
         | _ -> None)
  |> List.length

(**[gen_items_and_mobs room settings] is the room with one round of item and mob
   generation run [runs] times on all tiles. *)
let rec gen_items_and_mobs room runs settings =
  let new_room =
    room_map
      (fun room spot ->
        let spot_data = get_at_vec room spot in
        if spot_data <> (Ground, Empty) then spot_data
        else
          let p_neighbors = principal_neighbors spot in
          let p_neighbors_2 = principal_neighbors_gen2 spot in
          let n_mob_gen1 =
            weak_mob_count room p_neighbors
            + (3 * strong_mob_count room p_neighbors)
            |> float_of_int
          in
          let n_weak_gen2 = weak_mob_count room p_neighbors_2 |> float_of_int in
          let n__strong_gen2 =
            strong_mob_count room p_neighbors_2 |> float_of_int
          in
          let weak_mob_r =
            settings.weak_mob_rate *. (0.5 ** n_mob_gen1)
            *. (0.8 ** n_weak_gen2) *. (0.5 ** n__strong_gen2)
          in
          let strong_mob_r =
            settings.strong_mob_rate *. (0.0 ** n_mob_gen1)
            *. (0.9 ** n_weak_gen2) *. (0.0 ** n__strong_gen2)
          in
          let item_r =
            settings.item_rate *. (2.0 ** n_mob_gen1) *. (1.2 ** n_weak_gen2)
            *. (5.0 ** n__strong_gen2)
          in
          let random = Random.float 1.0 in
          let choosen_entity =
            if random < item_r then Item (settings.gen_item ())
            else if random < item_r +. strong_mob_r then
              StrongMob (settings.gen_strong_mob ())
            else if random < item_r +. strong_mob_r +. weak_mob_r then
              WeakMob (settings.gen_weak_mob ())
            else Empty
          in
          (Ground, choosen_entity))
      room
  in
  match runs with
  | 1 -> new_room
  | i -> gen_items_and_mobs new_room (runs - 1) settings

(** [find_door room cardinal_dir] is the vec2 of the door that should be added
    to [room] *)
let find_door room cardinal_dir =
  let ground_tiles =
    room_tiles room
    |> List.filter (function
         | _, (Ground, _) -> true
         | _ -> false)
  in
  let find_extreme selector comparator lst : vec2 =
    match lst with
    | [] -> failwith "No ground tiles"
    | hd :: tl ->
        List.fold_left
          (fun v1 v2 ->
            if comparator (selector v1) (selector v2) then v1 else v2)
          hd tl
  in
  let tile_coords = List.map fst ground_tiles in
  let door_coord =
    match cardinal_dir with
    | N -> find_extreme snd ( < ) tile_coords |> fun x -> sub_vec2 x (0, 1)
    | E -> find_extreme fst ( > ) tile_coords |> fun x -> add_vec2 x (1, 0)
    | S -> find_extreme snd ( > ) tile_coords |> fun x -> add_vec2 x (0, 1)
    | W -> find_extreme fst ( < ) tile_coords |> fun x -> sub_vec2 x (1, 0)
  in
  door_coord

(** [transpose matrix] is the transposed matrix *)
let transpose matrix =
  let rows = Array.length matrix in
  let cols = Array.length matrix.(0) in
  Array.init cols (fun col -> Array.init rows (fun row -> matrix.(row).(col)))

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
  for _ = 1 to settings.rule_one_cave_merge_runs do
    room_ref := room_map rule_one !room_ref
  done;
  for _ = 1 to settings.rule_two_cave_merge_runs do
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
  else begin
    print_endline "| Cave merge failed, retrying ";
    generate_room settings
  end

and generate_room (settings : room_gen_settings) : t =
  let w_low, w_high = settings.room_width in
  let w = Random.int_in_range ~min:w_low ~max:w_high in
  let h_low, h_high = settings.room_height in
  let h = Random.int_in_range ~min:h_low ~max:h_high in
  print_endline
    ("Generating room with width: " ^ string_of_int w ^ " and height: "
   ^ string_of_int h);
  let room = noise_room w h settings.noise_room_wall_chance in
  let room = cave_merge room settings in
  let room = liquify_islands room settings in
  let room = add_mud room in
  let room = border_wall room in
  let room = remove_redundant_walls room in
  let room = gen_items_and_mobs room 5 settings in
  print_endline "Room generated";
  transpose room

let to_tile_list (room : t) : (tile * vec2) list =
  let width, height = dimensions room in
  List.fold_left
    (fun (acc_ext : (tile * vec2) list) (y : int) ->
      List.fold_left
        (fun (acc : (tile * vec2) list) (x : int) ->
          (room.(x).(y), (x, y)) :: acc)
        acc_ext (List.init width Fun.id))
    [] (List.init height Fun.id)

(** [add_player room] is [room] with a player added to a random square *)
let add_player room =
  let new_room = copy_room room in
  let ground_tiles =
    room_tiles room
    |> List.filter (function
         | _, (Ground, _) -> true
         | _ -> false)
    |> List.map fst
  in
  set_at_vec new_room
    (List.nth ground_tiles (Random.int (List.length ground_tiles)))
    (Ground, Player);
  new_room

let generate_floor (settings : room_gen_settings) =
  let layout = gen_layout settings.num_rooms in
  print_endline "Layout generated";
  let floor =
    Array.map
      (Array.map (fun b -> if b then Some (generate_room settings) else None))
      layout
  in
  print_endline "Rooms generated";
  let counter = ref 0 in
  let indices =
    Array.map
      (fun row ->
        Array.map
          (fun b ->
            if b then (
              let idx = !counter in
              counter := !counter + 1;
              Some idx)
            else None)
          row)
      layout
  in
  let rooms = ref [] in
  for row = Array.length indices - 1 downto 0 do
    for col = Array.length indices.(row) - 1 downto 0 do
      let vec = (row, col) in
      let curr_room_opt = get_at_vec floor vec in
      match curr_room_opt with
      | None -> ()
      | Some room ->
          let neighbors =
            cardinal_neighbors_with_dir vec
            |> List.filter_map (fun (v, d) ->
                   match get_at_vec_opt indices v with
                   | None -> None (* Out of bounds *)
                   | Some None -> None (* Empty *)
                   | Some (Some index) -> Some (index, d, v))
          in
          let new_room =
            List.fold_left
              (fun room_acc (index, dir, v) ->
                let door_coord = find_door room_acc dir in
                let room_copy = copy_room room_acc in
                let linked_room =
                  match get_at_vec floor v with
                  | None -> failwith "Linked room expected"
                  | Some room -> room
                in
                let linked_room_door_coord =
                  find_door linked_room (opposite dir)
                in
                let linked_room_spawn_coord =
                  match dir with
                  | N -> sub_vec2 linked_room_door_coord (0, 1)
                  | E -> add_vec2 linked_room_door_coord (1, 0)
                  | S -> add_vec2 linked_room_door_coord (0, 1)
                  | W -> sub_vec2 linked_room_door_coord (1, 0)
                in
                set_at_vec room_copy door_coord
                  (Void, Door (index, linked_room_spawn_coord));
                room_copy)
              room neighbors
          in
          rooms := new_room :: !rooms
    done
  done;
  print_endline "Rooms linked";
  let player_room_id = Random.int (List.length !rooms) in
  rooms :=
    List.mapi (fun i x -> if i = player_room_id then add_player x else x) !rooms;
  (player_room_id, !rooms)
