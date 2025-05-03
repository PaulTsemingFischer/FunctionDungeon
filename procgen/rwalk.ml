(* open Engine.Utils
open BatList
open Seq
open Game.Transitions
open Engine
open Game.GameDefinitions

type tile =
  | Void
  | Ground

type t = tile array array

let string_of_tile = function
  | Void -> "·"
  | Ground -> "☐"

let string_of_genworld (arr : t) =
  String.concat "\n"
    (Array.map
       (fun row ->
         String.concat " " (Array.map string_of_tile row |> Array.to_list))
       arr
    |> Array.to_list)
  ^ "\n"

type walker = {
  mutable coord : vec2;
  mutable age : int;
}

(**[generate_aux world walkers nwalkers] walks a random walker from the list of
   [walkers] 1 step with no more than [nwalkers] existing at any moment. *)
let generate_aux world walkers nwalkers =
  let random_walker () =
    BatList.at !walkers (Random.int (BatList.length !walkers))
  in
  let kill_walker walker = walkers := BatList.remove !walkers walker in

  let walker = random_walker () in
  let possible_tiles =
    cardinal_neighbors walker.coord
    |> List.filter (fun c -> get_at_vec_opt world c = Some Void)
  in
  if List.is_empty possible_tiles then kill_walker walker
  else
    let tile =
      BatList.at possible_tiles (Random.int (BatList.length possible_tiles))
    in
    set_at_vec world tile Ground;
    walker.coord <- tile;
    walker.age <- walker.age - 1;
    if walker.age <= 0 then kill_walker walker
    else if BatList.length !walkers < nwalkers then
      walkers :=
        BatList.cons
          { coord = walker.coord; age = walker.age }
          !walkers (*Add walker*)

(**[post_process world] is the world after being processed to avoid narrow
   passages. *)
let post_process world fill_chance =
  Array.mapi
    (fun outer_index arr ->
      Array.mapi
        (fun inner_index tile ->
          if Random.float 1.0 > fill_chance then tile
          else
            let neighbor_tiles =
              List.map (get_at_vec_opt world)
                (cardinal_neighbors (outer_index, inner_index))
              |> List.filter Option.is_some |> List.map Option.get
            in
            if List.mem Ground neighbor_tiles then Ground else tile)
        arr)
    world

let generate ?(printing = false) ?(walker_age = 30) ?(nwalkers = 5)
    ?(pp_fill_chance = 0.6) width height =
  Random.self_init ();
  let world = Array.init height (fun _ -> Array.make width Void) in
  let center : vec2 = (height / 2, width / 2) in
  set_at_vec world center Ground;
  let walkers = ref [ { coord = center; age = walker_age } ] in
  while BatList.length !walkers <> 0 do
    if printing then string_of_genworld world |> print_endline;
    generate_aux world walkers nwalkers
  done;
  if printing then string_of_genworld world |> print_endline;
  let world = post_process world pp_fill_chance in
  if printing then string_of_genworld world |> print_endline;
  world

let world_from_genworld gen_world =
  let game_world = ref GameWorld.empty in
  let rows = Array.length gen_world in
  if rows = 0 then !game_world
  else
    let cols = Array.length gen_world.(0) in
    for i = 0 to rows - 1 do
      for j = 0 to cols - 1 do
        let tile = get_at_vec gen_world (i, j) in
        let neighbor_tiles =
          List.map (get_at_vec_opt gen_world) (cardinal_neighbors (i, j))
        in
        if tile = Void && List.mem (Some Ground) neighbor_tiles then
          (* print_endline ("Adding to " ^ string_of_int i ^ ", " ^
             string_of_int j); *)
          game_world :=
            GameWorld.put_entity !game_world (create_default_at Wall (i, j))
      done
    done;
    print_endline (string_of_genworld gen_world);
    !game_world *)
