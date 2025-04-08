open Engine
open Engine.ComparableVec2
open Modifiers
module AttackMap = Map.Make (Vec2)

(** [add_all_attacks lst atk] adds all attacked tiles in [lst] to the attack
    [atk]. If a tile already exists in [atk], then it will add the new effects
    on top of already-existing ones (i.e. effects will stack). *)
let rec add_all_attacks lst atk =
  match lst with
  | [] -> atk
  | h :: t ->
      let effects = try AttackMap.find (fst h) atk with Not_found -> [] in
      add_all_attacks t (AttackMap.add (fst h) (effects @ snd h) atk)

(** Helper method [modify_attack func atk]. Do not call directly *)
let rec modify_attack_helper func atk_lst atk =
  match atk_lst with
  | [] -> atk
  | h :: t -> modify_attack_helper func t (add_all_attacks (func h) atk)

(** [modify_attack func atk] applies the modification [func] to the attack
    [atk]. New effects are appended TO THE BACK of each tile's effect list. *)
let modify_attack func atk =
  modify_attack_helper func (AttackMap.bindings atk) atk

(** [compare_effects a b] is true if [a] and [b] are equal. *)
let compare_effects a b =
  match (a, b) with
  | DealDamage a, DealDamage b -> a = b
  | ApplyFire a, ApplyFire b -> a = b
  | _, _ -> false

let rec effects_to_string lst =
  match lst with
  | [] -> ""
  | h :: t -> (
      match h with
      | DealDamage x -> "Damage " ^ string_of_float x
      | ApplyFire x -> "Fire " ^ string_of_int x ^ "; " ^ effects_to_string t)

let rec bindings_to_string_helper lst =
  match lst with
  | [] -> ""
  | h :: t ->
      ("("
      ^ string_of_int (fst (fst h))
      ^ ","
      ^ string_of_int (snd (fst h))
      ^ ") "
      ^ effects_to_string (snd h))
      ^ "\n"
      ^ bindings_to_string_helper t

let bindings_to_string map = bindings_to_string_helper (AttackMap.bindings map)

(* EXAMPLE ITEMS *)

(** [do_damage_example] is a modifier example that adds 1 damage to all attacked
    tiles. *)
let do_damage_example = fun tile -> [ (fst tile, DealDamage 1.0) ]

(** [add_fire_example] is a modifier example that causes all attacked tiles to
    deal fire damage for 3 turns. *)
let add_fire_example = fun tile -> [ (fst tile, ApplyFire 3) ]

(** [augment_to_above_example] is a modifier example that adds all tiles one
    step above currently attacked tiles. *)
let augment_to_above_example =
 fun tile -> [ (Utils.add_vec2 (fst tile) (0, 1), snd tile) ]

(** [augment_to_adjacents_example] is a modifier example that adds all tiles
    directly adjacent to currently attacked tiles (four cardinal directions). *)
let augment_to_adjacents_example =
 fun tile ->
  [
    tile;
    (Utils.add_vec2 (fst tile) (0, 1), snd tile);
    (Utils.add_vec2 (fst tile) (0, -1), snd tile);
    (Utils.add_vec2 (fst tile) (1, 0), snd tile);
    (Utils.add_vec2 (fst tile) (1, 0), snd tile);
  ]
