open Engine
open Engine.Utils
open Engine.ComparableVec2
open Modifiers
module AttackMap = Map.Make (Vec2)

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
  | ApplyFire (a, a2), ApplyFire (b, b2) -> a = b && a2 = b2
  | DealFireDamage a, DealFireDamage b -> a = b
  | StealAttack, StealAttack -> true
  | BarrierAttack (a, _), BarrierAttack (b, _) -> a = b
  | FogAttack (a, b), FogAttack (x, y) -> a = x && b = y
  | _, _ -> false

(** [effects_to_string lst] converts the attack effects in [lst] to a string. *)
let rec effects_to_string lst =
  match lst with
  | [] -> ""
  | h :: t ->
      (match h with
      | DealDamage x -> "Damage " ^ string_of_float x
      | ApplyFire (x, y) ->
          "Apply fire " ^ string_of_float x ^ ", " ^ string_of_int y
      | DealFireDamage x -> "Deal fire damage " ^ string_of_float x
      | StealAttack -> "Steal"
      | BarrierAttack (x, _) -> "Barrier " ^ string_of_int x
      | FogAttack (r, f) -> "Fog")
      ^ "; " ^ effects_to_string t

(** [bindings_to_string_helper] [lst] converts the attacks in [lst] into a
    string representation. *)
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
let map_of_list lst = AttackMap.of_list lst
let to_key_list lst = lst
let of_key_list lst = lst
let to_key k = k
