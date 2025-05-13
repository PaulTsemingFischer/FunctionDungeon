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
  | ApplyFire (a, a2), ApplyFire (b, b2) -> a = b && a2 = b2
  | _, _ -> false
