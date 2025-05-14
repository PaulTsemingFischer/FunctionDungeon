open Engine
open Engine.ComparableVec2
open Modifiers
module AttackMap : Map.S

val add_all_attacks :
  (Vec2.t * action list) list ->
  action list AttackMap.t ->
  action list AttackMap.t
(** [add_all_attacks lst atk] adds all attacked tiles in [lst] to the attack
    [atk]. If a tile already exists in [atk], then it will add the new effects
    on top of already-existing ones (i.e. effects will stack). *)

val modify_attack :
  (Vec2.t * action list -> (Vec2.t * action list) list) ->
  action list AttackMap.t ->
  action list AttackMap.t
(** [modify_attack func atk] applies the modification [func] to the attack
    [atk]. New effects are appended TO THE BACK of each tile's effect list. *)

val compare_effects : action -> action -> bool
(** [compare_effects a b] is true if [a] and [b] are equal. *)

val bindings_to_string : action list AttackMap.t -> string
(** [bindings_to_string] converts an attack to a string representation. *)

val map_of_list : possible_action list -> action list AttackMap.t
(** [map_of_list lst] is equivalent to [AttackMap.of_list lst]. *)

val to_key_list : possible_action list -> (AttackMap.key * action list) list
(** [to_key_list lst] converts the vec2 values in [lst] into AttackMap keys. *)

val of_key_list : (AttackMap.key * action list) list -> possible_action list
(** [of_key_list lst] converts the AttackMap keys in [lst] to vec2 values. *)

val to_key : int * int -> AttackMap.key
(** [to_key vec] converts the vec2 to an AttackMap key. *)
