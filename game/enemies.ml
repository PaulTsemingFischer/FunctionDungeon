open Root

type enemy =
  | Jailer of int * int
    (* fence player in X radius from their current position for Y turns *)
  | Thief (* Randomly take one of player's items *)
  | Blinder of
      int (* player can't see any of the board/must go by memory for X turns *)
  | Fog_Cloud of int * int
(* player can't see more than X radius around current position for Y turns *)
