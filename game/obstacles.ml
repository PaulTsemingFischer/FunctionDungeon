open Root

type obstacle = Spreading_Fire of int * int * int
(* fire patch is currently centered at X with radius Y and growing at Z rate *)
