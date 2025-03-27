module Vec2 : Map.OrderedType with type t = int * int = struct
  type t = int * int

  let compare v1 v2 = if v1 < v2 then -1 else if v1 = v2 then 0 else 1
end
