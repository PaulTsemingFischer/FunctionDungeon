module Vec2 : Map.OrderedType = struct
  type t = int * int

  let compare v1 v2 = if v1 < v2 then -1 else if v1 = v2 then 0 else 1
end
