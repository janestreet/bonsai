module Event = struct
  type t = unit

  let no_op = ()
  let sequence _ = ()
end

module Attr = struct
  type t = unit

  let on_click _ = failwith "not real"
end

module Node = struct
  type t = unit

  let button _ _ = ()
  let text _ = ()
  let div _ _ = ()
end
