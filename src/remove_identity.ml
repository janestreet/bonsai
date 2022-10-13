let remove_identity computation =
  let computation_mapper =
    { Transform.For_computation.f =
        (fun context () t ->
           let t = context.recurse () t in
           match t.kind with
           | Identity t -> t
           | _ -> t)
    }
  in
  let value_mapper =
    { Transform.For_value.f = (fun context () t -> context.recurse () t) }
  in
  Transform.map ~computation_mapper ~value_mapper ~init:() computation
;;
