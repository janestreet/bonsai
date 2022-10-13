open! Core
module Types = Types

module Make (Name : Types.Name) = struct
  module Types = Types.Make (Name)
  module Transform = Transform.Make (Name)
  open Types

  let union_of_list = List.fold ~init:Name.Set.empty ~f:Set.union

  module Value = struct
    include Value

    let sexp_of_t = Value.sexp_of_t

    let named ?here name =
      { value_kind = Named name; value_here = here; value_id = Name.create () }
    ;;

    let mapn ?here values =
      let value_id = Name.create () in
      { Value.value_kind = Mapn values; value_here = here; value_id }
    ;;

    let rec free_vars (v : t) =
      match v.value_kind with
      | Fake -> Name.Set.empty
      | Redirect { name } -> Name.Set.singleton name
      | Named name -> Name.Set.singleton name
      | Singleton -> Name.Set.empty
      | Mapn names ->
        names |> List.map ~f:free_vars |> List.fold ~init:Name.Set.empty ~f:Set.union
    ;;

    let fake = { value_kind = Fake; value_here = None; value_id = Name.create () }

    let singleton ?here () =
      let value_id = Name.create () in
      { Value.value_kind = Singleton; value_here = here; value_id }
    ;;
  end

  module Computation = struct
    include Computation

    let sexp_of_t = Computation.sexp_of_t
    let free_variables { Computation.free_variables; _ } = free_variables

    let sub ?here ~bound ~as_ ~for_ () =
      match bound.kind with
      | Value { value_kind = Named n; value_here = _; value_id = _ } ->
        Transform.replace_c for_ ~from:as_ ~to_:n
      | _ ->
        let my_binding = { Binding.bound; as_ } in
        let bindings, last_body =
          match for_.kind with
          | Bindings { bindings; last_body } -> my_binding :: bindings, last_body
          | _ -> [ my_binding ], for_
        in
        let kind = Kind.Bindings { bindings; last_body } in
        let free_var_bound = free_variables bound in
        let free_var_for = Set.remove (free_variables for_) as_ in
        let free_variables = Set.union free_var_bound free_var_for in
        { Computation.kind; free_variables; here }
    ;;

    let rec return ?here (v : Value.t) =
      match v.value_kind with
      | Named _ | Redirect _ | Singleton | Fake ->
        { Computation.kind = Value v; free_variables = Value.free_vars v; here }
      | Mapn children ->
        let original_free = Value.free_vars v in
        let introduced, bindings =
          List.fold children ~init:([], []) ~f:(fun (free, bindings) v ->
            match v.value_kind with
            | Named n | Redirect { name = n } -> n :: free, bindings
            | Mapn _ | Singleton | Fake ->
              let as_ = Name.create () in
              as_ :: free, { Binding.bound = return v; as_ } :: bindings)
        in
        let free = Set.of_list (module Name) introduced in
        let last_body =
          let value =
            Value.mapn ?here:None (List.map introduced ~f:(Value.named ?here:None))
          in
          { Computation.kind = Kind.Value value; free_variables = free; here }
        in
        { Computation.kind = Kind.Bindings { bindings; last_body }
        ; free_variables = original_free
        ; here
        }
    ;;

    let wrap ?here ~name ~introduces ~bodies () =
      let kind = Kind.Wrapping { name; introduces; bodies } in
      let introduced_variables = Name.Set.of_list introduces in
      let free_in_bodies = bodies |> List.map ~f:free_variables |> union_of_list in
      let free_variables = Set.diff free_in_bodies introduced_variables in
      { kind; free_variables; here }
    ;;
  end
end
