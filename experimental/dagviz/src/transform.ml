open! Core

module Make (Name : Types.Name) = struct
  module Types = Types.Make (Name)
  open Types

  let rec replace_v (v : Value.t) ~(from : Name.t) ~(to_ : Name.t) : Value.t =
    let kind =
      match v.value_kind with
      | Fake -> Value.Fake
      | Redirect { name } ->
        let name = if Name.equal name from then to_ else name in
        Value.Redirect { name }
      | Named n -> Named (if Name.equal n from then to_ else n)
      | Singleton -> Singleton
      | Mapn l -> Mapn (List.map l ~f:(replace_v ~from ~to_))
    in
    { v with value_kind = kind }

  and replace_c
        ({ kind; free_variables; here } as c : Computation.t)
        ~(from : Name.t)
        ~(to_ : Name.t)
    : Computation.t
    =
    if not (Set.mem free_variables from)
    then c
    else (
      let kind : Kind.t =
        match kind with
        | Bindings { bindings; last_body } ->
          let bindings =
            List.map bindings ~f:(fun binding ->
              { binding with bound = replace_c binding.bound ~from ~to_ })
          in
          let last_body = replace_c last_body ~from ~to_ in
          Bindings { bindings; last_body }
        | Value v -> Value (replace_v v ~from ~to_)
        | Wrapping w ->
          Wrapping { w with bodies = List.map w.bodies ~f:(replace_c ~from ~to_) }
      in
      let free_variables =
        free_variables |> Fn.flip Set.remove from |> Fn.flip Set.add to_
      in
      { kind; free_variables; here })
  ;;

  let compare_bindings_for_sorting
        { Binding.as_ = as1; bound = { free_variables = f1; _ } }
        { Binding.as_ = as2; bound = { free_variables = f2; _ } }
    =
    match Name.Set.compare f1 f2 with
    | 0 -> Name.compare as1 as2
    | other -> other
  ;;

  let insert groups item =
    let rec find_indexes (groups : Binding.t list list) (item : Binding.t) idx acc =
      match groups with
      | [] -> acc
      | group :: rest ->
        if List.exists group ~f:(fun member ->
          Set.mem member.bound.free_variables item.as_)
        then acc
        else find_indexes rest item (idx + 1) (idx :: acc)
    in
    match find_indexes groups item 0 [] with
    | [] -> [ item ] :: groups
    | idx :: _ ->
      List.mapi groups ~f:(fun i group -> if i = idx then item :: group else group)
  ;;

  let group_by_deps l ~last_body ~point_to =
    List.fold
      (List.rev l)
      ~init:[ [ { Binding.bound = last_body; as_ = point_to } ] ]
      ~f:insert
  ;;

  let group_bindings (bindings : Binding.t list) ~curr_id ~last_body ~point_to =
    let grouped = group_by_deps ~last_body ~point_to bindings in
    List.rev grouped
    |> List.fold
         ~init:([], [], Name.Set.empty, curr_id)
         ~f:(fun (rows, down_row, missing_one_level_down, curr_id) row ->
           let provided_here = Name.Set.of_list (List.map row ~f:(fun { as_; _ } -> as_)) in
           let gaps = Set.diff missing_one_level_down provided_here in
           let missing_here =
             Set.union
               gaps
               (List.fold
                  row
                  ~init:Name.Set.empty
                  ~f:(fun acc { Binding.bound = { free_variables; _ }; _ } ->
                    Set.union acc free_variables))
           in
           let rewrite, curr_id =
             Set.fold gaps ~init:(Name.Map.empty, curr_id) ~f:(fun (acc, curr_id) gap ->
               let name, curr_id = Name.next curr_id in
               Map.set acc ~key:gap ~data:name, curr_id)
           in
           let down_row =
             List.map down_row ~f:(fun binding ->
               let bound =
                 Map.fold rewrite ~init:binding.Binding.bound ~f:(fun ~key ~data acc ->
                   replace_c acc ~from:key ~to_:data)
               in
               { binding with bound })
           in
           let curr_id, redirections =
             let rewritten = Map.to_alist rewrite in
             List.fold_map rewritten ~init:curr_id ~f:(fun curr_id (from, to_) ->
               let intermediate, curr_id = Name.next curr_id in
               let bound_id, curr_id = Name.next curr_id in
               let last_body_id, curr_id = Name.next curr_id in
               let inner =
                 { Binding.bound =
                     { kind =
                         Kind.Value
                           { value_kind = Value.Redirect { name = from }
                           ; value_here = None
                           ; value_id = bound_id
                           }
                     ; free_variables = Name.Set.singleton from
                     ; here = None
                     }
                 ; as_ = intermediate
                 }
               in
               let last_body =
                 { Types.Computation.kind =
                     Kind.Value
                       { value_kind = Value.Redirect { name = intermediate }
                       ; value_here = None
                       ; value_id = last_body_id
                       }
                 ; free_variables = Name.Set.singleton intermediate
                 ; here = None
                 }
               in
               ( curr_id
               , { Binding.bound =
                     { kind = Kind.Bindings { bindings = [ inner ]; last_body }
                     ; free_variables = Name.Set.singleton from
                     ; here = None
                     }
                 ; as_ = to_
                 } ))
           in
           let this_row_including_redirections =
             redirections @ row |> List.sort ~compare:compare_bindings_for_sorting
           in
           down_row :: rows, this_row_including_redirections, missing_here, curr_id)
    |> fun (rows, last_row, _, curr_id) -> last_row :: rows, curr_id
  ;;

  let reorder_to_minimize_crossings l =
    List.folding_map l ~init:None ~f:(fun prev cur ->
      match prev with
      | None -> Some cur, cur
      | Some prev ->
        List.map cur ~f:(fun n ->
          let positions =
            n.Binding.bound.free_variables
            |> Set.to_list
            |> List.filter_map ~f:(fun free ->
              List.findi prev ~f:(fun _ { Binding.as_; _ } -> Name.equal as_ free)
              |> Option.map ~f:(fun (i, _) -> Float.of_int i))
          in
          let pos =
            match List.length positions with
            | 0 -> -1.0 (* nodes with no dependencies go to the top *)
            | n -> List.sum (module Float) ~f:Fn.id positions /. Float.of_int n
          in
          pos, n)
        |> List.sort ~compare:[%compare: float * Types.Binding.t]
        |> List.map ~f:(fun (_, n) -> n)
        |> fun a -> Some a, a)
  ;;

  let organize_bindings bindings ~curr_id ~last_body ~point_to =
    let grouped, curr_id = group_bindings bindings ~last_body ~point_to ~curr_id in
    reorder_to_minimize_crossings grouped, curr_id
  ;;
end
