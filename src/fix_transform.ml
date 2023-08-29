open! Core
open! Import
include Fix_transform_intf

module Make
    (Types : Types) (F : functor (_ : Recurse with module Types := Types) ->
      Transform with module Types := Types) : Transform with module Types := Types = struct
  module rec Recurse : (Recurse with module Types := Types) = struct
    let combine_up, empty, empty_for_lazy = Types.Up.(combine, empty, empty_for_lazy)

    let default_c (type a) down acc (computation : a Computation.t)
      : _ * _ * a Computation.t
      =
      let acc, up, computation =
        match computation with
        | Return value ->
          let acc, up, value = User.transform_v down acc value in
          acc, up, Computation.Return value
        | Leaf1 { model; input_id; dynamic_action; apply_action; input; reset } ->
          let acc, up, input = User.transform_v down acc input in
          acc, up, Leaf1 { model; input_id; dynamic_action; apply_action; input; reset }
        | Leaf0 { model; static_action; apply_action; reset } ->
          acc, empty, Leaf0 { model; static_action; apply_action; reset }
        | Leaf_incr { input; compute } ->
          let acc, up, input = User.transform_v down acc input in
          acc, up, Leaf_incr { input; compute }
        | Sub { from; via; into; here } ->
          let acc, up1, from = User.transform_c down acc from in
          let acc, up2, into = User.transform_c down acc into in
          acc, combine_up up1 up2, Sub { from; via; into; here }
        | Store { id; value; inner } ->
          let acc, up1, value = User.transform_v down acc value in
          let acc, up2, inner = User.transform_c down acc inner in
          acc, combine_up up1 up2, Store { id; value; inner }
        | Fetch { id; default; for_some } -> acc, empty, Fetch { id; default; for_some }
        | Assoc { map; key_comparator; key_id; cmp_id; data_id; by } ->
          let acc, up1, map = User.transform_v down acc map in
          let acc, up2, by = User.transform_c down acc by in
          ( acc
          , combine_up up1 up2
          , Assoc { map; key_comparator; key_id; cmp_id; data_id; by } )
        | Assoc_on t ->
          let acc, up1, map = User.transform_v down acc t.map in
          let acc, up2, by = User.transform_c down acc t.by in
          acc, combine_up up1 up2, Assoc_on { t with map; by }
        | Assoc_simpl { map; by } ->
          let acc, up, map = User.transform_v down acc map in
          acc, up, Assoc_simpl { map; by }
        | Switch { match_; arms; here } ->
          let acc, up1, match_ = User.transform_v down acc match_ in
          let (acc, upn), arms =
            arms
            |> Map.to_alist
            |> List.fold_map ~init:(acc, up1) ~f:(fun (acc, up) (k, v) ->
              let acc, up', v = User.transform_c down acc v in
              (acc, combine_up up up'), (k, v))
          in
          let arms = Map.of_alist_exn (module Int) arms in
          acc, upn, Switch { match_; arms; here }
        | Lazy t ->
          let t =
            Lazy.map t ~f:(fun t ->
              let _acc, _up, t = User.transform_c down acc t in
              t)
          in
          acc, empty_for_lazy, Lazy t
        | Wrap
            { wrapper_model
            ; action_id
            ; result_id
            ; inject_id
            ; model_id
            ; inner
            ; dynamic_apply_action
            ; reset
            } ->
          let acc, up, inner = User.transform_c down acc inner in
          let res =
            Computation.Wrap
              { wrapper_model
              ; action_id
              ; result_id
              ; inject_id
              ; model_id
              ; inner
              ; dynamic_apply_action
              ; reset
              }
          in
          acc, up, res
        | With_model_resetter { inner; reset_id } ->
          let acc, up, inner = User.transform_c down acc inner in
          acc, up, With_model_resetter { inner; reset_id }
        | Path -> acc, empty, Path
        | Lifecycle value ->
          let acc, up, value = User.transform_v down acc value in
          acc, up, Lifecycle value
      in
      acc, up, computation
    ;;

    let reduce_up l = List.reduce l ~f:combine_up |> Option.value ~default:empty

    let default_v (type a) down acc ({ value; id; here } : a Value.t) : _ * _ * a Value.t =
      let acc, up, value =
        match value with
        | Constant (c : a) -> acc, empty, Value.Constant c
        | Exception (e : exn) -> acc, empty, Exception e
        | Incr incr_node -> acc, empty, Incr incr_node
        | Named (name_source : Value.Name_source.t) -> acc, empty, Named name_source
        | Both (a, b) ->
          let acc, up_a, a = User.transform_v down acc a in
          let acc, up_b, b = User.transform_v down acc b in
          acc, combine_up up_a up_b, Both (a, b)
        | Cutoff t ->
          let acc, up, value = User.transform_v down acc t.t in
          ( acc
          , up
          , Cutoff
              { equal = t.equal; t = value; added_by_let_syntax = t.added_by_let_syntax }
          )
        | Map t ->
          let acc, up, value = User.transform_v down acc t.t in
          acc, up, Map { f = t.f; t = value }
        | Map2 t ->
          let acc, up1, t1 = User.transform_v down acc t.t1 in
          let acc, up2, t2 = User.transform_v down acc t.t2 in
          acc, reduce_up [ up1; up2 ], Map2 { f = t.f; t1; t2 }
        | Map3 t ->
          let acc, up1, t1 = User.transform_v down acc t.t1 in
          let acc, up2, t2 = User.transform_v down acc t.t2 in
          let acc, up3, t3 = User.transform_v down acc t.t3 in
          acc, reduce_up [ up1; up2; up3 ], Map3 { f = t.f; t1; t2; t3 }
        | Map4 t ->
          let acc, up1, t1 = User.transform_v down acc t.t1 in
          let acc, up2, t2 = User.transform_v down acc t.t2 in
          let acc, up3, t3 = User.transform_v down acc t.t3 in
          let acc, up4, t4 = User.transform_v down acc t.t4 in
          acc, reduce_up [ up1; up2; up3; up4 ], Map4 { f = t.f; t1; t2; t3; t4 }
        | Map5 t ->
          let acc, up1, t1 = User.transform_v down acc t.t1 in
          let acc, up2, t2 = User.transform_v down acc t.t2 in
          let acc, up3, t3 = User.transform_v down acc t.t3 in
          let acc, up4, t4 = User.transform_v down acc t.t4 in
          let acc, up5, t5 = User.transform_v down acc t.t5 in
          let up = reduce_up [ up1; up2; up3; up4; up5 ] in
          acc, up, Map5 { f = t.f; t1; t2; t3; t4; t5 }
        | Map6 t ->
          let acc, up1, t1 = User.transform_v down acc t.t1 in
          let acc, up2, t2 = User.transform_v down acc t.t2 in
          let acc, up3, t3 = User.transform_v down acc t.t3 in
          let acc, up4, t4 = User.transform_v down acc t.t4 in
          let acc, up5, t5 = User.transform_v down acc t.t5 in
          let acc, up6, t6 = User.transform_v down acc t.t6 in
          let up = reduce_up [ up1; up2; up3; up4; up5; up6 ] in
          acc, up, Map6 { f = t.f; t1; t2; t3; t4; t5; t6 }
        | Map7 t ->
          let acc, up1, t1 = User.transform_v down acc t.t1 in
          let acc, up2, t2 = User.transform_v down acc t.t2 in
          let acc, up3, t3 = User.transform_v down acc t.t3 in
          let acc, up4, t4 = User.transform_v down acc t.t4 in
          let acc, up5, t5 = User.transform_v down acc t.t5 in
          let acc, up6, t6 = User.transform_v down acc t.t6 in
          let acc, up7, t7 = User.transform_v down acc t.t7 in
          let up = reduce_up [ up1; up2; up3; up4; up5; up6; up7 ] in
          acc, up, Map7 { f = t.f; t1; t2; t3; t4; t5; t6; t7 }
      in
      acc, up, { value; here; id }
    ;;

    let on_value down acc behavior value =
      match behavior with
      | `Directly_on -> User.transform_v down acc value
      | `Skipping_over -> default_v down acc value
    ;;

    let on_computation down acc behavior computation =
      match behavior with
      | `Directly_on -> User.transform_c down acc computation
      | `Skipping_over -> default_c down acc computation
    ;;
  end

  and User : (Transform with module Types := Types) = F (Recurse)

  let transform_c down acc computation =
    Recurse.on_computation down acc `Directly_on computation
  ;;

  let transform_v down acc value = Recurse.on_value down acc `Directly_on value
end
