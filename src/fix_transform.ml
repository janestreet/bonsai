open! Core
open! Import
include Fix_transform_intf

module Make
    (Types : Types)
    (F : functor
       (_ : Recurse with module Types := Types)
       -> Transform with module Types := Types) : Transform with module Types := Types =
struct
  module rec Recurse : (Recurse with module Types := Types) = struct
    let combine_up, empty, empty_for_lazy = Types.Up.(combine, empty, empty_for_lazy)

    open Trampoline.Let_syntax

    let default_c (type a) down acc (computation : a Computation.t)
      : (_ * _ * a Computation.t) Trampoline.t
      =
      match computation with
      | Return { value; here } ->
        let acc, up, value = User.transform_v down acc value in
        return (acc, up, Computation.Return { value; here })
      | Leaf1 { model; input_id; dynamic_action; apply_action; input; reset; here } ->
        let acc, up, input = User.transform_v down acc input in
        return
          ( acc
          , up
          , Computation.Leaf1
              { model; input_id; dynamic_action; apply_action; input; reset; here } )
      | Leaf0 { model; static_action; apply_action; reset; here } ->
        return
          ( acc
          , empty
          , Computation.Leaf0 { model; static_action; apply_action; reset; here } )
      | Leaf_incr { input; compute; here } ->
        let acc, up, input = User.transform_v down acc input in
        return (acc, up, Computation.Leaf_incr { input; compute; here })
      | Sub { from; via; into; invert_lifecycles; here } ->
        let%bind acc, up1, from = User.transform_c down acc from in
        let%bind acc, up2, into = User.transform_c down acc into in
        return
          ( acc
          , combine_up up1 up2
          , Computation.Sub { from; via; into; invert_lifecycles; here } )
      | Store { id; value; inner; here } ->
        let acc, up1, value = User.transform_v down acc value in
        let%bind acc, up2, inner = User.transform_c down acc inner in
        return (acc, combine_up up1 up2, Computation.Store { id; value; inner; here })
      | Fetch { id; default; for_some; here } ->
        return (acc, empty, Computation.Fetch { id; default; for_some; here })
      | Assoc { map; key_comparator; key_id; cmp_id; data_id; by; here } ->
        let acc, up1, map = User.transform_v down acc map in
        let%bind acc, up2, by = User.transform_c down acc by in
        return
          ( acc
          , combine_up up1 up2
          , Computation.Assoc { map; key_comparator; key_id; cmp_id; data_id; by; here }
          )
      | Assoc_on t ->
        let acc, up1, map = User.transform_v down acc t.map in
        let%bind acc, up2, by = User.transform_c down acc t.by in
        return (acc, combine_up up1 up2, Computation.Assoc_on { t with map; by })
      | Assoc_simpl { map; by; may_contain; here } ->
        let acc, up, map = User.transform_v down acc map in
        return (acc, up, Computation.Assoc_simpl { map; by; may_contain; here })
      | Switch { match_; arms; here } ->
        let acc, up1, match_ = User.transform_v down acc match_ in
        let acc_and_upn_and_arms =
          arms
          |> Map.to_alist
          |> List.fold
               ~init:(return (acc, up1, []))
               ~f:(fun acc_and_up_and_arms (k, v) ->
                 let%bind acc, up, arms = acc_and_up_and_arms in
                 let%bind acc, up', v = User.transform_c down acc v in
                 return (acc, combine_up up up', (k, v) :: arms))
        in
        let%bind acc, upn, arms = acc_and_upn_and_arms in
        let arms = Map.of_alist_exn (module Int) arms in
        return (acc, upn, Computation.Switch { match_; arms; here })
      | Fix_define { result; initial_input; fix_id; input_id; here } ->
        let acc, up1, initial_input = User.transform_v down acc initial_input in
        let%bind acc, up2, result = User.transform_c down acc result in
        return
          ( acc
          , combine_up up1 up2
          , Computation.Fix_define { result; initial_input; fix_id; input_id; here } )
      | Fix_recurse { fix_id; input; input_id; here } ->
        let acc, up, input = User.transform_v down acc input in
        return (acc, up, Computation.Fix_recurse { fix_id; input; input_id; here })
      | Lazy { t; here } ->
        let t =
          Incr.lazy_from_fun (fun () ->
            (* In case any incremental nodes are created inside the transform (e.g.
               [Incr.map] via [Computation_watcher.instrument_incremental_node]), we need
               to ensure that we preserve the current scope no matter where we force the
               lazy. If we ever remove the call to Incr.map, we can revert this case to
               just calling [Lazy.map]. *)
            let t = Lazy.force t in
            Trampoline.run
              (let%bind _acc, _up, t = User.transform_c down acc t in
               return t))
        in
        return (acc, empty_for_lazy, Computation.Lazy { t; here })
      | Wrap
          { wrapper_model
          ; action_id
          ; result_id
          ; inject_id
          ; model_id
          ; inner
          ; dynamic_apply_action
          ; reset
          ; here
          } ->
        let%bind acc, up, inner = User.transform_c down acc inner in
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
            ; here
            }
        in
        return (acc, up, res)
      | With_model_resetter { inner; reset_id; here } ->
        let%bind acc, up, inner = User.transform_c down acc inner in
        return (acc, up, Computation.With_model_resetter { inner; reset_id; here })
      | Path { here } -> return (acc, empty, Computation.Path { here })
      | Lifecycle { lifecycle = value; here } ->
        let acc, up, value = User.transform_v down acc value in
        return (acc, up, Computation.Lifecycle { lifecycle = value; here })
      | Computation_watcher
          { inner
          ; here
          ; free_vars
          ; config
          ; queue
          ; value_id_observation_definition_positions
          ; enable_watcher
          } ->
        let%bind acc, up, inner = User.transform_c down acc inner in
        return
          ( acc
          , up
          , Computation.Computation_watcher
              { inner
              ; here
              ; free_vars
              ; config
              ; queue
              ; value_id_observation_definition_positions
              ; enable_watcher
              } )
    ;;

    let reduce_up l = List.reduce l ~f:combine_up |> Option.value ~default:empty

    let default_v (type a) down acc ({ value; here } : a Value.t) : _ * _ * a Value.t =
      let acc, up, value =
        match value with
        | Constant (c : a Lazy.t) -> acc, empty, Value.Constant c
        | Exception (e : exn) -> acc, empty, Exception e
        | Incr incr_node -> acc, empty, Incr incr_node
        | Named ((name_source, id) : Value.Name_source.t * a Type_equal.Id.t) ->
          acc, empty, Named (name_source, id)
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
      acc, up, { value; here }
    ;;

    let on_value down acc behavior value =
      match behavior with
      | `Directly_on -> User.transform_v down acc value
      | `Skipping_over -> default_v down acc value
    ;;

    let on_computation down acc behavior computation =
      Trampoline.lazy_
        (lazy
          (match behavior with
           | `Directly_on -> User.transform_c down acc computation
           | `Skipping_over -> default_c down acc computation))
    ;;
  end

  and User : (Transform with module Types := Types) = F (Recurse)

  let transform_c down acc computation =
    Recurse.on_computation down acc `Directly_on computation
  ;;

  let transform_v down acc value = Recurse.on_value down acc `Directly_on value
end
