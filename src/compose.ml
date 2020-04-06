open! Core_kernel
open! Import
open Incremental.Let_syntax
open Component

module T = struct
  type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
    | C :
        { t1 : ('i1, 'm1, 'a1, 'r1, 'incr, 'event) unpacked
        ; model1 : 'm1 Packed.model_info
        ; action_type_id1 : 'a1 Type_equal.Id.t
        ; t2 : ('r1, 'm2, 'a2, 'r2, 'incr, 'event) unpacked
        ; model2 : 'm2 Packed.model_info
        ; action_type_id2 : 'a2 Type_equal.Id.t
        }
        -> ('i1, 'm1 * 'm2, ('a1, 'a2) Either.t, 'r2, 'incr, 'event) unpacked

  let sexp_of_unpacked (type i m a r) (component : (i, m, a, r, _, _) unpacked) =
    match component with
    | C { t1; t2; _ } -> [%sexp Compose, (t1 : unpacked), (t2 : unpacked)]
    | _ -> assert false
  ;;

  let extension_constructor = [%extension_constructor C]

  let eval (type i m a r incr event) : (i, m, a, r, incr, event) eval_type =
    fun ~input ~old_model ~model ~inject ~action_type_id:_ ~incr_state t ->
    match t with
    | C { t1; t2; action_type_id1; action_type_id2; model1 = _; model2 = _ } ->
      let s1 =
        let inject e = inject (First e) in
        let model = model >>| Tuple2.get1
        and old_model = old_model >>| Option.map ~f:Tuple2.get1 in
        eval_ext
          t1
          ~input
          ~old_model
          ~model
          ~inject
          ~action_type_id:action_type_id1
          ~incr_state
      in
      let s2 =
        let inject e = inject (Second e) in
        let model = model >>| Tuple2.get2
        and old_model = old_model >>| Option.map ~f:Tuple2.get2 in
        eval_ext
          t2
          ~input:(s1 >>| Snapshot.result)
          ~old_model
          ~model
          ~inject
          ~action_type_id:action_type_id2
          ~incr_state
      in
      let%map s1 = s1
      and s2 = s2
      and m1, m2 = model in
      let apply_action1 = Snapshot.apply_action s1
      and apply_action2 = Snapshot.apply_action s2
      and result2 = Snapshot.result s2 in
      let apply_action ~schedule_event action =
        match action with
        | First action1 -> apply_action1 action1 ~schedule_event, m2
        | Second action2 -> m1, apply_action2 action2 ~schedule_event
      in
      Snapshot.create ~result:result2 ~apply_action
    | _ -> assert false
  ;;

  let compose
        Packed.(T { unpacked = t1; action_type_id = action_type_id1; model = model1 })
        Packed.(T { unpacked = t2; action_type_id = action_type_id2; model = model2 })
    =
    let action_type_id =
      Type_equal.Id.create
        ~name:
          (sprintf
             "(%s, %s) Either.t"
             (Type_equal.Id.name action_type_id1)
             (Type_equal.Id.name action_type_id2))
        (Either.sexp_of_t
           (Type_equal.Id.to_sexp action_type_id1)
           (Type_equal.Id.to_sexp action_type_id2))
    in
    let model = Packed.both_model_infos model1 model2 in
    Packed.T
      { unpacked = C { t1; t2; action_type_id1; action_type_id2; model1; model2 }
      ; action_type_id
      ; model
      }
  ;;

  let visit (Packed.T { unpacked; action_type_id = _; model = _ }) visitor =
    match unpacked with
    | C { t1; action_type_id1; t2; action_type_id2; model1; model2 } ->
      let t1 =
        visit_ext
          (T { unpacked = t1; action_type_id = action_type_id1; model = model1 })
          visitor
      in
      let t2 =
        visit_ext
          (T { unpacked = t2; action_type_id = action_type_id2; model = model2 })
          visitor
      in
      visitor.visit (compose t1 t2)
    | _ -> assert false
  ;;
end

include T

let () = Component.define (module T)
