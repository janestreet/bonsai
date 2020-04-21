open! Core_kernel
open! Import
open Incremental.Let_syntax
open Component

module T = struct
  type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
    | Map1 :
        { t : ('input, 'model, 'action, 'r1, 'incr, 'event) unpacked
        ; f : 'r1 -> 'r2
        }
        -> ('input, 'model, 'action, 'r2, 'incr, 'event) unpacked
    | Map2 :
        { t1 : ('input, 'model1, 'action1, 'r1, 'incr, 'event) unpacked
        ; action_type_id1 : 'action1 Type_equal.Id.t
        ; model1 : 'model1 Packed.model_info
        ; t2 : ('input, 'model2, 'action2, 'r2, 'incr, 'event) unpacked
        ; action_type_id2 : 'action2 Type_equal.Id.t
        ; model2 : 'model2 Packed.model_info
        ; f : 'r1 -> 'r2 -> 'result
        }
        -> ( 'input
           , 'model1 * 'model2
           , ('action1, 'action2) Either.t
           , 'result
           , 'incr
           , 'event )
             unpacked

  let sexp_of_unpacked (type i m a r) (component : (i, m, a, r, _, _) unpacked) =
    match component with
    | Map1 { t; f = _ } -> [%sexp Map (t : unpacked)]
    | Map2 { t1; t2; _ } -> [%sexp Map2, (t1 : unpacked), (t2 : unpacked)]
    | _ -> assert false
  ;;

  let eval (type i m a r incr event) : (i, m, a, r, incr, event) eval_type =
    fun ~input ~old_model ~model ~inject ~action_type_id ~environment ~incr_state t ->
      match t with
      | Map1 { t; f } ->
        let%map snapshot =
          eval_ext
            ~input
            ~old_model
            ~model
            ~inject
            ~action_type_id
            ~environment
            ~incr_state
            t
        in
        Snapshot.create
          ~result:(f (Snapshot.result snapshot))
          ~apply_action:(Snapshot.apply_action snapshot)
      | Map2 { t1; action_type_id1; t2; action_type_id2; f; _ } ->
        let m1 = model >>| Tuple2.get1 in
        let m2 = model >>| Tuple2.get2 in
        let om1 = old_model >>| Option.map ~f:Tuple2.get1 in
        let om2 = old_model >>| Option.map ~f:Tuple2.get2 in
        let%map s1 =
          let inject e = inject (First e) in
          eval_ext
            ~input
            ~old_model:om1
            ~model:m1
            ~inject
            ~action_type_id:action_type_id1
            ~environment
            ~incr_state
            t1
        and s2 =
          let inject e = inject (Second e) in
          eval_ext
            ~input
            ~old_model:om2
            ~model:m2
            ~inject
            ~action_type_id:action_type_id2
            ~environment
            ~incr_state
            t2
        and m1, m2 = model in
        let apply_action ~schedule_event action =
          match action with
          | First action1 -> Snapshot.apply_action s1 action1 ~schedule_event, m2
          | Second action2 -> m1, Snapshot.apply_action s2 action2 ~schedule_event
        in
        let result = f (Snapshot.result s1) (Snapshot.result s2) in
        Snapshot.create ~result ~apply_action
      | _ -> assert false
  ;;
end

include T

let map (Packed.T { unpacked; action_type_id; model }) ~f =
  Packed.T { unpacked = Map1 { t = unpacked; f }; action_type_id; model }
;;

let () =
  Component.define
    (module struct
      include T

      let extension_constructor = [%extension_constructor Map1]

      let visit (Packed.T { unpacked; action_type_id; model }) visitor =
        match unpacked with
        | Map1 { t; f } ->
          let visited = visit_ext (T { unpacked = t; action_type_id; model }) visitor in
          visitor.visit (map visited ~f)
        | _ -> assert false
      ;;
    end)
;;

let map2
      (Packed.T { unpacked = t1; action_type_id = action_type_id1; model = model1 })
      (Packed.T { unpacked = t2; action_type_id = action_type_id2; model = model2 })
      ~f
  =
  let action_type_id =
    Type_equal.Id.create
      ~name:(Source_code_position.to_string [%here])
      (Either.sexp_of_t
         (Type_equal.Id.to_sexp action_type_id1)
         (Type_equal.Id.to_sexp action_type_id2))
  in
  Packed.T
    { unpacked = Map2 { t1; action_type_id1; t2; action_type_id2; f; model1; model2 }
    ; action_type_id
    ; model = Packed.both_model_infos model1 model2
    }
;;

let both = map2 ~f:Tuple2.create

let () =
  Component.define
    (module struct
      include T

      let extension_constructor = [%extension_constructor Map2]

      let visit (Packed.T { unpacked; action_type_id = _; model = _ }) visitor =
        match unpacked with
        | Map2 { t1; action_type_id1; t2; action_type_id2; model1; model2; f } ->
          let visited1 =
            let unpacked, action_type_id, model = t1, action_type_id1, model1 in
            visit_ext (T { unpacked; action_type_id; model }) visitor
          in
          let visited2 =
            let unpacked, action_type_id, model = t2, action_type_id2, model2 in
            visit_ext (T { unpacked; action_type_id; model }) visitor
          in
          visitor.visit (map2 visited1 visited2 ~f)
        | _ -> assert false
      ;;
    end)
;;
