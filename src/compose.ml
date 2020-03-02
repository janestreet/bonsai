open! Core_kernel
open! Import
open Incremental.Let_syntax
open Component

module T = struct
  type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
    | C :
        { t1 : ('i1, 'model, 'a1, 'r1, 'incr, 'event) unpacked
        ; action_type_id1 : 'a1 Type_equal.Id.t
        ; t2 : ('r1, 'model, 'a2, 'r2, 'incr, 'event) unpacked
        ; action_type_id2 : 'a2 Type_equal.Id.t
        }
        -> ('i1, 'model, ('a1, 'a2) Either.t, 'r2, 'incr, 'event) unpacked

  let sexp_of_unpacked (type i m a r) (component : (i, m, a, r, _, _) unpacked) =
    match component with
    | C { t1; action_type_id1 = _; t2; action_type_id2 = _ } ->
      [%sexp Compose, (t1 : unpacked), (t2 : unpacked)]
    | _ -> assert false
  ;;

  let extension_constructor = [%extension_constructor C]

  let eval (type i m a r incr event) : (i, m, a, r, incr, event) eval_type =
    fun ~input ~old_model ~model ~inject ~action_type_id:_ ~incr_state t ->
    match t with
    | C { t1; action_type_id1; t2; action_type_id2 } ->
      let s1 =
        let inject e = inject (First e) in
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
      and s2 = s2 in
      let apply_action1 = Snapshot.apply_action s1
      and apply_action2 = Snapshot.apply_action s2
      and result2 = Snapshot.result s2 in
      let apply_action ~schedule_event action =
        match action with
        | First action1 -> apply_action1 action1 ~schedule_event
        | Second action2 -> apply_action2 action2 ~schedule_event
      in
      Snapshot.create ~result:result2 ~apply_action
    | _ -> assert false
  ;;

  let visit component visitor =
    match component with
    | Packed.T (C { t1; action_type_id1; t2; action_type_id2 }, typ_id) ->
      let (T (t1, action_type_id1)) = visit_ext (T (t1, action_type_id1)) visitor in
      let (T (t2, action_type_id2)) = visit_ext (T (t2, action_type_id2)) visitor in
      let typ_id =
        Type_equal.Id.create
          ~name:(Type_equal.Id.name typ_id)
          (Either.sexp_of_t
             (Type_equal.Id.to_sexp action_type_id1)
             (Type_equal.Id.to_sexp action_type_id2))
      in
      visitor.visit (T (C { t1; action_type_id1; t2; action_type_id2 }, typ_id))
    | _ -> assert false
  ;;
end

include T

let () = Component.define (module T)
