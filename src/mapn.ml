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
        { t1 : ('input, 'model, 'a1, 'r1, 'incr, 'event) unpacked
        ; action_type_id1 : 'a1 Type_equal.Id.t
        ; t2 : ('input, 'model, 'a2, 'r2, 'incr, 'event) unpacked
        ; action_type_id2 : 'a2 Type_equal.Id.t
        ; f : 'r1 -> 'r2 -> 'result
        }
        -> ('input, 'model, ('a1, 'a2) Either.t, 'result, 'incr, 'event) unpacked

  let sexp_of_unpacked (type i m a r) (component : (i, m, a, r, _, _) unpacked) =
    match component with
    | Map1 { t; f = _ } -> [%sexp Map (t : unpacked)]
    | Map2 { t1; action_type_id1 = _; t2; action_type_id2 = _; f = _ } ->
      [%sexp Map2, (t1 : unpacked), (t2 : unpacked)]
    | _ -> assert false
  ;;

  let eval (type i m a r incr event) : (i, m, a, r, incr, event) eval_type =
    fun ~input ~old_model ~model ~inject ~action_type_id ~incr_state t ->
      match t with
      | Map1 { t; f } ->
        let%map snapshot =
          eval_ext ~input ~old_model ~model ~inject ~action_type_id ~incr_state t
        in
        Snapshot.create
          ~result:(f (Snapshot.result snapshot))
          ~apply_action:(Snapshot.apply_action snapshot)
      | Map2 { t1; action_type_id1; t2; action_type_id2; f } ->
        let%map s1 =
          let inject e = inject (First e) in
          eval_ext
            ~input
            ~old_model
            ~model
            ~inject
            ~action_type_id:action_type_id1
            ~incr_state
            t1
        and s2 =
          let inject e = inject (Second e) in
          eval_ext
            ~input
            ~old_model
            ~model
            ~inject
            ~action_type_id:action_type_id2
            ~incr_state
            t2
        in
        let apply_action ~schedule_event action =
          match action with
          | First action1 -> Snapshot.apply_action s1 action1 ~schedule_event
          | Second action2 -> Snapshot.apply_action s2 action2 ~schedule_event
        in
        let result = f (Snapshot.result s1) (Snapshot.result s2) in
        Snapshot.create ~result ~apply_action
      | _ -> assert false
  ;;

  let visit component visitor =
    match component with
    | Packed.T (Map1 { t; f }, typ_id) ->
      let (T (t, typ_id)) = visit_ext (T (t, typ_id)) visitor in
      visitor.visit (T (Map1 { t; f }, typ_id))
    | Packed.T (Map2 { t1; action_type_id1; t2; action_type_id2; f }, typ_id) ->
      let (T (t1, action_type_id1)) = visit_ext (T (t1, action_type_id1)) visitor in
      let (T (t2, action_type_id2)) = visit_ext (T (t2, action_type_id2)) visitor in
      let typ_id =
        Type_equal.Id.create
          ~name:(Type_equal.Id.name typ_id)
          (Either.sexp_of_t
             (Type_equal.Id.to_sexp action_type_id1)
             (Type_equal.Id.to_sexp action_type_id2))
      in
      visitor.visit (T (Map2 { t1; action_type_id1; t2; action_type_id2; f }, typ_id))
    | _ -> assert false
  ;;
end

include T

let () =
  Component.define
    (module struct
      include T

      let extension_constructor = [%extension_constructor Map1]
    end)
;;

let () =
  Component.define
    (module struct
      include T

      let extension_constructor = [%extension_constructor Map2]
    end)
;;
