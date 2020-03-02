open! Core_kernel
open! Import
open Incremental.Let_syntax
open Component

module T = struct
  type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
    | C :
        { t1 : ('input, 'm1, 'a1, 'r1, 'incr, 'event) unpacked
        ; action_type_id1 : 'a1 Type_equal.Id.t
        ; t2 : ('input, 'm2, 'a2, 'r2, 'incr, 'event) unpacked
        ; action_type_id2 : 'a2 Type_equal.Id.t
        ; on_action_for_other_component :
            ( [ `Action_for_first of 'm2 | `Action_for_second of 'm1 ]
            , ('m1, 'm2) Either.t )
              on_action_mismatch
        }
        -> ( 'input
           , ('m1, 'm2) Either.t
           , ('a1, 'a2) Either.t
           , ('r1, 'r2) Either.t
           , 'incr
           , 'event )
             unpacked

  let extension_constructor = [%extension_constructor C]

  let sexp_of_unpacked (type i m a r) (component : (i, m, a, r, _, _) unpacked) =
    match component with
    | C
        { t1
        ; action_type_id1 = _
        ; t2
        ; action_type_id2 = _
        ; on_action_for_other_component = _
        } -> [%sexp Either, (t1 : unpacked), (t2 : unpacked)]
    | _ -> assert false
  ;;

  let eval (type i m a r incr event) : (i, m, a, r, incr, event) eval_type =
    fun ~input ~old_model ~model ~inject ~action_type_id ~incr_state t ->
      let sexp_of_action = Type_equal.Id.to_sexp action_type_id in
      match t with
      | C { t1; action_type_id1; t2; action_type_id2; on_action_for_other_component } ->
        (* I couldn't figure out how to pull each case out into another function even
           though they are similar. *)
        let return = Incremental.return incr_state in
        let outer_model = model in
        (match%pattern_bind outer_model with
         | First model ->
           let old_model =
             match%pattern_map old_model with
             | Some (First om) -> Some om
             | Some (Second _) | None -> None
           in
           let inject a = inject (First a) in
           let snapshot =
             eval_ext
               ~input
               ~model
               ~old_model
               ~inject
               ~action_type_id:action_type_id1
               ~incr_state
               t1
           in
           Incremental.map3 snapshot outer_model model ~f:(fun snapshot outer_model model ->
             let result = First (Snapshot.result snapshot) in
             let apply_action ~schedule_event action =
               let error_message action =
                 [%message
                   "Either component is in its First state, but got an action intended \
                    for the Second state"
                     (action : action)]
               in
               match action, on_action_for_other_component with
               | First a, _ -> First (Snapshot.apply_action snapshot ~schedule_event a)
               | Second _, `Ignore -> outer_model
               | Second _, `Warn ->
                 eprint_s (error_message action);
                 outer_model
               | Second _, `Raise -> raise_s (error_message action)
               | Second _, `Custom handler -> handler (`Action_for_second model)
             in
             Snapshot.create ~result ~apply_action)
         | Second model ->
           let return = Incremental.return incr_state in
           let old_model =
             match%pattern_map old_model with
             | Some (Second om) -> Some om
             | Some (First _) | None -> None
           in
           let inject a = inject (Second a) in
           let snapshot =
             eval_ext
               ~input
               ~model
               ~old_model
               ~inject
               ~action_type_id:action_type_id2
               ~incr_state
               t2
           in
           Incremental.map3 snapshot outer_model model ~f:(fun snapshot outer_model model ->
             let result = Second (Snapshot.result snapshot) in
             let apply_action ~schedule_event action =
               let error_message action =
                 [%message
                   "Either component is in its Second state, but got an action intended \
                    for the First state"
                     (action : action)]
               in
               match action, on_action_for_other_component with
               | Second a, _ -> Second (Snapshot.apply_action snapshot ~schedule_event a)
               | First _, `Ignore -> outer_model
               | First _, `Warn ->
                 eprint_s (error_message action);
                 outer_model
               | First _, `Raise -> raise_s (error_message action)
               | First _, `Custom handler -> handler (`Action_for_first model)
             in
             Snapshot.create ~result ~apply_action))
      | _ -> assert false
  ;;

  let visit (type i m r incr event) (component : (i, m, r, incr, event) Packed.t) visitor
    : (i, m, r, incr, event) Packed.t
    =
    match component with
    | Packed.T
        ( C { t1; action_type_id1; t2; action_type_id2; on_action_for_other_component }
        , typ_id ) ->
      let (T (t1, action_type_id1)) = visit_ext (T (t1, action_type_id1)) visitor in
      let (T (t2, action_type_id2)) = visit_ext (T (t2, action_type_id2)) visitor in
      let typ_id =
        Type_equal.Id.create
          ~name:(Type_equal.Id.name typ_id)
          (Either.sexp_of_t
             (Type_equal.Id.to_sexp action_type_id1)
             (Type_equal.Id.to_sexp action_type_id2))
      in
      visitor.visit
        (Packed.T
           ( C { t1; action_type_id1; t2; action_type_id2; on_action_for_other_component }
           , typ_id ))
    | _ -> assert false
  ;;
end

include T

let () = Component.define (module T)
