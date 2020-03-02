open! Core_kernel
open! Import
open Incremental.Let_syntax
open Component

module T = struct
  type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
    | C :
        { t : ('input, 'model, 'action, 'result, 'incr, 'event) unpacked
        ; on_action_for_none : (unit, 'model option) on_action_mismatch
        }
        -> ('input, 'model option, 'action, 'result option, 'incr, 'event) unpacked

  let extension_constructor = [%extension_constructor C]

  let sexp_of_unpacked (type i m a r) (component : (i, m, a, r, _, _) unpacked) =
    match component with
    | C { t; on_action_for_none = _ } -> [%sexp Option (t : unpacked)]
    | _ -> assert false
  ;;

  let eval (type i m a r incr event) : (i, m, a, r, incr, event) eval_type =
    fun ~input ~old_model ~model ~inject ~action_type_id ~incr_state t ->
      let sexp_of_action = Type_equal.Id.to_sexp action_type_id in
      match t with
      | C { t; on_action_for_none } ->
        let none_old_model = Incremental.const incr_state None in
        let error_message action =
          [%message
            "Option component received an action while its model was None"
              (action : action)]
        in
        (match%pattern_bind model with
         | None ->
           let apply_action =
             match on_action_for_none with
             | `Ignore -> fun ~schedule_event:_ _ -> None
             | `Warn ->
               fun ~schedule_event:_ action ->
                 eprint_s (error_message action);
                 None
             | `Raise -> fun ~schedule_event:_ action -> raise_s (error_message action)
             | `Custom handler -> fun ~schedule_event:_ _ -> handler ()
           in
           Incremental.return incr_state (Snapshot.create ~result:None ~apply_action)
         | Some model ->
           let%map snapshot =
             match%pattern_bind old_model with
             | Some old_model ->
               eval_ext ~input ~old_model ~model ~inject ~action_type_id ~incr_state t
             | None ->
               eval_ext
                 ~input
                 ~old_model:none_old_model
                 ~model
                 ~inject
                 ~action_type_id
                 ~incr_state
                 t
           in
           let result = Some (Snapshot.result snapshot) in
           let apply_action ~schedule_event a =
             Some (Snapshot.apply_action snapshot ~schedule_event a)
           in
           Snapshot.create ~result ~apply_action)
      | _ -> assert false
  ;;

  let visit (type i m r incr event) (component : (i, m, r, incr, event) Packed.t) visitor
    : (i, m, r, incr, event) Packed.t
    =
    match component with
    | Packed.T (C { t; on_action_for_none }, typ_id) ->
      let (T (t, typ_id)) = visit_ext (T (t, typ_id)) visitor in
      visitor.visit (T (C { t; on_action_for_none }, typ_id))
    | _ -> assert false
  ;;
end

include T

let () = Component.define (module T)
