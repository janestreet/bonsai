open! Core_kernel
open! Import
open Incremental.Let_syntax
open Component

module T = struct
  type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
    | Model :
        { t : ('input, 'model, 'action, 'result, 'incr, 'event) unpacked
        ; model_equal : 'model -> 'model -> bool
        }
        -> ('input, 'model, 'action, 'result, 'incr, 'event) unpacked
    | Value :
        'input Incremental.Cutoff.t
        -> ('input, _, Nothing.t, 'input, 'incr, 'event) unpacked

  let sexp_of_unpacked (type i m a r) (component : (i, m, a, r, _, _) unpacked) =
    match component with
    | Model { t; model_equal = _ } -> [%sexp Model_cutoff { t : unpacked }]
    | Value cutoff -> [%sexp Value_cutoff { cutoff : _ Incremental.Cutoff.t }]
    | _ -> assert false
  ;;

  let apply_action ~schedule_event:_ a = Nothing.unreachable_code a

  let eval (type i m a r incr event) : (i, m, a, r, incr, event) eval_type =
    fun ~input ~old_model ~model ~inject ~action_type_id ~incr_state t ->
    match t with
    | Model { t; model_equal } ->
      let model = model >>| Fn.id in
      let old_model = old_model >>| Fn.id in
      let cutoff = Incremental.Cutoff.of_equal model_equal in
      Incremental.set_cutoff model cutoff;
      Incremental.set_cutoff
        old_model
        (Incremental.Cutoff.create (fun ~old_value ~new_value ->
           match old_value, new_value with
           | None, None -> true
           | Some old_value, Some new_value ->
             Incremental.Cutoff.should_cutoff cutoff ~old_value ~new_value
           | None, Some _ | Some _, None -> false));
      Component.eval_ext ~input ~model ~old_model ~inject ~action_type_id ~incr_state t
    | Value cutoff ->
      let input = input >>| Fn.id in
      Incremental.set_cutoff input cutoff;
      let%map input = input in
      Snapshot.create ~result:input ~apply_action
    | _ -> assert false
  ;;
end

include T

let value_cutoff ~cutoff =
  Packed.T
    { unpacked = Value cutoff
    ; action_type_id = nothing_type_id
    ; model = Packed.unit_model_info
    }
;;

let model_cutoff (Packed.T { unpacked; action_type_id; model }) =
  Packed.T
    { unpacked = Model { t = unpacked; model_equal = model.equal }
    ; action_type_id
    ; model
    }
;;

let () =
  Component.define
    (module struct
      include T

      let extension_constructor = [%extension_constructor Model]

      let visit (Packed.T { unpacked; action_type_id; model }) visitor =
        match unpacked with
        | Model { t; model_equal = _ } ->
          let visited =
            visit_ext (Packed.T { unpacked = t; action_type_id; model }) visitor
          in
          visitor.visit (model_cutoff visited)
        | _ -> assert false
      ;;
    end)
;;

let () =
  Component.define
    (module struct
      include T

      let extension_constructor = [%extension_constructor Value]
      let visit component (visitor : Visitor.t) = visitor.visit component
    end)
;;
