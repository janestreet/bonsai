open! Core_kernel
open! Import
open Component

module T = struct
  type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
    | C : 'result -> (_, _, Nothing.t, 'result, 'incr, _) unpacked

  let sexp_of_unpacked _ = [%sexp Const]
  let apply_action ~schedule_event:_ a = Nothing.unreachable_code a
  let extension_constructor = [%extension_constructor C]

  let eval (type i m a r incr event) : (i, m, a, r, incr, event) eval_type =
    fun ~input:_
      ~old_model:_
      ~model:_
      ~inject:_
      ~action_type_id:_
      ~environment:_
      ~incr_state
      t ->
      match t with
      | C result -> Incremental.return incr_state (Snapshot.create ~result ~apply_action)
      | _ -> assert false
  ;;

  let visit component (visitor : Visitor.t) = visitor.visit component
end

include T

let () = Component.define (module T)

let const r =
  Packed.T
    { unpacked = C r; action_type_id = nothing_type_id; model = Packed.unit_model_info }
;;
