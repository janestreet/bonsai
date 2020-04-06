open! Core_kernel
open! Import
open Incremental.Let_syntax
open Component

module T = struct
  type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
    | C :
        (('input, 'incr) Incremental.t -> ('result, 'incr) Incremental.t)
        -> ('input, _, Nothing.t, 'result, 'incr, 'event) unpacked

  let sexp_of_unpacked _ = [%sexp Pure_incr]
  let extension_constructor = [%extension_constructor C]
  let apply_action ~schedule_event:_ a = Nothing.unreachable_code a

  let eval (type i m a r incr event) : (i, m, a, r, incr, event) eval_type =
    fun ~input ~old_model:_ ~model:_ ~inject:_ ~action_type_id:_ ~incr_state:_ t ->
    match t with
    | C f ->
      let%map result = f input in
      Snapshot.create ~result ~apply_action
    | _ -> assert false
  ;;

  let visit component (visitor : Visitor.t) = visitor.visit component
end

include T

let () = Component.define (module T)

let pure_incr ~f =
  Packed.T
    { unpacked = C f; action_type_id = nothing_type_id; model = Packed.unit_model_info }
;;
