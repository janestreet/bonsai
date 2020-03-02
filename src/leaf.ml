open! Core_kernel
open! Import
open Incremental.Let_syntax
open Component

module T = struct
  type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
    | C :
        { apply_action :
            inject:('action -> 'event)
            -> schedule_event:('event -> unit)
            -> 'input
            -> 'model
            -> 'action
            -> 'model
        ; compute : inject:('action -> 'event) -> 'input -> 'model -> 'result
        ; name : string
        }
        -> ('input, 'model, 'action, 'result, 'incr, 'event) unpacked

  let sexp_of_unpacked t =
    match t with
    | C { apply_action = _; compute = _; name } -> [%sexp Leaf (name : string)]
    | _ -> assert false
  ;;

  let extension_constructor = [%extension_constructor C]

  let eval (type i m a r incr event) : (i, m, a, r, incr, event) eval_type =
    fun ~input ~old_model:_ ~model ~inject ~action_type_id:_ ~incr_state:_ t ->
    match t with
    | C { apply_action; compute; name = _ } ->
      let%map input = input
      and model = model in
      let result = compute input model ~inject in
      let apply_action = apply_action input model ~inject in
      Snapshot.create ~result ~apply_action
    | _ -> assert false
  ;;

  let visit component (visitor : Visitor.t) = visitor.visit component
end

include T

let () = Component.define (module T)
