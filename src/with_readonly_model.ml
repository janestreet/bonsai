open! Core_kernel
open! Import
open Incremental.Let_syntax
open Component

module T = struct
  type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
    | C :
        ('input * 'model, unit, 'action, 'result, 'incr, 'event) unpacked
        -> ('input, 'model, 'action, 'result, 'incr, 'event) unpacked

  let sexp_of_unpacked (type i m a r) (component : (i, m, a, r, _, _) unpacked) =
    match component with
    | C t -> [%sexp With_readonly_model (t : unpacked)]
    | _ -> assert false
  ;;

  let extension_constructor = [%extension_constructor C]

  let eval (type i m a r incr event) : (i, m, a, r, incr, event) eval_type =
    fun ~input ~old_model ~model ~inject ~action_type_id ~incr_state t ->
    match t with
    | C t ->
      let old_model = old_model >>| Option.map ~f:(Fn.const ()) in
      let%map snapshot =
        Component.eval_ext
          t
          ~input:(Incremental.both input model)
          ~model:(Incremental.const incr_state ())
          ~old_model
          ~inject
          ~action_type_id
          ~incr_state
      and model = model in
      let result = Snapshot.result snapshot in
      let apply_action ~schedule_event:_ _action = model in
      Snapshot.create ~result ~apply_action
    | _ -> assert false
  ;;

  let visit component visitor =
    match component with
    | Packed.T (C t, typ_id) ->
      let (Packed.T (t, typ_id)) = visit_ext (Packed.T (t, typ_id)) visitor in
      visitor.visit (Packed.T (C t, typ_id))
    | _ -> assert false
  ;;
end

include T

let () = Component.define (module T)
