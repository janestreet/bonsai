open! Core_kernel
open! Import
open Incremental.Let_syntax
open Component

module T = struct
  type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
    | C :
        { t : ('input, 'model, 'action, 'r1, 'incr, 'event) unpacked
        ; f : ('r1, 'incr) Incremental.t -> ('r2, 'incr) Incremental.t
        }
        -> ('input, 'model, 'action, 'r2, 'incr, 'event) unpacked

  let extension_constructor = [%extension_constructor C]

  let sexp_of_unpacked t =
    match t with
    | C { t; f = _ } -> [%sexp Map_incr (t : unpacked)]
    | _ -> assert false
  ;;

  let eval (type i m a r incr event) : (i, m, a, r, incr, event) eval_type =
    fun ~input ~old_model ~model ~inject ~action_type_id ~incr_state t ->
      match t with
      | C { t; f } ->
        let snapshot =
          eval_ext ~input ~old_model ~model ~inject ~action_type_id ~incr_state t
        in
        let result = snapshot >>| Snapshot.result |> f in
        let apply_action = snapshot >>| Snapshot.apply_action in
        let%map result = result
        and apply_action = apply_action in
        Snapshot.create ~result ~apply_action
      | _ -> assert false
  ;;

  let visit component visitor =
    match component with
    | Packed.T (C { t; f }, typ_id) ->
      let (T (t, typ_id)) = visit_ext (T (t, typ_id)) visitor in
      visitor.visit (T (C { t; f }, typ_id))
    | _ -> assert false
  ;;
end

include T

let () = Component.define (module T)
