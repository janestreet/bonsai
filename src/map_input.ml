open! Core_kernel
open! Import
open Incremental.Let_syntax
open Component

module T = struct
  type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
    | C :
        { t : ('i2, 'model, 'action, 'result, 'incr, 'event) unpacked
        ; f : 'i1 -> 'i2
        }
        -> ('i1, 'model, 'action, 'result, 'incr, 'event) unpacked

  let extension_constructor = [%extension_constructor C]

  let sexp_of_unpacked (type i m a r) (component : (i, m, a, r, _, _) unpacked) =
    match component with
    | C { t; f = _ } -> [%sexp Map_input (t : unpacked)]
    | _ -> assert false
  ;;

  let eval (type i m a r incr event) : (i, m, a, r, incr, event) eval_type =
    fun ~input ~old_model ~model ~inject ~action_type_id ~incr_state t ->
      match t with
      | C { t; f } ->
        let input = input >>| f in
        eval_ext ~input ~old_model ~model ~inject ~action_type_id ~incr_state t
      | _ -> assert false
  ;;

  let map_input (Packed.T { unpacked; action_type_id; model }) ~f =
    Packed.T { unpacked = C { t = unpacked; f }; action_type_id; model }
  ;;

  let visit (Packed.T { unpacked; action_type_id; model }) visitor =
    match unpacked with
    | C { t; f } ->
      let visited =
        visit_ext (Packed.T { unpacked = t; action_type_id; model }) visitor
      in
      visitor.visit (map_input visited ~f)
    | _ -> assert false
  ;;
end

let () = Component.define (module T)

include T
