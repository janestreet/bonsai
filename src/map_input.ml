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

  let visit component visitor =
    match component with
    | Packed.T (C { t; f }, typ_id) ->
      let (T (t, typ_id)) = visit_ext (T (t, typ_id)) visitor in
      visitor.visit (T (C { t; f }, typ_id))
    | _ -> assert false
  ;;
end

let () = Component.define (module T)

include T
