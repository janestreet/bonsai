open! Core_kernel
open! Import
open Component

type ('input, 'model, 'action, 'result, 'incr, 'event) t =
  input:('input, 'incr) Incremental.t
  -> old_model:('model option, 'incr) Incremental.t
  -> model:('model, 'incr) Incremental.t
  -> inject:('action -> 'event)
  -> environment:'incr Bonsai_types.Environment.t
  -> incr_state:'incr Incremental.State.t
  -> (('model, 'action, 'result, 'event) Snapshot.t, 'incr) Incremental.t

module T = struct
  type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
    | C :
        { f : ('input, 'model, 'action, 'result, 'incr, 'event) t
        ; constructed_at : Source_code_position.t
        }
        -> ('input, 'model, 'action, 'result, 'incr, 'event) unpacked

  let sexp_of_unpacked (type i m a r) (component : (i, m, a, r, _, _) unpacked) =
    match component with
    | C { f = _; constructed_at } ->
      [%sexp Full { constructed_at : Source_code_position.t }]
    | _ -> assert false
  ;;

  let extension_constructor = [%extension_constructor C]

  let eval (type i m a r incr event) : (i, m, a, r, incr, event) eval_type =
    fun ~input ~old_model ~model ~inject ~action_type_id:_ ~environment ~incr_state t ->
    match t with
    | C { f; constructed_at = _ } ->
      f ~input ~old_model ~model ~inject ~environment ~incr_state
    | _ -> assert false
  ;;

  let visit component (visitor : Visitor.t) = visitor.visit component
end

include T

let () = Component.define (module T)

let of_full
      constructed_at
      ~f
      ~action_type_id
      ~model_type_id
      ~default_model
      ~model_equal
      ~sexp_of_model
      ~model_of_sexp
  =
  Packed.T
    { unpacked = C { f; constructed_at }
    ; action_type_id
    ; model =
        { type_id = model_type_id
        ; default = default_model
        ; equal = model_equal
        ; sexp_of = sexp_of_model
        ; of_sexp = model_of_sexp
        }
    }
;;
