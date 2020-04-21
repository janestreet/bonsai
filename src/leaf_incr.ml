open! Core_kernel
open! Import
open Incremental.Let_syntax
open Component

module T = struct
  type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
    | C :
        { apply_action :
            ('input, 'incr) Incremental.t
            -> ('model, 'incr) Incremental.t
            -> inject:('action -> 'event)
            -> ( schedule_event:('event -> unit) -> 'action -> 'model
               , 'incr )
                 Incremental.t
        ; compute :
            ('input, 'incr) Incremental.t
            -> ('model, 'incr) Incremental.t
            -> inject:('action -> 'event)
            -> ('result, 'incr) Incremental.t
        ; name : string
        }
        -> ('input, 'model, 'action, 'result, 'incr, 'event) unpacked

  let sexp_of_unpacked t =
    match t with
    | C { apply_action = _; compute = _; name } -> [%sexp Leaf_incr (name : string)]
    | _ -> assert false
  ;;

  let extension_constructor = [%extension_constructor C]

  let eval (type i m a r incr event) : (i, m, a, r, incr, event) eval_type =
    fun ~input
      ~old_model:_
      ~model
      ~inject
      ~action_type_id:_
      ~environment:_
      ~incr_state:_
      t ->
      match t with
      | C { apply_action; compute; name = _ } ->
        let%map apply_action = apply_action input model ~inject
        and result = compute input model ~inject in
        Snapshot.create ~result ~apply_action
      | _ -> assert false
  ;;

  let visit component (visitor : Visitor.t) = visitor.visit component
end

include T

let leaf_incr
      (type m a)
      (module M : Model with type t = m)
      (module A : Action with type t = a)
      ~name
      ~default_model
      ~apply_action
      ~compute
  =
  let action_type_id = Type_equal.Id.create ~name A.sexp_of_t in
  let model_type_id = Type_equal.Id.create ~name M.sexp_of_t in
  Packed.T
    { unpacked = C { apply_action; compute; name }
    ; action_type_id
    ; model =
        { default = default_model
        ; type_id = model_type_id
        ; equal = M.equal
        ; sexp_of = M.sexp_of_t
        ; of_sexp = M.t_of_sexp
        }
    }
;;

let () = Component.define (module T)
