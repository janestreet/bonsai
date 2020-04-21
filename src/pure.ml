open! Core_kernel
open! Import
open Incremental.Let_syntax
open Component

module T = struct
  type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
    | Pure_input :
        ('input -> 'result)
        -> ('input, _, Nothing.t, 'result, 'incr, _) unpacked
    | Return_input : ('input, _, Nothing.t, 'input, 'incr, _) unpacked

  let apply_action ~schedule_event:_ a = Nothing.unreachable_code a

  let eval (type i m a r incr event) : (i, m, a, r, incr, event) eval_type =
    fun ~input
      ~old_model:_
      ~model:_
      ~inject:_
      ~action_type_id:_
      ~environment:_
      ~incr_state:_
      t ->
      match t with
      | Pure_input f ->
        let%map input = input in
        Snapshot.create ~result:(f input) ~apply_action
      | Return_input ->
        let%map input = input in
        Snapshot.create ~result:input ~apply_action
      | _ -> assert false
  ;;

  let visit component (visitor : Visitor.t) = visitor.visit component
end

include T

let () =
  Component.define
    (module struct
      include T

      let sexp_of_unpacked _ = [%sexp Pure_input]
      let extension_constructor = [%extension_constructor Pure_input]
    end)
;;

let () =
  Component.define
    (module struct
      include T

      let sexp_of_unpacked _ = [%sexp Return_input]
      let extension_constructor = [%extension_constructor Return_input]
    end)
;;

let action_type_id = nothing_type_id

let pure ~f =
  Packed.T { unpacked = Pure_input f; action_type_id; model = Packed.unit_model_info }
;;

let input =
  Packed.T { unpacked = Return_input; action_type_id; model = Packed.unit_model_info }
;;
