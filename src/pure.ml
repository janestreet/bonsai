open! Core_kernel
open! Import
open Incremental.Let_syntax
open Component

module T = struct
  type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
    | Pure_input :
        ('input -> 'result)
        -> ('input, _, Nothing.t, 'result, 'incr, _) unpacked
    | Pure_model :
        ('model -> 'result)
        -> (_, 'model, Nothing.t, 'result, 'incr, _) unpacked
    | Return_input : ('input, _, Nothing.t, 'input, 'incr, _) unpacked
    | Return_model : (_, 'model, Nothing.t, 'model, 'incr, _) unpacked

  let apply_action ~schedule_event:_ a = Nothing.unreachable_code a

  let eval (type i m a r incr event) : (i, m, a, r, incr, event) eval_type =
    fun ~input ~old_model:_ ~model ~inject:_ ~action_type_id:_ ~incr_state:_ t ->
    match t with
    | Pure_input f ->
      let%map input = input in
      Snapshot.create ~result:(f input) ~apply_action
    | Pure_model f ->
      let%map model = model in
      Snapshot.create ~result:(f model) ~apply_action
    | Return_input ->
      let%map input = input in
      Snapshot.create ~result:input ~apply_action
    | Return_model ->
      let%map model = model in
      Snapshot.create ~result:model ~apply_action
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

      let sexp_of_unpacked _ = [%sexp Pure_model]
      let extension_constructor = [%extension_constructor Pure_model]
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

let () =
  Component.define
    (module struct
      include T

      let sexp_of_unpacked _ = [%sexp Return_model]
      let extension_constructor = [%extension_constructor Return_model]
    end)
;;
