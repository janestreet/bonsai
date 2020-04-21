open! Core_kernel
open! Import
open Component
open Incremental.Let_syntax

module T = struct
  type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
    | Abstraction :
        { t : (unit, 'model, 'action, 'result, 'incr, 'event) unpacked
        ; type_id : 'a Type_equal.Id.t
        }
        -> ('a, 'model, 'action, 'result, 'incr, 'event) unpacked
    | Var :
        { type_id : 'a Type_equal.Id.t }
        -> (unit, unit, Nothing.t, 'a, 'incr, 'event) unpacked

  let eval (type i m a r incr event) : (i, m, a, r, incr, event) eval_type =
    fun ~input ~old_model ~model ~inject ~action_type_id ~environment ~incr_state t ->
    match t with
    | Abstraction { t; type_id } ->
      let return = Incremental.return incr_state in
      let environment = Environment.add_exn environment type_id input in
      eval_ext
        ~input:(return ())
        ~old_model
        ~model
        ~inject
        ~action_type_id
        ~environment
        ~incr_state
        t
    | Var { type_id } ->
      let%map read = Environment.find_exn environment type_id in
      Snapshot.create ~result:read ~apply_action:(fun ~schedule_event:_ ->
        Nothing.unreachable_code)
    | _ -> assert false
  ;;

  let abstraction (Packed.T { unpacked; model; action_type_id }) ~type_id =
    Packed.T { unpacked = Abstraction { t = unpacked; type_id }; model; action_type_id }
  ;;

  let var (type a) (type_id : a Type_equal.Id.t) : (_, a, 'incr, 'event) Packed.t =
    Packed.T
      { unpacked = Var { type_id }
      ; action_type_id = nothing_type_id
      ; model = Packed.unit_model_info
      }
  ;;

  let sexp_of_unpacked (type i m a r) (component : (i, m, a, r, _, _) unpacked) =
    match component with
    | Abstraction { t; type_id = _ } -> [%sexp Proc_abstraction (t : unpacked)]
    | Var { type_id = _ } -> [%sexp Proc_var]
    | _ -> assert false
  ;;

  let visit
        (type i r incr event)
        (T { unpacked; action_type_id; model } as packed : (i, r, incr, event) Packed.t)
        visitor
    =
    match unpacked with
    | Abstraction { t; type_id } ->
      let visited =
        visit_ext (Packed.T { unpacked = t; action_type_id; model }) visitor
      in
      (visitor.visit (abstraction visited ~type_id) : (i, _, _, _) Packed.t)
    | Var _ -> visitor.visit packed
    | _ -> assert false
  ;;
end

include T

let () =
  Component.define
    (module struct
      include T

      let extension_constructor = [%extension_constructor Abstraction]
    end)
;;

let () =
  Component.define
    (module struct
      include T

      let extension_constructor = [%extension_constructor Var]
    end)
;;

module Val = struct
  type 'a t =
    | Constant : 'a -> 'a t
    | Named : 'a Type_equal.Id.t -> 'a t
    | Map :
        { t : 'a t
        ; f : 'a -> 'b
        }
        -> 'b t
    | Map2 :
        { a : 'a t
        ; b : 'b t
        ; f : 'a -> 'b -> 'c
        }
        -> 'c t
    | Both :
        { a : 'a t
        ; b : 'b t
        }
        -> ('a * 'b) t

  module T = Applicative.Make_using_map2 (struct
      type nonrec 'a t = 'a t

      let map = `Custom (fun t ~f -> Map { t; f })
      let map2 a b ~f = Map2 { a; b; f }
      let return a = Constant a
    end)

  include T

  module Open_on_rhs_intf = struct
    module type S = sig end
  end

  module Let_syntax = struct
    include T

    module Let_syntax = struct
      include T

      let both a b = Both { a; b }

      module Open_on_rhs = struct end
    end
  end
end

module Computation = struct
  type ('a, 'incr, 'event) t = (unit, 'a, 'incr, 'event) Packed.t
end

let proc f =
  let type_id = Type_equal.Id.create ~name:"some variable" sexp_of_opaque in
  abstraction ~type_id (f (Val.Named type_id))
;;

let subst c ~f = Compose.compose c (proc f)

let rec return : type a. a Val.t -> (a, _, _) Computation.t = function
  | Val.Constant a -> Const.const a
  | Val.Named type_id -> var type_id
  | Val.Map { t; f } -> Mapn.map (return t) ~f
  | Val.Map2 { a; b; f } -> Mapn.map2 (return a) (return b) ~f
  | Val.Both { a; b } -> Mapn.both (return a) (return b)
;;

let apply c v = Compose.compose (return v) c
let apply_unit c = Compose.compose (Const.const ()) c
let ignore_input = Map_input.map_input ~f:Fn.ignore

let if_ cond ~then_ ~else_ =
  Compose.compose
    (return cond)
    (Switch.if_ Fn.id ~then_:(ignore_input then_) ~else_:(ignore_input else_))
;;

let enum mdl ~match_ ~with_ =
  Compose.compose
    (return match_)
    (Switch.enum mdl ~which:Fn.id ~handle:(fun a -> ignore_input (with_ a)))
;;
