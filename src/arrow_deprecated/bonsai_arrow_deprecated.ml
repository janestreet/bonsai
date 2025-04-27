open! Core
open! Import
module Proc = Bonsai_proc

module type S = Module_types.Component_s

type ('i, 'r) t = 'i Proc.Value.t -> 'r Proc.Computation.t

let const x _ = Proc.const x
let input = Proc.read
let pure ~f i = Proc.read (Proc.Value.map i ~f)
let compose a b i = Proc.Let_syntax.Let_syntax.sub (a i) ~f:b

let map a ~f i =
  Proc.Let_syntax.Let_syntax.sub (a i) ~f:(fun x -> Proc.read (Proc.Value.map ~f x))
;;

let map_input a ~f i = a (Proc.Value.map i ~f)
let of_module = Proc.of_module_with_input

let state_machine
  ~sexp_of_action
  ?sexp_of_model
  ~equal
  _here
  ~default_model
  ~apply_action
  input
  =
  Proc.state_machine_with_input
    ~sexp_of_action
    ?sexp_of_model
    ~equal
    ~default_model
    ~apply_action:(fun context input model action ->
      match input with
      | Active input -> apply_action context input model action
      | Inactive ->
        let action = sexp_of_action action in
        eprint_s
          [%message
            [%here]
              "An action sent to a [state_machine1] has been dropped because its input \
               was not present. This happens when the [state_machine1] is inactive when \
               it receives a message."
              (action : Sexp.t)];
        model)
    input
;;

let both ~(here : [%call_pos]) a b i =
  let open Proc.Let_syntax in
  let%sub a = a i in
  let%sub b = b i in
  return (Proc.Value.both a b)
;;

let enum ~(here : [%call_pos]) m ~which ~handle input =
  let match_ = Proc.Value.map ~here input ~f:which in
  let with_ key = handle key input in
  Proc.enum m ~match_ ~with_
;;

let if_ ~(here : [%call_pos]) choose ~then_ ~else_ input =
  let open Proc.Let_syntax in
  let cond = Proc.Value.map input ~f:choose in
  if%sub cond then then_ input else else_ input
;;

module Map = struct
  let assoc_input comparator f input = Proc.assoc comparator input ~f:(fun _ -> f)

  let associ_input comparator f input =
    Proc.assoc comparator input ~f:(fun key data -> f (Proc.Value.both key data))
  ;;

  let associ_input_with_extra comparator f input =
    let open Proc.Let_syntax in
    let%pattern_bind input, extra = input in
    Proc.assoc comparator input ~f:(fun key data ->
      f (Tuple3.create <$> key <*> data <*> extra))
  ;;
end

include struct
  open Proc.Let_syntax

  let arr f = pure ~f
  let ( >>^ ) a f = map a ~f
  let ( ^>> ) a f = map_input a ~f

  let first ~(here : [%call_pos]) f i =
    let%pattern_bind fst, snd = i in
    let%sub out = f fst in
    return (Proc.Value.both ~here out snd)
  ;;

  let second ~(here : [%call_pos]) f i =
    let%pattern_bind fst, snd = i in
    let%sub out = f snd in
    return (Proc.Value.both ~here fst out)
  ;;

  let split ~(here : [%call_pos]) f1 f2 i =
    let%pattern_bind fst, snd = i in
    let%sub out1 = f1 fst in
    let%sub out2 = f2 snd in
    return (Proc.Value.both ~here out1 out2)
  ;;

  let extend_first ~(here : [%call_pos]) f i =
    let%sub out = f i in
    return (Proc.Value.both ~here out i)
  ;;

  let extend_second ~(here : [%call_pos]) f i =
    let%sub out = f i in
    return (Proc.Value.both ~here i out)
  ;;

  let fanout ~(here : [%call_pos]) f1 f2 i =
    let%sub out1 = f1 i in
    let%sub out2 = f2 i in
    return (Proc.Value.both out1 out2)
  ;;

  let partial_compose_first ~(here : [%call_pos]) f1 f2 i =
    let%sub out1 = f1 i in
    let%pattern_bind shared, out1 = out1 in
    let%sub out2 = f2 (Proc.Value.both i shared) in
    return (Proc.Value.both out1 out2)
  ;;

  let pipe ~(here : [%call_pos]) f1 ~into ~via ~finalize i =
    let%sub r1 = f1 i in
    let intermediate = via <$> i <*> r1 in
    let%sub r2 = into intermediate in
    return (finalize <$> i <*> r1 <*> r2)
  ;;

  let ( *** ) = split
  let ( &&& ) = fanout
end

module With_incr = struct
  let of_incr : 'a Incr.t -> (_, 'a) t = fun i _ -> Proc.read (Proc.Incr.to_value i)

  open Proc.Let_syntax

  let of_module
    (type i m a r)
    ~(here : [%call_pos])
    ?sexp_of_model
    (component : (i, m, a, r) component_s_incr)
    ~equal
    ~default_model
    input
    : r Proc.Computation.t
    =
    let (module M) = component in
    let%sub state =
      Proc.state_machine_with_input
        ~here
        ~sexp_of_action:M.Action.sexp_of_t
        ?sexp_of_model
        ~equal
        ~default_model
        ~apply_action:(fun ctx input model action ->
          match input with
          | Active input ->
            M.apply_action
              input
              ~inject:(Proc.Apply_action_context.inject ctx)
              ~schedule_event:(Proc.Apply_action_context.schedule_event ctx)
              model
              action
          | Inactive ->
            eprint_s
              [%message
                [%here]
                  "An action sent to an [of_module] has been dropped because its input \
                   was not present. This happens when the [of_module] is inactive when \
                   it receives a message."
                  (action : M.Action.t)];
            model)
        input
    in
    Proc.Incr.compute ~here (Proc.Value.both ~here input state) ~f:(fun input_and_state ->
      let%pattern_bind.Ui_incr input, (model, inject) = input_and_state in
      M.compute input model ~inject)
  ;;

  let pure ~f x = Proc.Incr.compute ~f x
  let map a ~f = compose a (pure ~f)

  let value_cutoff ~cutoff =
    map input ~f:(fun input ->
      let input = Incr.map input ~f:Fn.id in
      Incr.set_cutoff input cutoff;
      input)
  ;;
end

module Infix = struct
  let ( >>> ) = compose
  let ( >>| ) a f = map a ~f
  let ( @>> ) f a = map_input a ~f
end

module Let_syntax = struct
  let return = const
  let map = map
  let both = both

  include Infix

  module Let_syntax = struct
    let return = const
    let both = both
    let map = map

    module Open_on_rhs = Infix
  end
end
