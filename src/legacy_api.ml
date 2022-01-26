open! Core
open! Import

module type S = Module_types.Component_s

type ('i, 'r) t = 'i Proc.Value.t -> 'r Proc.Computation.t

module type Model = Model
module type Action = Action

let const x _ = Proc.const x
let input = Proc.read
let pure ~f i = Proc.read (Proc.Value.map i ~f)
let compose a b i = Proc.Let_syntax.Let_syntax.sub (a i) ~f:b

let map a ~f i =
  Proc.Let_syntax.Let_syntax.sub (a i) ~f:(fun x -> Proc.read (Proc.Value.map ~f x))
;;

let map_input a ~f i = a (Proc.Value.map i ~f)
let of_module = Proc.of_module1
let state_machine model action here = Proc.state_machine1 here model action

let both a b i =
  let open Proc.Let_syntax in
  let%sub a = a i in
  let%sub b = b i in
  return (Proc.Value.both a b)
;;

let enum m ~which ~handle input =
  let match_ = Proc.Value.map input ~f:which in
  let with_ key = handle key input in
  Proc.enum m ~match_ ~with_
;;

let if_ choose ~then_ ~else_ input =
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

  let first f i =
    let%pattern_bind fst, snd = i in
    let%sub out = f fst in
    return (Proc.Value.both out snd)
  ;;

  let second f i =
    let%pattern_bind fst, snd = i in
    let%sub out = f snd in
    return (Proc.Value.both fst out)
  ;;

  let split f1 f2 i =
    let%pattern_bind fst, snd = i in
    let%sub out1 = f1 fst in
    let%sub out2 = f2 snd in
    return (Proc.Value.both out1 out2)
  ;;

  let extend_first f i =
    let%sub out = f i in
    return (Proc.Value.both out i)
  ;;

  let extend_second f i =
    let%sub out = f i in
    return (Proc.Value.both i out)
  ;;

  let fanout f1 f2 i =
    let%sub out1 = f1 i in
    let%sub out2 = f2 i in
    return (Proc.Value.both out1 out2)
  ;;

  let partial_compose_first f1 f2 i =
    let%sub out1 = f1 i in
    let%pattern_bind shared, out1 = out1 in
    let%sub out2 = f2 (Proc.Value.both i shared) in
    return (Proc.Value.both out1 out2)
  ;;

  let pipe f1 ~into ~via ~finalize i =
    let%sub r1 = f1 i in
    let intermediate = via <$> i <*> r1 in
    let%sub r2 = into intermediate in
    return (finalize <$> i <*> r1 <*> r2)
  ;;

  let ( *** ) = split
  let ( &&& ) = fanout
end

module With_incr = struct
  let of_incr i _ = Proc.read (Proc.Private.conceal_value (Value.of_incr i))

  let of_module
        (type i m a r)
        (component : (i, m, a, r) component_s_incr)
        ~default_model
        input
    : r Proc.Computation.t
    =
    let input = Proc.Private.reveal_value input in
    let (module M) = component in
    let t =
      Computation.Leaf_incr
        { input
        ; dynamic_apply_action = M.apply_action
        ; compute = (fun _ -> M.compute)
        ; name = M.name
        }
    in
    Computation.T
      { t
      ; model = Meta.Model.of_module (module M.Model) ~name:M.name ~default:default_model
      ; dynamic_action = Meta.Action.of_module (module M.Action) ~name:M.name
      ; static_action = Meta.Action.nothing
      ; apply_static = Proc.unusable_static_apply_action
      }
    |> Proc.Private.conceal_computation
  ;;

  let pure (type i r) ~f =
    of_module
      (module struct
        module Input = struct
          type t = i
        end

        module Result = struct
          type t = r
        end

        module Model = Unit
        module Action = Nothing

        let name = "pure"
        let apply_action _ ~inject:_ = Incr.return (fun ~schedule_event:_ _ _ -> ())
        let compute input _ ~inject:_ = f input
      end)
      ~default_model:()
  ;;

  let map a ~f = compose a (pure ~f)
  let model_cutoff f a = Proc.Incr.model_cutoff (f a)

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
