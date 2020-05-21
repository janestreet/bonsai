open! Core_kernel
open! Import
include Bonsai_intf

module Generic = struct
  include Component
  include Packed

  let eval = eval_ext

  let sexp_of_t (T { unpacked; action_type_id = _; model = _ }) =
    sexp_of_unpacked unpacked
  ;;

  (* Constructor Functions *)

  let const = Const.const
  let pure = Pure.pure
  let leaf = Leaf.leaf

  module type Enum = Switch.Enum

  let enum = Switch.enum
  let if_ = Switch.if_

  (* Modifier Functions *)
  let compose = Compose.compose
  let map = Mapn.map
  let map2 = Mapn.map2
  let map_input = Map_input.map_input
  let return = const
  let both = Mapn.both
  let ( >>| ) t f = map t ~f

  module Infix = struct
    let ( >>> ) = compose
    let ( >>| ) = ( >>| )
    let ( @>> ) f t = map_input t ~f
  end

  module Proc = Proc
  open Infix

  let state_machine = Leaf.state_machine
  let input = Pure.input

  module List_deprecated = struct end

  module With_incr = struct
    let pure = Pure_incr.pure_incr
    let of_incr t = pure ~f:(Fn.const t)
    let leaf = Leaf_incr.leaf_incr
    let model_cutoff = Cutoff.model_cutoff
    let value_cutoff = Cutoff.value_cutoff
    let map = Map_incr.map_incr
    let map_input t ~f = compose (pure ~f) t
  end

  module Map = struct
    module type Comparator = Bonsai_types.Comparator

    type ('k, 'cmp) comparator = ('k, 'cmp) Bonsai_types.comparator

    let associ_input_with_extra = Assoc.associ_input

    let associ_input comparator t =
      (fun map -> map, ())
      @>> associ_input_with_extra comparator ((fun (k, i, _) -> k, i) @>> t)
    ;;

    let assoc_input comparator t =
      (fun map -> map, ())
      @>> associ_input_with_extra comparator ((fun (_, i, _) -> i) @>> t)
    ;;

    let merge a b ~f =
      both a b
      |> With_incr.map ~f:(fun pair ->
        let open Incremental.Let_syntax in
        let return = pair |> Incremental.state |> Incremental.return in
        let%pattern_bind a, b = pair in
        Incr_map.merge a b ~f)
    ;;
  end

  module Open_on_rhs_intf = struct
    module type S = sig end
  end

  module Let_syntax = struct
    let return = return
    let map = map
    let both = both

    include Infix

    module Let_syntax = struct
      let return = return
      let both = both
      let map = map
      let sub = Proc.sub

      module Open_on_rhs = Infix
    end
  end

  module Arrow = struct
    let arr f = pure ~f
    let first t = both (fst @>> t) (arr snd)
    let second t = both (arr fst) (snd @>> t)
    let split t u = both (fst @>> t) (snd @>> u)
    let fanout = both
    let extend_first component = pure ~f:(fun x -> x, x) >>> first component
    let extend_second component = pure ~f:(fun x -> x, x) >>> second component
    let ( *** ) = split
    let ( &&& ) = fanout
    let ( ^>> ) = ( @>> )
    let ( >>^ ) = ( >>| )

    let partial_compose_first a b =
      let rearrange ((shared, output1), input) = output1, (input, shared) in
      extend_first a >>> rearrange @>> second b
    ;;

    let pipe from ~into ~via ~finalize =
      let intermediate =
        let%map i, r1 = extend_second from in
        (i, r1), via i r1
      in
      let%map (i, r), r2 = intermediate >>> second into in
      finalize i r r2
    ;;
  end

  module Expert = struct
    module Snapshot = Snapshot

    type nonrec ('input, 'model, 'action, 'result, 'incr, 'event) unpacked =
      ('input, 'model, 'action, 'result, 'incr, 'event) unpacked

    include Packed

    let reveal = Fn.id
    let conceal = Fn.id
    let of_full = Full.of_full
    let eval = eval
    let optimize = Optimize.optimize
  end
end

module type S = sig
  module Incr : Incremental.S
  module Event : Event.S

  include
    S_gen
    with module Incr := Incr
    with module Event := Event
    with type 'a Proc.Computation.t =
           ('a, Incr.state_witness, Event.t) Generic.Proc.Computation.t

  val to_generic : ('i, 'r) t -> ('i, 'r, Incr.state_witness, Event.t) Generic.t
  val of_generic : ('i, 'r, Incr.state_witness, Event.t) Generic.t -> ('i, 'r) t
end

module Make (Incr : Incremental.S) (Event : Event.S) :
  S with module Incr = Incr and module Event = Event = struct
  module Incr = Incr
  module Event = Event
  include Generic

  type ('i, 'r) t = ('i, 'r, Incr.state_witness, Event.t) Generic.Packed.t

  module Proc = struct
    include Generic.Proc

    module Computation = struct
      type 'a t = ('a, Incr.state_witness, Event.t) Proc.Computation.t
    end
  end

  let to_generic = Fn.id
  let of_generic = Fn.id

  module type S = sig
    module Input : T
    module Model : Model
    module Action : Action
    module Result : T

    val apply_action
      :  inject:(Action.t -> Event.t)
      -> schedule_event:(Event.t -> unit)
      -> Input.t
      -> Model.t
      -> Action.t
      -> Model.t

    val compute : inject:(Action.t -> Event.t) -> Input.t -> Model.t -> Result.t
    val name : string
  end

  type ('input, 'model, 'action, 'result) component_s =
    (module S
      with type Input.t = 'input
       and type Model.t = 'model
       and type Action.t = 'action
       and type Result.t = 'result)

  module M (Component : S) = struct
    type nonrec t = (Component.Input.t, Component.Result.t) t
  end

  let of_module (type i m a r) m ~default_model =
    let module M = (val m : S
                    with type Input.t = i
                     and type Action.t = a
                     and type Model.t = m
                     and type Result.t = r)
    in
    leaf
      (module M.Model)
      (module M.Action)
      ~name:M.name
      ~default_model
      ~apply_action:M.apply_action
      ~compute:M.compute
  ;;

  include struct
    open Applicative.Make2_using_map2 (struct
        type nonrec ('r, 'i) t = ('i, 'r) t

        let map = `Custom map
        let map2 = map2
        let return = const
      end)

    module Applicative_infix = Applicative_infix

    let all = all
    let all_unit = all_unit
    let apply = apply
    let both = both
    let map3 = map3
    let return = return
    let ( <* ) = ( <* )
    let ( *> ) = ( *> )
    let ( <*> ) = ( <*> )
    let ( >>| ) = ( >>| )
  end

  module With_incr = struct
    include With_incr

    module type S = sig
      module Input : T
      module Model : Model
      module Action : Action
      module Result : T

      val apply_action
        :  Input.t Incr.t
        -> Model.t Incr.t
        -> inject:(Action.t -> Event.t)
        -> (schedule_event:(Event.t -> unit) -> Action.t -> Model.t) Incr.t

      val compute
        :  Input.t Incr.t
        -> Model.t Incr.t
        -> inject:(Action.t -> Event.t)
        -> Result.t Incr.t

      val name : string
    end

    type ('input, 'model, 'action, 'result) component_s =
      (module S
        with type Input.t = 'input
         and type Model.t = 'model
         and type Action.t = 'action
         and type Result.t = 'result)

    let of_module (type i m a r) m =
      let module M = (val m : S
                      with type Input.t = i
                       and type Action.t = a
                       and type Model.t = m
                       and type Result.t = r)
      in
      Leaf_incr.leaf_incr
        (module M.Model)
        (module M.Action)
        ~name:M.name
        ~apply_action:M.apply_action
        ~compute:M.compute
    ;;
  end
end

module Snapshot = Snapshot
