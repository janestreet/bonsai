open! Core
open! Import
module Q = Base_quickcheck

module Witness = struct
  type 'a t =
    | Unit : unit t
    | Int : int t
end

module type Quickcheckable = sig
  type 'a t

  type packed =
    | T :
        { unpacked : 'a t
        ; witness : 'a Witness.t
        }
        -> packed

  val quickcheck_generator : 'a Witness.t -> 'a t Q.Generator.t
  val packed_quickcheck_shrinker : packed Q.Shrinker.t
  val quickcheck_observer : 'a Witness.t -> 'a t Q.Observer.t
end

module type Packed_quickcheckable = sig
  type packed

  val quickcheck_generator : packed Q.Generator.t Lazy.t
  val quickcheck_observer : packed Q.Observer.t Lazy.t
end

module With_witness (M : Quickcheckable) :
  Packed_quickcheckable with type packed := M.packed = struct
  let quickcheck_generator =
    let open Q.Generator.Let_syntax in
    lazy
      (Q.Generator.of_lazy
         (lazy
           (Q.Generator.weighted_union
              [ ( 1.
                , let%map unpacked = M.quickcheck_generator Unit in
                  M.T { unpacked; witness = Unit } )
              ; ( 1.
                , let%map unpacked = M.quickcheck_generator Int in
                  M.T { unpacked; witness = Int } )
              ])))
  ;;

  let quickcheck_observer =
    lazy
      (Q.Observer.of_lazy
         (lazy
           (Q.Observer.create (fun (M.T { unpacked; witness }) ~size ~hash ->
              let observe (type w) (unpacked : w M.t) (witness : w Witness.t) =
                let hash =
                  match witness with
                  | Unit -> hash_fold_int hash 0
                  | Int -> hash_fold_int hash 1
                in
                Q.Observer.observe (M.quickcheck_observer witness) unpacked ~size ~hash
              in
              observe unpacked witness))))
  ;;
end

module Data = struct
  module T = struct
    type 'a t =
      | Unit : unit -> unit t
      | Int : int -> int t

    type packed =
      | T :
          { unpacked : 'a t
          ; witness : 'a Witness.t
          }
          -> packed

    let quickcheck_generator (type w) (witness : w Witness.t) : w t Q.Generator.t =
      let open Q.Generator.Let_syntax in
      match witness with
      | Unit ->
        let%map u = quickcheck_generator_unit in
        Unit u
      | Int ->
        let%map i = quickcheck_generator_int in
        Int i
    ;;

    let quickcheck_observer (type w) (witness : w Witness.t) : w t Q.Observer.t =
      match witness with
      | Unit -> Q.Observer.unmap quickcheck_observer_unit ~f:(fun (Unit u) -> u)
      | Int -> Q.Observer.unmap quickcheck_observer_int ~f:(fun (Int i) -> i)
    ;;

    let packed_quickcheck_shrinker : packed Q.Shrinker.t =
      Q.Shrinker.create (fun (T { unpacked; witness = _ }) ->
        let shrink (type w) (unpacked : w t) : packed Sequence.t =
          match unpacked with
          | Unit u ->
            Sequence.map
              (Q.Shrinker.shrink
                 (Q.Shrinker.map
                    quickcheck_shrinker_unit
                    ~f:(fun u -> Unit u)
                    ~f_inverse:(fun (Unit u) -> u))
                 (Unit u))
              ~f:(fun unpacked -> T { unpacked; witness = Unit })
          | Int i ->
            Sequence.map
              (Q.Shrinker.shrink
                 (Q.Shrinker.map
                    quickcheck_shrinker_int
                    ~f:(fun i -> Int i)
                    ~f_inverse:(fun (Int i) -> i))
                 (Int i))
              ~f:(fun unpacked -> T { unpacked; witness = Int })
        in
        shrink unpacked)
    ;;
  end

  [@@@warning "-60"]

  module Packed = With_witness (T)
  include T
end

module Value = struct
  module rec T : sig
    type 'a t =
      | Return : 'a Data.t -> 'a t
      | Map : 'a t * 'a Witness.t * ('a Data.t -> 'b Data.t) -> 'b t

    type packed =
      | T :
          { unpacked : 'a t
          ; witness : 'a Witness.t
          }
          -> packed

    include Quickcheckable with type 'a t := 'a t and type packed := packed
  end = struct
    type 'a t =
      | Return : 'a Data.t -> 'a t
      | Map : 'a t * 'a Witness.t * ('a Data.t -> 'b Data.t) -> 'b t

    type packed =
      | T :
          { unpacked : 'a t
          ; witness : 'a Witness.t
          }
          -> packed

    let quickcheck_generator witness =
      let open Q.Generator.Let_syntax in
      Q.Generator.weighted_union
        [ ( 1.
          , let%map data = Data.quickcheck_generator witness in
            Return data )
        ; ( 1.
          , let%bind (T inner) = Lazy.force Packed.quickcheck_generator in
            let%map f =
              Q.Generator.fn
                (Data.quickcheck_observer inner.witness)
                (Data.quickcheck_generator witness)
            in
            Map (inner.unpacked, inner.witness, f) )
        ]
    ;;

    let rec quickcheck_observer : type w. w Witness.t -> w t Q.Observer.t =
      fun witness ->
        Q.Observer.create (fun t ~size ~hash ->
          match t with
          | Return data ->
            let hash = hash_fold_int hash 0 in
            Q.Observer.observe (Data.quickcheck_observer witness) data ~size ~hash
          | Map (inner, inner_witness, f) ->
            let hash = hash_fold_int hash 1 in
            let hash =
              Q.Observer.observe (quickcheck_observer inner_witness) inner ~size ~hash
            in
            let fn_observer =
              Q.Observer.fn
                (Data.quickcheck_generator inner_witness)
                (Data.quickcheck_observer witness)
            in
            Q.Observer.observe fn_observer f ~size ~hash)
    ;;

    let packed_quickcheck_shrinker =
      Q.Shrinker.create (fun (T { unpacked; witness }) ->
        match unpacked with
        | Return data ->
          Sequence.round_robin
            [ Sequence.map
                (Q.Shrinker.shrink
                   Data.packed_quickcheck_shrinker
                   (Data.T { unpacked = data; witness }))
                ~f:(fun (T { unpacked; witness }) ->
                  T { unpacked = Return unpacked; witness })
            ]
        | Map (inner, inner_witness, _f) ->
          Sequence.singleton (T { unpacked = inner; witness = inner_witness }))
    ;;
  end

  and Packed : (Packed_quickcheckable with type packed := T.packed) = With_witness (T)

  include T
end

module Computation = struct
  module T = struct
    type 'a t = Return : 'a Value.t -> 'a t

    type packed =
      | T :
          { unpacked : 'a t
          ; witness : 'a Witness.t
          }
          -> packed

    let quickcheck_generator witness =
      let open Q.Generator.Let_syntax in
      let%map data = Value.quickcheck_generator witness in
      Return data
    ;;

    let quickcheck_observer witness =
      Q.Observer.create (fun t ~size ~hash ->
        match t with
        | Return data ->
          Q.Observer.observe (Value.quickcheck_observer witness) data ~size ~hash)
    ;;

    let packed_quickcheck_shrinker =
      Q.Shrinker.create (fun (T { unpacked; witness }) ->
        match unpacked with
        | Return data ->
          Sequence.round_robin
            [ Sequence.map
                (Q.Shrinker.shrink
                   Value.packed_quickcheck_shrinker
                   (Value.T { unpacked = data; witness }))
                ~f:(fun (T { unpacked; witness }) ->
                  T { unpacked = Return unpacked; witness })
            ])
    ;;
  end

  module Packed = With_witness (T)
  include T
end

let to_real_data (type w) (unpacked : w Data.t) : w =
  match unpacked with
  | Unit u -> u
  | Int i -> i
;;

let of_real_data (type w) (witness : w Witness.t) (data : w) : w Data.t =
  match witness with
  | Unit -> Unit data
  | Int -> Int data
;;

let rec to_real_value : type w. w Value.t -> w Bonsai.Value.t =
  fun unpacked ->
  match unpacked with
  | Return data -> Bonsai.Value.return (to_real_data data)
  | Map (inner, inner_witness, f) ->
    Bonsai.Value.map (to_real_value inner) ~f:(fun x ->
      to_real_data (f (of_real_data inner_witness x)))
;;

let to_real_computation (type w) (unpacked : w Computation.t) : w Bonsai.Computation.t =
  match unpacked with
  | Return unpacked -> Bonsai.read (to_real_value unpacked)
;;

module Packed_real_computation = struct
  type t =
    | T :
        { unpacked : 'a Bonsai.Computation.t
        ; witness : 'a Witness.t
        }
        -> t
end

let to_packed_real_computation (T { unpacked; witness } : Computation.packed) =
  Packed_real_computation.T { unpacked = to_real_computation unpacked; witness }
;;

open Proc

let witness_to_result_spec : type w. w Witness.t -> (w, Nothing.t) Result_spec.t =
  fun witness ->
  Result_spec.sexp
    (module struct
      type t = w

      let sexp_of_t : w -> Sexp.t =
        match witness with
        | Int -> sexp_of_int
        | Unit -> sexp_of_unit
      ;;
    end)
;;

let%expect_test _ =
  let random = Splittable_random.State.create (Random.State.make [| Random.bits () |]) in
  let recompute_view_on_random_computation () =
    let (T { unpacked; witness }) =
      to_packed_real_computation
        (Q.Generator.generate
           (Lazy.force Computation.Packed.quickcheck_generator)
           ~size:10
           ~random)
    in
    let handle = Handle.create (witness_to_result_spec witness) unpacked in
    Handle.recompute_view handle
  in
  recompute_view_on_random_computation ();
  recompute_view_on_random_computation ();
  recompute_view_on_random_computation ();
  recompute_view_on_random_computation ()
;;
