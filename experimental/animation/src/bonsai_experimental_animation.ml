open! Core
open Bonsai.Let_syntax
open Bonsai.For_open

module _ = struct
  type t = float * float [@@deriving sexp, equal]
end

module Callback = struct
  type t = unit Effect.t

  let sexp_of_t = sexp_of_opaque
  let t_of_sexp _ = failwith "stop using tangle"
  let equal = phys_equal
end

module Interpolator = struct
  type t =
    | Linear
    | Ease_in_quad
    | Ease_out_quad
    | Ease_in_out_quad
    | Ease_in_cubic
    | Ease_out_cubic
    | Ease_in_out_cubic
    | Ease_in_quart
    | Ease_out_quart
    | Ease_in_out_quart
    | Ease_in_quint
    | Ease_out_quint
    | Ease_in_out_quint
    | Ease_in_sin
    | Ease_out_sin
    | Ease_in_out_sin
    | Ease_in_exp
    | Ease_out_exp
    | Ease_in_out_exp
    | Ease_in_circ
    | Ease_out_circ
    | Ease_in_out_circ
    | Ease_in_back
    | Ease_out_back
    | Ease_in_out_back
  [@@deriving sexp, equal, enumerate, compare]

  let to_f =
    let open Float in
    function
    | Linear -> fun t -> t
    | Ease_in_quad -> fun t -> t * t
    | Ease_out_quad -> fun t -> t * (2.0 - t)
    | Ease_in_out_quad ->
      (function
        | t when t < 0.5 -> 2.0 * t * t
        | t -> -1.0 + ((4.0 - (2.0 * t)) * t))
    | Ease_in_cubic -> fun t -> t * t * t
    | Ease_out_cubic ->
      fun t ->
        let t = t - 1.0 in
        (-t * t * t) + 1.0
    | Ease_in_out_cubic ->
      (function
        | t when t < 0.5 -> 4.0 * t * t * t
        | t -> ((t - 1.0) * ((2.0 * t) - 2.0) * ((2.0 * t) - 2.0)) + 1.0)
    | Ease_in_quart -> fun t -> t * t * t * t
    | Ease_out_quart ->
      fun t ->
        let t = t - 1.0 in
        1.0 - (t * t * t * t)
    | Ease_in_out_quart ->
      (function
        | t when t < 0.5 -> 8.0 * t * t * t * t
        | t ->
          let t = t - 1.0 in
          1.0 - (8.0 * t * t * t * t))
    | Ease_in_quint -> fun t -> t * t * t * t * t
    | Ease_out_quint ->
      fun t ->
        let t = t - 1.0 in
        1.0 + (t * t * t * t * t)
    | Ease_in_out_quint ->
      (function
        | t when t < 0.5 -> 16.0 * t * t * t * t * t
        | t ->
          let t = t - 1.0 in
          1.0 + (16.0 * t * t * t * t * t))
    | Ease_in_sin -> fun t -> 1.0 - cos (t * pi / 2.0)
    | Ease_out_sin -> fun t -> sin (t * pi / 2.0)
    | Ease_in_out_sin -> fun t -> -(cos (pi * t) - 1.0) / 2.0
    | Ease_in_exp ->
      (function
        | 0.0 -> 0.0
        | t -> 2.0 ** ((10.0 * t) - 10.0))
    | Ease_out_exp ->
      (function
        | 1.0 -> 1.0
        | t -> 1.0 - (2.0 ** (-10.0 * t)))
    | Ease_in_out_exp ->
      (function
        | 0.0 -> 0.0
        | 1.0 -> 1.0
        | t when t < 0.5 -> (2.0 ** ((20.0 * t) - 10.0)) / 2.0
        | t -> 2.0 - ((2.0 ** ((-20.0 * t) + 10.0)) / 2.0))
    | Ease_in_circ -> fun t -> 1.0 - sqrt (1.0 - (t ** 2.0))
    | Ease_out_circ -> fun t -> sqrt (1.0 - ((t - 1.0) ** 2.0))
    | Ease_in_out_circ ->
      (function
        | t when t < 0.5 -> (1.0 - sqrt (1.0 - ((2.0 * t) ** 2.0))) / 2.0
        | t -> (sqrt (1.0 - (((-2.0 * t) + 2.0) ** 2.0)) + 1.0) / 2.0)
    | Ease_in_back ->
      let c1 = 1.70158 in
      let c3 = c1 + 1.0 in
      fun t -> (c3 * t * t * t) - (c1 * t * t)
    | Ease_out_back ->
      let c1 = 1.70158 in
      let c3 = c1 + 1.0 in
      fun t -> 1.0 + (c3 * ((t - 1.0) ** 3.0)) + (c1 * ((t - 1.0) ** 2.0))
    | Ease_in_out_back ->
      let c1 = 1.70158 in
      let c2 = c1 * 1.525 in
      (function
        | t when t < 0.5 -> ((2.0 * t) ** 2.0) * (((c2 + 1.0) * 2.0 * t) - c2) / 2.0
        | t ->
          (((((2.0 * t) - 2.0) ** 2.0) * (((c2 + 1.0) * ((t * 2.0) - 2.0)) + c2)) + 2.0)
          / 2.0)
  ;;
end

type 'a t =
  { value : 'a
  ; animate :
      ?after_finished:Callback.t
      -> ?with_:Interpolator.t
      -> [ `End_at of Time_ns.t | `For of Time_ns.Span.t | `Now ]
      -> 'a
      -> unit Effect.t
  }

let curtime = Effect.of_sync_fun (fun () -> Ui_incr.Clock.now Ui_incr.clock) ()

module Interpolatable = struct
  type 'a t = 'a -> 'a -> float -> 'a

  let float low high percent_float =
    (low *. (1.0 -. percent_float)) +. (high *. percent_float)
  ;;

  let int low high percent_float =
    float (Int.to_float low) (Int.to_float high) percent_float
    |> Float.round_nearest_half_to_even
    |> Float.to_int
  ;;
end

let make
  : type a.
    fallback:a Value.t -> interpolate:(a -> a -> float -> a) -> a t Computation.t
  =
  fun ~fallback ~interpolate ->
  let module A_star_a = struct
    type t = a * a

    let sexp_of_t = sexp_of_opaque
    let t_of_sexp _ = assert false
    let equal (a, b) (c, d) = phys_equal a c && phys_equal b d
  end
  in
  let%sub start_time, set_start = Bonsai.state_opt (module Time_ns.Alternate_sexp) in
  let%sub interpolator, set_interpolator =
    Bonsai.state (module Interpolator) ~default_model:Interpolator.Linear
  in
  let%sub end_time, set_end = Bonsai.state_opt (module Time_ns.Alternate_sexp) in
  let%sub callback, set_callback = Bonsai.state_opt (module Callback) in
  let%sub range, set_range = Bonsai.state_opt (module A_star_a) in
  let%sub percent_float =
    match%sub Value.both start_time end_time with
    | None, _ | _, None -> Bonsai.const 0.0
    | Some start_time, Some end_time ->
      let%sub before_or_after = Bonsai.Clock.at end_time in
      let%sub () =
        match%sub callback with
        | None -> Bonsai.const ()
        | Some callback ->
          let callback =
            let%map callback = callback
            and set_callback = set_callback in
            fun prev new_ ->
              let remove_callback = set_callback None in
              match prev, new_ with
              | ( Some Bonsai.Clock.Before_or_after.Before
                , Bonsai.Clock.Before_or_after.After ) ->
                Effect.Many [ remove_callback; callback ]
              | _ -> Effect.Ignore
          in
          Bonsai.Edge.on_change'
            (module Bonsai.Clock.Before_or_after)
            before_or_after
            ~callback
      in
      (match%sub before_or_after with
       | After -> Bonsai.const 1.0
       | Before ->
         let%sub cur_time = Bonsai.Clock.now in
         let%arr start_time = start_time
         and end_time = end_time
         and cur_time = cur_time in
         let range_delta = Time_ns.abs_diff end_time start_time in
         let cur_delta = Time_ns.abs_diff cur_time start_time in
         Time_ns.Span.to_ms cur_delta /. Time_ns.Span.to_ms range_delta)
  in
  let interpolator = Value.map interpolator ~f:Interpolator.to_f in
  let%sub value =
    let%arr fallback = fallback
    and percent_float = percent_float
    and interpolator = interpolator
    and range = range in
    let percent_float = interpolator percent_float in
    match range with
    | None -> fallback
    | Some (low, high) -> interpolate low high percent_float
  in
  let%sub get_value = Bonsai.yoink value in
  let%sub animate =
    let%arr set_start = set_start
    and set_end = set_end
    and set_callback = set_callback
    and set_interpolator = set_interpolator
    and set_range = set_range
    and get_value = get_value in
    fun ?after_finished ?with_ time target ->
      let%bind.Effect now = curtime in
      let%bind.Effect value =
        match%bind.Effect get_value with
        | Active value -> Effect.return value
        | Inactive ->
          Effect.never
      in
      let target_time =
        match time with
        | `End_at time -> time
        | `For delta -> Time_ns.add now delta
        | `Now -> now
      in
      let effects =
        [ set_start (Some now)
        ; set_end (Some target_time)
        ; set_range (Some (value, target))
        ]
      in
      let effects =
        match after_finished with
        | None -> effects
        | Some callback -> set_callback (Some callback) :: effects
      in
      let effects =
        match with_ with
        | None -> effects
        | Some interpolator -> set_interpolator interpolator :: effects
      in
      Effect.Many effects
  in
  let%arr value = value
  and animate = animate in
  { value; animate }
;;

let smooth m ?(with_ = Interpolator.Linear) ~duration ~interpolate v =
  let%sub { value; animate } = make ~fallback:v ~interpolate in
  let%sub () =
    let callback =
      let%map animate = animate
      and duration = duration in
      fun new_ -> animate (`For duration) ~with_ new_
    in
    Bonsai.Edge.on_change m v ~callback
  in
  return value
;;

module Advanced = struct
  type nonrec 'a t = 'a t =
    { value : 'a
    ; animate :
        ?after_finished:Callback.t
        -> ?with_:Interpolator.t
        -> [ `End_at of Time_ns.t | `For of Time_ns.Span.t | `Now ]
        -> 'a
        -> unit Effect.t
    }

  let make = make
end
