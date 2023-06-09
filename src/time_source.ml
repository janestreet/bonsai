open! Core
open! Import

type t =
  { incr : Incr.Clock.t
  ; timing_wheel : (unit, unit) Ui_effect.Private.Callback.t Timing_wheel.t
  ; mutable advance_to : Time_ns.t option
  }

let create ~start =
  let config = Incr.Clock.default_timing_wheel_config in
  let incr = Incr.Clock.create ~timing_wheel_config:config ~start () in
  let timing_wheel = Timing_wheel.create ~config ~start in
  { incr; timing_wheel; advance_to = None }
;;

let incr_clock t = t.incr

let now t =
  match t.advance_to with
  | Some to_ -> to_
  | None -> Incr.Clock.now t.incr
;;

let at_intervals t span = Incr.Clock.at_intervals t.incr span
let watch_now t = Incr.Clock.watch_now t.incr
let at t at = Incr.Clock.at t.incr at

let advance_clock t ~to_ =
  assert (Time_ns.( >= ) to_ (now t));
  t.advance_to <- Some to_
;;

let advance_clock_by t span = advance_clock t ~to_:(Time_ns.add (now t) span)

let until t at =
  Effect.Private.make ~request:() ~evaluator:(fun callback ->
    let (_ : _ Timing_wheel.Alarm.t) = Timing_wheel.add t.timing_wheel ~at callback in
    Effect.Ignore)
;;

let sleep t span = until t (Time_ns.add (now t) span)

module Private = struct
  let flush t =
    let handle_fired callback =
      Effect.Expert.handle
        (Effect.Private.Callback.respond_to
           (Timing_wheel.Alarm.value t.timing_wheel callback)
           ())
    in
    match t.advance_to with
    | Some to_ ->
      t.advance_to <- None;
      Timing_wheel.advance_clock t.timing_wheel ~to_ ~handle_fired;
      Timing_wheel.fire_past_alarms t.timing_wheel ~handle_fired;
      Incr.Clock.advance_clock t.incr ~to_
    | None -> Timing_wheel.fire_past_alarms t.timing_wheel ~handle_fired
  ;;
end
