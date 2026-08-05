open! Core
open! Bonsai

(** [effect_throttle input ~effect_] performs [effect_ value] every time the value is
    changed, but throttles the effects to be dispatched at most [wait] after they are
    resolved. The effect is performed on both the leading and trailing edge.

    Common use cases include:
    - Search inputs: Trigger API calls as users type, without overwhelming your backend
      with a request per keystroke.
    - Form validations: Run expensive validations on field changes while maintaining UI
      responsiveness.
    - Real-time filtering: Update filtered results for large datasets without re-filtering
      on every input change.
    - API rate limiting: Ensure your application respects API rate limits by controlling
      the cadence of requests. *)
val effect_throttle
  :  'input Bonsai.t
  -> equal:('input -> 'input -> bool)
  -> wait:Time_ns.Span.t
  -> effect_:('input -> 'output Effect.t) Bonsai.t
  -> Bonsai.graph
  -> 'output option Bonsai.t

(** Immediately updates the result when the value changes initially, then throttles
    subsequent updates according to the [wait] interval.

    Common use cases include:
    - Polling state rpc query rate-limiting: PSRPC eagerly polls when the query changes,
      so query throttling can reduce wasted server CPU. *)
val value_throttle
  :  wait:Time_ns.Span.t
  -> equal:('a -> 'a -> bool)
  -> 'a Bonsai.t
  -> Bonsai.graph
  -> 'a Bonsai.t
