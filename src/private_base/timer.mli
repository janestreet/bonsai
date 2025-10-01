open! Core

type 'timeable_event t = { time : 'a. 'timeable_event -> f:(unit -> 'a) -> 'a }

val create : start_timer:('a -> 'timer) -> stop_timer:('timer -> unit) -> 'a t

type event =
  [ `Graph_application
  | `Preprocess
  | `Gather
  ]
[@@deriving to_string]

type event_timer = event t

(** A timer to be used throughout Bonsai's internals. Since it needs to be used in
    user-facing functions (e.g. Bonsai.delay/Bonsai.handle_for_lazy), it can't be passed
    around as an argument, and since it depends on the instrumentation config's timer
    functions, it can't just be a constant. We store it instead in mutable state, with the
    expectation that this state is only ever set once, right before graph application. *)
val timer : unit -> event_timer

val set_timer : timer:event_timer -> unit
