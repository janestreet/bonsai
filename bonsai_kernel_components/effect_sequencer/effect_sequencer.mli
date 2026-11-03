open! Core
module Effect = Ui_effect

(** A sequencer for ensuring that the current effect completes before the next one starts. *)
type t

(** Makes a new sequencer. *)
val create : Bonsai.graph @ local -> t Bonsai.t

(** Produces an effect that first waits in line until all the effects in front of it are
    finished running, and then runs the provided effect.

    A couple points of emphasis:
    - Like the provided effect, using the resulting effect multiple times will do the
      side-effect multiple times (unlike [_ Deferred.t]).
    - The effect starts waiting in line _when the effect runs_, rather than when [run] is
      called. Consequently, using the effect multiple times means that each run of the
      effect will be waiting in line individually, rather than all of them sharing a spot
      in line.

    Based on these points, you can think of [run] as a function that returns the an effect
    that is almost exactly like the effect it was given, but "more atomic", in that it
    won't get interleaved with other effects sent through the same sequencer.

    WARNING: It's very easy to "deadlock" with this function. Just call [run] within the
    effect that you pass to [run], using the same sequencer for both. The result is that
    the returned function will never complete. (although, it's worth noting, the rest of
    your program won't be blocked from running). *)
val run : t -> this_effect_doesn't_call_run:'a Effect.t -> 'a Effect.t

module For_non_bonsai_users : sig
  val create : unit -> t
end
