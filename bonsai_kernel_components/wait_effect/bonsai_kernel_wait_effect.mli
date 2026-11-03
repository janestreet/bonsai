open! Core

(** [upon condition graph] returns an effect that resolves whenever [condition] is [true].

    If the condition is already true at the time the effect is scheduled, it will resolve
    immediately. Otherwise it will resolve the next time the condition becomes true. *)
val upon : bool Bonsai.t -> Bonsai.graph @ local -> unit Bonsai.Effect.t Bonsai.t
