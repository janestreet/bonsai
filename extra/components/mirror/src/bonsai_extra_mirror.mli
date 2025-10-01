open! Core
module Bonsai := Bonsai.Cont
module Effect := Bonsai.Effect

(** [mirror] is used to reflect state back and forth between locations. Frequently this
    will be used to back up a components model in a more persistent form of storage, such
    as the URL, or local-storage.

    The gist of this combinator is that if you have two states that you'd like to be
    synchronized, you can feed the "current value" and "set value" functions for both
    states into [mirror] and they'll automatically be kept up to date. Either of these can
    be backed by any kind of structure, but there are some important differences in their
    symmetry.

    When the component is first loaded, [store] has priority, so if the values are
    different, [store] wins, and [interactive] has its value "set". From that point on, if
    either incoming value changes, the opposite setter is called. In the case that both
    [store] and [interactive] change at the same time, the tie is broken in favor of
    [interactive], and [store_set] is called. *)
val mirror
  :  ?sexp_of_model:('m -> Sexp.t)
  -> equal:('m -> 'm -> bool)
  -> store_set:('m -> unit Effect.t) Bonsai.t
  -> store_value:'m Bonsai.t
  -> interactive_set:('m -> unit Effect.t) Bonsai.t
  -> interactive_value:'m Bonsai.t
  -> local_ Bonsai.graph
  -> unit

(** [mirror'] is like [mirror], but the incoming values have the type ['a option Bonsai.t]
    instead of just ['a Bonsai.t]. When a value is changed and its new value is None, we
    don't propagate it to the other setter. *)
val mirror'
  :  ?sexp_of_model:('m -> Sexp.t)
  -> equal:('m -> 'm -> bool)
  -> store_set:('m -> unit Effect.t) Bonsai.t
  -> store_value:'m option Bonsai.t
  -> interactive_set:('m -> unit Effect.t) Bonsai.t
  -> interactive_value:'m option Bonsai.t
  -> local_ Bonsai.graph
  -> unit
