open! Core
open! Import

module type Up = sig
  (** Values of type [Up.t] are passed from child to parent during a transformation. For
      transformations that use the fallback "skipping-over" recursion, the default
      transformer needs to know how to allocate empty [Up.t] values, and combine [Up.t]
      for computations that have more than one child. *)

  type t

  val combine : t -> t -> t
  val empty : t

  (** [Computation.lazy] is an obstacle to optimization in many cases. To force
      optimization authors to think about [lazy], we have a special empty value.

      Optimizations get applied to [Computation.lazy] when it is forced, rather than
      immediately. Thus, we use a constant, [empty_for_lazy], as the value that is passed
      back to the parent computation. *)
  val empty_for_lazy : t
end

module type Types = sig
  (** Values of type [Down.t] are passed from parent to child during a transformation. *)
  module Down : T

  (** Values of type [Acc.t] are threaded through the computational transformation in a
      depth-first manner. *)
  module Acc : T

  module Up : Up
end

module Unit : Up with type t = unit = struct
  type t = unit

  let combine () () = ()
  let empty = ()
  let empty_for_lazy = ()
end

module type Recurse = sig
  (** A module with signature [Recurse] is passed to the transformation implementation.
      Use of values in this module is necessary for the transformation to recurse. The
      main value that this API provides is that it allows transformation authors to skip
      computation constructors that they can't do anything for, while still applying their
      transformation to children of that node. *)

  module Types : Types

  (** [on_computation] is the function that a user of the transform API must call to
      recursively transform a computation. The main feature of it is the ability for the
      caller to skip applying their transformation to the provided node, while still
      applying it to the children of that node. This behavior is produced by providing
      [`Skipping_over] to the function, while [`Directly_on] will run the users
      transformation on the node. *)
  val on_computation
    :  Types.Down.t
    -> Types.Acc.t
    -> [ `Directly_on | `Skipping_over ]
    -> 'a Computation.t
    -> (Types.Acc.t * Types.Up.t * 'a Computation.t) Trampoline.t

  (** Like [on_computation], but for values instead. *)
  val on_value
    :  Types.Down.t
    -> Types.Acc.t
    -> [ `Directly_on | `Skipping_over ]
    -> 'a Value.t
    -> Types.Acc.t * Types.Up.t * 'a Value.t
end

module type Transform = sig
  module Types : Types

  val transform_c
    :  Types.Down.t
    -> Types.Acc.t
    -> 'a Computation.t
    -> (Types.Acc.t * Types.Up.t * 'a Computation.t) Trampoline.t

  val transform_v
    :  Types.Down.t
    -> Types.Acc.t
    -> 'a Value.t
    -> Types.Acc.t * Types.Up.t * 'a Value.t
end

module type S = sig
  module Unit = Unit

  module type Recurse = Recurse

  module Make
      (Types : Types)
      (_ : functor
         (_ : Recurse with module Types := Types)
         -> Transform with module Types := Types) : Transform with module Types := Types
end
