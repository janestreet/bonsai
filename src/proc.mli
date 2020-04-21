open! Core_kernel
open! Import
open Component

type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
  | Abstraction :
      { t : (unit, 'model, 'action, 'result, 'incr, 'event) unpacked
      ; type_id : 'a Type_equal.Id.t
      }
      -> ('a, 'model, 'action, 'result, 'incr, 'event) unpacked
  | Var :
      { type_id : 'a Type_equal.Id.t }
      -> (unit, unit, Nothing.t, 'a, 'incr, 'event) unpacked

module Val : sig
  type 'a t

  include Applicative.S with type 'a t := 'a t
  include Applicative.Let_syntax with type 'a t := 'a t
end

module Computation : sig
  type ('a, 'incr, 'event) t
end

val subst
  :  ('a, 'incr, 'event) Computation.t
  -> f:('a Val.t -> ('b, 'incr, 'event) Computation.t)
  -> ('b, 'incr, 'event) Computation.t

val return : 'a Val.t -> ('a, _, _) Computation.t

val apply
  :  ('a, 'b, 'incr, 'event) Packed.t
  -> 'a Val.t
  -> ('b, 'incr, 'event) Computation.t

val apply_unit : (unit, 'b, 'incr, 'event) Packed.t -> ('b, 'incr, 'event) Computation.t

val proc
  :  ('a Val.t -> ('b, 'incr, 'event) Computation.t)
  -> ('a, 'b, 'incr, 'event) Packed.t

val if_
  :  bool Val.t
  -> then_:('a, 'incr, 'event) Computation.t
  -> else_:('a, 'incr, 'event) Computation.t
  -> ('a, 'incr, 'event) Computation.t

val enum
  :  (module Switch.Enum with type t = 'a)
  -> match_:'a Val.t
  -> with_:('a -> ('b, 'incr, 'event) Computation.t)
  -> ('b, 'incr, 'event) Computation.t
