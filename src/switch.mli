open! Core_kernel
open! Import
open Component

module type Enum = sig
  type t [@@deriving compare, enumerate, sexp]
end

val enum
  :  (module Enum with type t = 'k)
  -> which:('a -> 'k)
  -> handle:('k -> ('a, 'b, 'c, 'd) Packed.t)
  -> ('a, 'b, 'c, 'd) Packed.t

val if_
  :  ('input -> bool)
  -> then_:('input, 'result, 'incr, 'event) Packed.t
  -> else_:('input, 'result, 'incr, 'event) Packed.t
  -> ('input, 'result, 'incr, 'event) Packed.t
