open! Core_kernel

type t =
  | Packed : 'a * 'a Type_equal.Id.t -> t
  (** External_event simulates an e.g. Tangle event *)
  | External_event : string -> t

val pack : 'a Type_equal.Id.t -> 'a -> t
