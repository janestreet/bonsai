open! Core_kernel

type t =
  | Packed : 'a * 'a Type_equal.Id.t -> t
  | External_event : string -> t
  | No_op : t
  | Sequence : t list -> t

val pack : 'a Type_equal.Id.t -> 'a -> t
val sequence : t list -> t
val no_op : t
