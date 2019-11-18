open! Core_kernel

type t =
  | Packed : 'a * 'a Type_equal.Id.t -> t
  | External_event : string -> t

let pack type_id t = Packed (t, type_id)
