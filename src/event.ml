open! Core_kernel
open! Import

module type S = sig
  type t

  val sequence : t list -> t
  val no_op : t
end
