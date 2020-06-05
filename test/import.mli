open! Core_kernel

module Event : sig
  type t = Ui_event.t

  val sequence : t list -> t
  val no_op : t
  val external_ : string -> t
end

module Incr = Ui_incr

include module type of struct
  include Expect_test_helpers_core
end
