open! Core_kernel

module Event = struct
  type t =
    | Packed : 'a * 'a Type_equal.Id.t -> t
    | External_event : string -> t
    | No_op : t
    | Sequence : t list -> t

  let pack type_id t = Packed (t, type_id)
  let sequence events = Sequence events
  let no_op = No_op
  let external_ string = External_event string
end

module Incr = Incremental.Make ()
module Bonsai_lib = Bonsai

module Bonsai = struct
  module Event = Event
  module Incr = Incr
  include Bonsai.Make (Incr) (Event)
end

module Incr_map = Incr_map.Make (Incr)
include Expect_test_helpers_core
