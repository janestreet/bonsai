open! Core_kernel
include Virtual_dom
include Bonsai_web

module Event = struct
  type t = Vdom.Event.t

  let no_op = Vdom.Event.Ignore
  let sequence xs = Vdom.Event.Many xs
  let external_ = External_event.inject
end

module Driver = Bonsai_test.Arrow.Driver
include Expect_test_helpers_core
