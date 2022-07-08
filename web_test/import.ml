open! Core
include Virtual_dom
include Bonsai_web

module Effect = struct
  type 'a t = 'a Vdom.Effect.t

  let no_op = Vdom.Effect.Ignore
  let sequence xs = Vdom.Effect.Many xs
  let external_ = External_event.inject
end

module Driver = Bonsai_test.Arrow.Driver
include Expect_test_helpers_core

let opaque_const x = Bonsai.read (Bonsai.Var.value (Bonsai.Var.create x))
let opaque_const_value x = Bonsai.Var.value (Bonsai.Var.create x)
