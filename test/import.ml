open! Core_kernel

module Event = struct
  include Ui_event

  module External = Define (struct
      module Action = String

      let handle str = printf "External event: %s\n" str
    end)

  let sequence l = Many l
  let no_op = Ignore
  let external_ = External.inject
end

module Incr = Ui_incr
include Expect_test_helpers_core
