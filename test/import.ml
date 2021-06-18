open! Core

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

let dummy_source_code_position =
  Source_code_position.
    { pos_fname = "file_name.ml"; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 }
;;
