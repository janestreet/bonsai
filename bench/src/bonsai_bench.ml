open! Core
open! Bonsai
module Test = Test
module Interaction = Interaction
include Interaction

let bench ?run_config ?analysis_configs ?display_config ?save_to_file ?libname tests =
  tests
  |> List.intersperse ~sep:Cleanup.invalidate_observers
  |> Core_bench_js.bench
       ?run_config
       ?analysis_configs
       ?display_config
       ?save_to_file
       ?libname
;;
