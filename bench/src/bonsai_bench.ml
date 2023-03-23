open! Core
module Interaction = Interaction
include Config

let to_core_bench_test (T { clock; name; component; get_inject; interaction } : t) =
  let bonsai_bench_initialize_run `init =
    let runner =
      Runner.initialize
        ~clock
        ~component
        ~wrap_driver_creation:{ f = (fun create_driver -> create_driver ()) }
        ~get_inject
        ~interaction
        ~filter_profiles:true
    in
    Cleanup.register_driver runner;
    fun () -> Runner.run_interactions runner ~handle_profile:(Fn.const ())
  in
  Core_bench_js.Test.create_with_initialization ~name bonsai_bench_initialize_run
;;

let benchmark ?run_config ?analysis_configs ?display_config ?save_to_file ?libname ts =
  ts
  |> List.map ~f:to_core_bench_test
  |> List.intersperse ~sep:Cleanup.invalidate_observers
  |> Core_bench_js.bench
       ?run_config
       ?analysis_configs
       ?display_config
       ?save_to_file
       ?libname
;;

let profile profiles = List.iter ~f:Profile.profile profiles
