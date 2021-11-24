open! Core
open! Bonsai
open! Bonsai_test
module Driver = Arrow.Driver

type t = Core_bench_js.Test.t

let rec flatten_to_list = function
  | Interaction.Many nested -> List.concat_map nested ~f:flatten_to_list
  | _ as t -> [ t ]
;;

let dedupe_stabilizes interactions =
  let both_stabilize t t' =
    match t, t' with
    | Interaction.Stabilize, Interaction.Stabilize -> true
    | _ -> false
  in
  List.remove_consecutive_duplicates interactions ~equal:both_stabilize
;;

(* [handle_interaction] needn't handle [Many]s, since we flatten the whole structure prior
   to running the benchmark. *)
let handle_interaction ~driver ~clock ~inject_action interaction =
  match (interaction : _ Interaction.t) with
  | Stabilize -> Driver.flush driver
  | Reset_model -> Driver.reset_model_to_default driver
  | Change_input (var, value) -> Var.set var value
  | Inject action -> Driver.schedule_event driver (inject_action action)
  | Advance_clock_by span -> Incr.Clock.advance_clock_by clock span
  | Many _ -> assert false
;;

let create
      ?(clock = Incr.Clock.create ~start:Time_ns.epoch ())
      ~name
      ~component
      ~get_inject
      interaction
  =
  let run_test `init =
    let component (_ : _ Value.t) = component in
    let driver = Driver.create ~clock ~initial_input:() component in
    Cleanup.register_driver driver;
    let inject_action action =
      (* Calling Driver.result every time that inject_action is called
         is important because the value can change during stabilization *)
      let result = Driver.result driver in
      (get_inject result) action
    in
    (* We perform two optimizations in this step: flattening the interactions and deduping
       stabilizations. Flattening the structure ensures that there's no additional
       overhead to nesting lots of [Many]s when creating benchmarks. Consecutive
       [Stabilize]s don't add anything to benchmarks and would add a function call of
       overhead. *)
    let interactions =
      Interaction.many [ Interaction.Stabilize; interaction; Interaction.Stabilize ]
      |> flatten_to_list
      |> dedupe_stabilizes
      |> Array.of_list
    in
    fun () ->
      Array.iter interactions ~f:(handle_interaction ~driver ~clock ~inject_action)
  in
  Core_bench_js.Test.create_with_initialization ~name run_test
;;

let create_with_resetter ?clock ~name ~component ~get_inject interaction =
  [ interaction; Interaction.Reset_model; Interaction.Stabilize ]
  |> Interaction.many
  |> create ?clock ~name ~component ~get_inject
;;
