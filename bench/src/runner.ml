open! Core
open Bonsai

type t =
  | T :
      { driver : 'r Bonsai_driver.t
      ; clock : Ui_incr.Clock.t
      ; inject_action : 'a -> unit Effect.t
      ; interactions : 'a Interaction.t array
      }
      -> t

type wrap_create = { f : 'a. (unit -> 'a) -> 'a } [@@unboxed]

let handle_interaction ~driver ~clock ~inject_action ~handle_profile interaction =
  match (interaction : _ Interaction.t) with
  | Profile name -> handle_profile name
  | Stabilize -> Bonsai_driver.flush driver
  | Reset_model -> Bonsai_driver.Expert.reset_model_to_default driver
  | Change_input (var, value) -> Var.set var value
  | Inject action -> Bonsai_driver.schedule_event driver (inject_action action)
  | Advance_clock_by span -> Ui_incr.Clock.advance_clock_by clock span
  | Many _ ->
    (* We flatten the interaction structure prior to running the benchmark. *)
    assert false
;;

let rec flatten_interactions_to_list = function
  | Interaction.Many nested -> List.concat_map nested ~f:flatten_interactions_to_list
  | t -> [ t ]
;;

let dedup_stabilizations interactions =
  let both_stabilize (t : _ Interaction.t) (t' : _ Interaction.t) =
    match t, t' with
    | Stabilize, Stabilize -> true
    | _ -> false
  in
  List.remove_consecutive_duplicates interactions ~equal:both_stabilize
;;

(* We perform two optimizations in this step: flattening the interactions and deduping
   stabilizations. Flattening the structure ensures that there's no additional
   overhead to nesting lots of [Many]s when creating benchmarks. Consecutive
   [Stabilize]s don't add anything to benchmarks and would add a function call of
   overhead. *)
let initialize
      ~filter_profiles
      ~wrap_driver_creation
      ~clock
      ~component
      ~get_inject
      ~interaction
  =
  let driver = wrap_driver_creation.f (fun () -> Bonsai_driver.create ~clock component) in
  let inject_action action =
    (* Calling Driver.result every time that inject_action is called
       is important because the value can change during stabilization *)
    let result = Bonsai_driver.result driver in
    (get_inject result) action
  in
  let interactions =
    Interaction.many
      [ Interaction.stabilize
      ; interaction
      ; Interaction.stabilize
      ; Interaction.profile ~name:"end of run"
      ]
    |> flatten_interactions_to_list
    |> List.filter ~f:(fun interaction ->
      match filter_profiles, interaction with
      | true, Profile _ -> false
      | _ -> true)
    |> dedup_stabilizations
    |> Array.of_list
  in
  T { driver; clock; inject_action; interactions }
;;

let run_interactions (T { driver; clock; inject_action; interactions }) ~handle_profile =
  Array.iter
    interactions
    ~f:(handle_interaction ~driver ~clock ~inject_action ~handle_profile)
;;

let invalidate_observers (T t) = Bonsai_driver.Expert.invalidate_observers t.driver
