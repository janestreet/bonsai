open! Core

module Input = struct
  type 'a t =
    { mutable curr_for_constructing_interactions : 'a
    ; var : 'a Bonsai.Expert.Var.t
    }

  let create initial =
    { var = Bonsai.Expert.Var.create initial
    ; curr_for_constructing_interactions = initial
    }
  ;;

  let value { var; _ } = Bonsai.Expert.Var.value var

  let change_for_interaction t ~f =
    let new_ = f t.curr_for_constructing_interactions in
    t.curr_for_constructing_interactions <- new_;
    new_
  ;;
end

module Interaction = struct
  type 'action t =
    | Profile : string -> _ t
    | Change_input : 'a Bonsai.Expert.Var.t * 'a -> _ t
    | Inject : 'action -> 'action t
    | Advance_clock_by : Time_ns.Span.t -> _ t
    | Stabilize : _ t
    | Reset_model : _ t
    | Many : 'action t list -> 'action t

  let update_input input ~f =
    let value = Input.change_for_interaction input ~f in
    Change_input (input.var, value)
  ;;

  let change_input input value = update_input input ~f:(fun _ -> value)
  let inject action = Inject action
  let advance_clock_by span = Advance_clock_by span
  let stabilize = Stabilize
  let reset_model = Reset_model
  let profile ~name = Profile name
  let many ts = Many ts
  let many_with_stabilizations ts = Many (List.intersperse ts ~sep:Stabilize)

  module Finalized = struct
    type nonrec 'a t = 'a t

    let handle ~driver ~time_source ~inject_action ~handle_profile interaction =
      match (interaction : _ t) with
      | Profile name -> handle_profile name
      | Stabilize -> Bonsai_driver.flush driver
      | Reset_model -> Bonsai_driver.Expert.reset_model_to_default driver
      | Change_input (var, value) -> Bonsai.Expert.Var.set var value
      | Inject action -> Bonsai_driver.schedule_event driver (inject_action action)
      | Advance_clock_by span -> Bonsai.Time_source.advance_clock_by time_source span
      | Many _ ->
        (* We flatten the interaction structure prior to running the benchmark. *)
        assert false
    ;;
  end

  let rec flatten_interactions_to_list = function
    | Many nested -> List.concat_map nested ~f:flatten_interactions_to_list
    | t -> [ t ]
  ;;

  let dedup_stabilizations interactions =
    let both_stabilize t t' =
      match t, t' with
      | Stabilize, Stabilize -> true
      | _ -> false
    in
    List.remove_consecutive_duplicates interactions ~equal:both_stabilize
  ;;

  let finalize ~filter_profiles interactions =
    flatten_interactions_to_list interactions
    |> List.filter ~f:(fun interaction ->
      match filter_profiles, interaction with
      | true, Profile _ -> false
      | _ -> true)
    |> dedup_stabilizations
  ;;
end

module Scenario = struct
  type ('input, 'action) t =
    { initial : 'input
    ; test_name : string
    ; interaction : 'input Input.t -> 'action Interaction.t
    }
end

module type Config = sig
  type t [@@deriving compare, sexp_of]
  type input
  type output
  type action

  val name : t -> string
  val computation : t -> input Bonsai.t -> Bonsai.graph -> output Bonsai.t
  val get_inject : output -> action -> unit Bonsai.Effect.t
end
