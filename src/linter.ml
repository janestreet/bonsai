open! Core
open! Import

module Warning = struct
  type t =
    { message : string
    ; here : Source_code_position.t
    }

  let to_string { here; message } = [%string "%{here#Source_code_position}: %{message}"]
  let unfolded_constant here = { here; message = "unfolded constant" }

  let state_machine1_can_be_state_machine0 here =
    { here; message = "state_machine1 can be optimized to a state_machine0" }
  ;;

  let relative_to (base : Source_code_position.t) t =
    if String.equal base.pos_fname t.here.pos_fname
    then { t with here = { t.here with pos_lnum = t.here.pos_lnum - base.pos_lnum } }
    else t
  ;;
end

let rec value_is_constant : Skeleton.Value.t -> bool =
  fun { kind; _ } ->
  match kind with
  | Constant | Exception -> true
  | Incr | Named _ -> false
  | Cutoff { t; added_by_let_syntax = _ } -> value_is_constant t
  | Mapn { inputs } -> List.for_all inputs ~f:value_is_constant
;;

let unfolded_constants_linter =
  object
    inherit [Warning.t list] Skeleton.Traverse.fold as super

    method! value (value : Skeleton.Value.t) warnings =
      let here = value.here in
      let is_unfolded_constant =
        match value.kind with
        | Constant | Exception | Incr | Named _ -> false
        | Cutoff { t; added_by_let_syntax = _ } -> value_is_constant t
        | Mapn { inputs } -> List.for_all inputs ~f:value_is_constant
      in
      if is_unfolded_constant
      then Warning.unfolded_constant here :: warnings
      else super#value value warnings
  end
;;

let state_machine1_to_state_machine0_linter =
  object
    inherit [Warning.t list] Skeleton.Traverse.fold as super

    method! computation computation warnings =
      let here = computation.here in
      let warnings =
        match computation.kind with
        | Leaf1 { input; _ } ->
          if value_is_constant input
          then Warning.state_machine1_can_be_state_machine0 here :: warnings
          else warnings
        | Return _
        | Leaf01 _
        | Leaf0
        | Leaf_incr _
        | Model_cutoff _
        | Sub _
        | Store _
        | Fetch _
        | Assoc _
        | Assoc_on _
        | Assoc_simpl _
        | Switch _
        | Lazy _
        | Wrap _
        | With_model_resetter _
        | Path
        | Lifecycle _
        | Fix_define _
        | Fix_recurse _
        | Computation_watcher _
        | Identity _ -> warnings
      in
      super#computation computation warnings
  end
;;

let list_warnings computation =
  let computation = Skeleton.Computation.of_computation computation in
  let linters = [ unfolded_constants_linter; state_machine1_to_state_machine0_linter ] in
  List.fold linters ~init:[] ~f:(fun warnings linter ->
    let warnings = linter#computation computation warnings in
    warnings)
;;
