open! Core
module Incr = Ui_incr

module Color = struct
  (* From [bonsai.tailwind_colors]. *)
  let red50 = `Hex "#FEF2F2"
  let red100 = `Hex "#FEE2E2"
  let red200 = `Hex "#FECACA"
  let red300 = `Hex "#FCA5A5"
  let amber100 = `Hex "#FEF3C7"
  let amber200 = `Hex "#FDE68A"
  let blue100 = `Hex "#DBEAFE"
  let blue200 = `Hex "#BFDBFE"
  let blue300 = `Hex "#93C5FD"
  let emerald100 = `Hex "#D1FAE5"
  let emerald200 = `Hex "#A7F3D0"
  let emerald300 = `Hex "#6EE7B7"
  let pink100 = `Hex "#FCE7F3"
  let pink200 = `Hex "#FBCFE8"
  let pink300 = `Hex "#F9A8D4"
end

let enabled = ref false
let enable () = enabled := true
let disable () = enabled := false
let empty_attrs = String.Map.empty

let attribute_loc pos t =
  let pos =
    pos
    |> Source_code_position.to_string
    |> String.chop_prefix_if_exists ~prefix:"lib/"
    |> String.chop_prefix_if_exists ~prefix:"app/"
  in
  let labels =
    match String.rsplit2 ~on:'/' pos with
    | Some (l, r) -> [ "bound: " ^ l; "bound: " ^ r ]
    | None -> [ pos ]
  in
  List.iter labels ~f:(fun label ->
    let label = [ label ] in
    Incr.Packed.append_user_info_graphviz t ~label ~attrs:empty_attrs)
;;

module Kind = struct
  module T = struct
    type t =
      | Input
      | Value
      | Result
      | Lifecycle
      | Empty_lifecycle
      | Model
      | Model_and_input
      | Switch_model
      | Assoc_key
      | Assoc_input
      | Assoc_results
      | Assoc_lifecycles
      | Assoc_inputs
      | Path
      | Lifecycle_apply_action_pair
    [@@deriving hash, compare, sexp]
  end

  include T
  include Hashable.Make (T)

  let name = function
    | Input -> "input"
    | Value -> "value"
    | Result -> "result"
    | Lifecycle -> "lifecycle"
    | Empty_lifecycle -> "empty lifecycle"
    | Model -> "model"
    | Model_and_input -> "model & input"
    | Switch_model -> "switch model"
    | Assoc_key -> "assoc key"
    | Assoc_input -> "assoc input"
    | Assoc_results -> "assoc result map"
    | Assoc_lifecycles -> "assoc lifecycle map"
    | Assoc_inputs -> "assoc input map"
    | Path -> "path"
    | Lifecycle_apply_action_pair -> "lifecycle/apply-action pair"
  ;;

  let color = function
    (* results red *)
    | Value -> Color.red50
    | Result -> Color.red100
    | Assoc_results -> Color.red200
    | Path -> Color.red300
    (* input amber *)
    | Input -> Color.amber100
    | Assoc_inputs -> Color.amber200
    (* lifecycles blue *)
    | Lifecycle -> Color.blue100
    | Assoc_lifecycles -> Color.blue200
    | Empty_lifecycle -> Color.blue300
    (* models green *)
    | Model -> Color.emerald100
    | Lifecycle_apply_action_pair -> Color.emerald200
    | Switch_model -> Color.emerald300
    (* assoc-related pink *)
    | Model_and_input -> Color.pink100
    | Assoc_key -> Color.pink200
    | Assoc_input -> Color.pink300
  ;;
end

let annotate_for_graphviz kind (incr : Ui_incr.Packed.t) =
  Memo.general
    ~hashable:Kind.hashable
    (fun kind incr ->
      let label = [ "kind"; Kind.name kind ] in
      let (`Hex color) = Kind.color kind in
      let attrs = String.Map.of_alist_exn [ "style", "filled"; "fillcolor", color ] in
      Incr.Packed.append_user_info_graphviz incr ~label ~attrs)
    kind
    incr
;;

module Counts = struct
  type t =
    { mutable input : int
    ; mutable value : int
    ; mutable result : int
    ; mutable lifecycle : int
    ; mutable empty_lifecycle : int
    ; mutable model : int
    ; mutable model_and_input : int
    ; mutable switch_model : int
    ; mutable assoc_key : int
    ; mutable assoc_input : int
    ; mutable assoc_results : int
    ; mutable assoc_lifecycles : int
    ; mutable assoc_inputs : int
    ; mutable path : int
    ; mutable lifecycle_apply_action_pair : int
    }
  [@@deriving sexp_of]

  let global =
    { input = 0
    ; value = 0
    ; result = 0
    ; lifecycle = 0
    ; empty_lifecycle = 0
    ; model = 0
    ; model_and_input = 0
    ; switch_model = 0
    ; assoc_key = 0
    ; assoc_input = 0
    ; assoc_results = 0
    ; assoc_lifecycles = 0
    ; assoc_inputs = 0
    ; path = 0
    ; lifecycle_apply_action_pair = 0
    }
  ;;

  let current () =
    { input = global.input
    ; value = global.value
    ; result = global.result
    ; lifecycle = global.lifecycle
    ; empty_lifecycle = global.empty_lifecycle
    ; model = global.model
    ; model_and_input = global.model_and_input
    ; switch_model = global.switch_model
    ; assoc_key = global.assoc_key
    ; assoc_input = global.assoc_input
    ; assoc_results = global.assoc_results
    ; assoc_lifecycles = global.assoc_lifecycles
    ; assoc_inputs = global.assoc_inputs
    ; path = global.path
    ; lifecycle_apply_action_pair = global.lifecycle_apply_action_pair
    }
  ;;

  let diff ~before ~after =
    { input = after.input - before.input
    ; value = after.value - before.value
    ; result = after.result - before.result
    ; lifecycle = after.lifecycle - before.lifecycle
    ; empty_lifecycle = after.empty_lifecycle - before.empty_lifecycle
    ; model = after.model - before.model
    ; model_and_input = after.model_and_input - before.model_and_input
    ; switch_model = after.switch_model - before.switch_model
    ; assoc_key = after.assoc_key - before.assoc_key
    ; assoc_input = after.assoc_input - before.assoc_input
    ; assoc_results = after.assoc_results - before.assoc_results
    ; assoc_lifecycles = after.assoc_lifecycles - before.assoc_lifecycles
    ; assoc_inputs = after.assoc_inputs - before.assoc_inputs
    ; path = after.path - before.path
    ; lifecycle_apply_action_pair =
        after.lifecycle_apply_action_pair - before.lifecycle_apply_action_pair
    }
  ;;

  let empty () =
    { input = 0
    ; value = 0
    ; result = 0
    ; lifecycle = 0
    ; empty_lifecycle = 0
    ; model = 0
    ; model_and_input = 0
    ; switch_model = 0
    ; assoc_key = 0
    ; assoc_input = 0
    ; assoc_results = 0
    ; assoc_lifecycles = 0
    ; assoc_inputs = 0
    ; path = 0
    ; lifecycle_apply_action_pair = 0
    }
  ;;

  let incr t : Kind.t -> unit = function
    | Input -> t.input <- t.input + 1
    | Value -> t.value <- t.value + 1
    | Result -> t.result <- t.result + 1
    | Lifecycle -> t.lifecycle <- t.lifecycle + 1
    | Empty_lifecycle -> t.empty_lifecycle <- t.empty_lifecycle + 1
    | Model -> t.model <- t.model + 1
    | Model_and_input -> t.model_and_input <- t.model_and_input + 1
    | Switch_model -> t.switch_model <- t.switch_model + 1
    | Assoc_key -> t.assoc_key <- t.assoc_key + 1
    | Assoc_input -> t.assoc_input <- t.assoc_input + 1
    | Assoc_results -> t.assoc_results <- t.assoc_results + 1
    | Assoc_lifecycles -> t.assoc_lifecycles <- t.assoc_lifecycles + 1
    | Assoc_inputs -> t.assoc_inputs <- t.assoc_inputs + 1
    | Path -> t.path <- t.path + 1
    | Lifecycle_apply_action_pair ->
      t.lifecycle_apply_action_pair <- t.lifecycle_apply_action_pair + 1
  ;;

  let incr_global = incr global
end

let incr_annotation_listeners =
  ref
    [ (fun ~here:_ kind _ ->
        (* NOTE: Because this counting is cheap, it is safe to always do it. *)
        Counts.incr_global kind)
    ; (fun ~here:_ kind incr ->
        (* The "is enabled" check is performed here in order to avoid going through the
           memoization or allocation of a Packed.t even when disabled. *)
        if !enabled then annotate_for_graphviz kind incr)
    ; (fun ~here:_ kind _ ->
        let metrics_counter_kind =
          match kind with
          | Input -> Ui_metrics.Counters.Kind.Incr_node_input
          | Value -> Incr_node_value
          | Result -> Incr_node_result
          | Lifecycle -> Incr_node_lifecycle
          | Empty_lifecycle -> Incr_node_empty_lifecycle
          | Model -> Incr_node_model
          | Model_and_input -> Incr_node_model_and_input
          | Switch_model -> Incr_node_switch_model
          | Assoc_key -> Incr_node_assoc_key
          | Assoc_input -> Incr_node_assoc_input
          | Assoc_results -> Incr_node_assoc_results
          | Assoc_lifecycles -> Incr_node_assoc_lifecycles
          | Assoc_inputs -> Incr_node_assoc_input
          | Path -> Incr_node_path
          | Lifecycle_apply_action_pair -> Incr_node_lifecycle_apply_action_pair
        in
        Ui_metrics.Counters.observe metrics_counter_kind)
    ]
;;

let on_incr_annotation f = incr_annotation_listeners := f :: !incr_annotation_listeners

let annotate_packed ~here kind incr =
  List.iter !incr_annotation_listeners ~f:(fun f -> f ~here kind incr)
;;

let annotate (type a) ~here kind (incr : a Ui_incr.t) =
  annotate_packed ~here kind (Ui_incr.pack incr)
;;

let attribute_packed pos incr = if !enabled then attribute_loc pos incr
let attribute pos_opt incr = attribute_packed pos_opt (Ui_incr.pack incr)

module For_profiling = struct
  module Counts = struct
    type t =
      { input : int
      ; value : int
      ; result : int
      ; lifecycle : int
      ; empty_lifecycle : int
      ; model : int
      ; model_and_input : int
      ; switch_model : int
      ; assoc_key : int
      ; assoc_input : int
      ; assoc_results : int
      ; assoc_lifecycles : int
      ; assoc_inputs : int
      ; path : int
      ; lifecycle_apply_action_pair : int
      }
    [@@deriving sexp, equal]

    let t_of_opaque_counts
      ({ input
       ; value
       ; result
       ; lifecycle
       ; empty_lifecycle
       ; model
       ; model_and_input
       ; switch_model
       ; assoc_key
       ; assoc_input
       ; assoc_results
       ; assoc_lifecycles
       ; assoc_inputs
       ; path
       ; lifecycle_apply_action_pair
       } :
        Counts.t)
      : t
      =
      { input
      ; value
      ; result
      ; lifecycle
      ; empty_lifecycle
      ; model
      ; model_and_input
      ; switch_model
      ; assoc_key
      ; assoc_input
      ; assoc_results
      ; assoc_lifecycles
      ; assoc_inputs
      ; path
      ; lifecycle_apply_action_pair
      }
    ;;

    let opaque_counts_of_t
      ({ input
       ; value
       ; result
       ; lifecycle
       ; empty_lifecycle
       ; model
       ; model_and_input
       ; switch_model
       ; assoc_key
       ; assoc_input
       ; assoc_results
       ; assoc_lifecycles
       ; assoc_inputs
       ; path
       ; lifecycle_apply_action_pair
       } :
        t)
      : Counts.t
      =
      { input
      ; value
      ; result
      ; lifecycle
      ; empty_lifecycle
      ; model
      ; model_and_input
      ; switch_model
      ; assoc_key
      ; assoc_input
      ; assoc_results
      ; assoc_lifecycles
      ; assoc_inputs
      ; path
      ; lifecycle_apply_action_pair
      }
    ;;

    let diff ~before ~after =
      Counts.diff ~before:(opaque_counts_of_t before) ~after:(opaque_counts_of_t after)
      |> t_of_opaque_counts
    ;;
  end
end
