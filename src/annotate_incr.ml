open! Core
module Incr = Ui_incr
module Color = Tailwind_colors

let enabled = ref false
let enable () = enabled := true
let disable () = enabled := false
let empty_attrs = String.Map.empty

let attribute_packed pos t =
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
      | Apply_action
      | Value
      | Result
      | Lifecycle
      | Empty_lifecycle
      | Model
      | Model_and_input
      | Assoc_key
      | Assoc_input
      | Assoc_results
      | Assoc_lifecycles
      | Assoc_apply_actions
      | Path
      | Impossible_apply_action
      | Lifecycle_apply_action_pair
    [@@deriving hash, compare, sexp]
  end

  include T
  include Hashable.Make (T)

  let name = function
    | Apply_action -> "apply-action"
    | Value -> "value"
    | Result -> "result"
    | Lifecycle -> "lifecycle"
    | Empty_lifecycle -> "empty lifecycle"
    | Model -> "model"
    | Model_and_input -> "model & input"
    | Assoc_key -> "assoc key"
    | Assoc_input -> "assoc input"
    | Assoc_results -> "assoc result map"
    | Assoc_lifecycles -> "assoc lifecycle map"
    | Assoc_apply_actions -> "assoc apply-action map"
    | Path -> "path"
    | Impossible_apply_action -> "impossible apply-action"
    | Lifecycle_apply_action_pair -> "lifecycle/apply-action pair"
  ;;

  let color = function
    (* results
       red *)
    | Value -> Color.red50
    | Result -> Color.red100
    | Assoc_results -> Color.red200
    | Path -> Color.red300
    (* apply-action
       yellow *)
    | Apply_action -> Color.yellow100
    | Assoc_apply_actions -> Color.yellow200
    | Impossible_apply_action -> Color.yellow300
    (* lifecycles
       blue *)
    | Lifecycle -> Color.blue100
    | Assoc_lifecycles -> Color.blue200
    | Empty_lifecycle -> Color.blue300
    (* models
       green *)
    | Model -> Color.green100
    | Lifecycle_apply_action_pair -> Color.green200
    (* assoc-related
       pink *)
    | Model_and_input -> Color.pink100
    | Assoc_key -> Color.pink200
    | Assoc_input -> Color.pink300
  ;;
end

let annotate_packed =
  Memo.general ~hashable:Kind.hashable (fun kind incr ->
    let label = [ Kind.name kind ] in
    let (`Hex color) = Kind.color kind in
    let attrs = String.Map.of_alist_exn [ "style", "filled"; "fillcolor", color ] in
    Incr.Packed.append_user_info_graphviz incr ~label ~attrs)
;;

(* The "is enabled" check are performed here in order to avoid
   going through the memoization or allocation of a Packed.t even
   when disabled. *)
let annotate_packed kind incr = if !enabled then annotate_packed kind incr
let annotate kind incr = if !enabled then annotate_packed kind (Incr.pack incr)

let attribute_packed pos_opt t =
  match !enabled, pos_opt with
  | true, Some pos -> attribute_packed pos t
  | _ -> ()
;;

let attribute pos_opt incr = if !enabled then attribute_packed pos_opt (Incr.pack incr)
