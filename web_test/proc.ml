open! Core_kernel
open! Import

module Result_spec = struct
  include Bonsai_test.Result_spec

  let vdom (type result) ?should_print_styles get_vdom =
    (module struct
      type t = result

      include No_incoming

      let view result =
        result
        |> get_vdom
        |> Virtual_dom_test_helpers.Node_helpers.unsafe_convert_exn
        |> Virtual_dom_test_helpers.Node_helpers.to_string_html ?should_print_styles
      ;;
    end : S
      with type t = result
       and type incoming = Nothing.t)
  ;;
end

module Handle = struct
  include Bonsai_test.Handle
  open Virtual_dom_test_helpers

  let get_element handle ~get_vdom ~selector =
    let node = handle |> result |> get_vdom |> Node_helpers.unsafe_convert_exn in
    Node_helpers.select_first_exn node ~selector
  ;;

  let click_on handle ~get_vdom ~selector =
    let element = get_element handle ~get_vdom ~selector in
    Node_helpers.User_actions.click_on element
  ;;

  let input_text handle ~get_vdom ~selector ~text =
    let element = get_element handle ~get_vdom ~selector in
    Node_helpers.User_actions.input_text element ~text
  ;;

  let trigger_hook handle ~get_vdom ~selector ~name type_id arg =
    get_element handle ~get_vdom ~selector
    |> Node_helpers.trigger_hook ~type_id ~name ~arg
  ;;

  module Drag_and_drop = struct
    type action =
      | Drag of string
      | Enter of string
      | Leave
      | Drop
    [@@deriving sexp]

    type t =
      | Not_dragging
      | Dragging of string
      | Over_target of string * string
    [@@deriving sexp]

    let (not_dragging : t) = Not_dragging

    let run handle ~get_vdom state actions =
      List.fold actions ~init:state ~f:(fun state action ->
        let raise_with_message message =
          raise_s [%message message (state : t) (action : action)]
        in
        match state with
        | Over_target (dragged, target) ->
          (match action with
           | Drag _ -> raise_with_message "Already started dragging"
           | Enter _ -> raise_with_message "Already inside drag target"
           | Leave ->
             let target_element = get_element handle ~get_vdom ~selector:target in
             Node_helpers.User_actions.leave target_element;
             Dragging dragged
           | Drop ->
             let target_element = get_element handle ~get_vdom ~selector:target in
             Node_helpers.User_actions.drop target_element;
             let dragged_element = get_element handle ~get_vdom ~selector:dragged in
             Node_helpers.User_actions.end_ dragged_element;
             recompute_view handle;
             Not_dragging)
        | Dragging dragged ->
          (match action with
           | Drag _ -> raise_with_message "Already started dragging"
           | Enter target ->
             let target_element = get_element handle ~get_vdom ~selector:target in
             Node_helpers.User_actions.enter target_element;
             recompute_view handle;
             Over_target (dragged, target)
           | Drop ->
             let dragged_element = get_element handle ~get_vdom ~selector:dragged in
             Node_helpers.User_actions.end_ dragged_element;
             recompute_view handle;
             Not_dragging
           | Leave -> raise_with_message "Not inside drop target yet")
        | Not_dragging ->
          (match action with
           | Drag dragged ->
             let dragged_element = get_element handle ~get_vdom ~selector:dragged in
             Node_helpers.User_actions.drag dragged_element;
             Dragging dragged
           | Enter _ | Leave | Drop -> raise_with_message "Not yet started dragging"))
    ;;
  end
end
