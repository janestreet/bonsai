open! Core
open! Import

module Result_spec = struct
  include Bonsai_test.Result_spec

  let vdom (type result) ?filter_printed_attributes get_vdom =
    (module struct
      type t = result

      include No_incoming

      let view result =
        result
        |> get_vdom
        |> Virtual_dom_test_helpers.Node_helpers.unsafe_convert_exn
        |> Virtual_dom_test_helpers.Node_helpers.to_string_html ?filter_printed_attributes
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

  let click_on
        ?extra_event_fields
        ?shift_key_down
        ?alt_key_down
        ?ctrl_key_down
        handle
        ~get_vdom
        ~selector
    =
    let element = get_element handle ~get_vdom ~selector in
    Node_helpers.User_actions.click_on
      element
      ?extra_event_fields
      ?shift_key_down
      ?alt_key_down
      ?ctrl_key_down
  ;;

  let set_checkbox
        ?extra_event_fields
        ?shift_key_down
        ?alt_key_down
        ?ctrl_key_down
        handle
        ~get_vdom
        ~selector
        ~checked
    =
    let element = get_element handle ~get_vdom ~selector in
    Node_helpers.User_actions.set_checkbox
      element
      ~checked
      ?extra_event_fields
      ?shift_key_down
      ?alt_key_down
      ?ctrl_key_down
  ;;

  let submit_form ?extra_event_fields handle ~get_vdom ~selector =
    let element = get_element handle ~get_vdom ~selector in
    Node_helpers.User_actions.submit_form element ?extra_event_fields
  ;;

  let focus ?extra_event_fields handle ~get_vdom ~selector =
    let element = get_element handle ~get_vdom ~selector in
    Node_helpers.User_actions.focus element ?extra_event_fields
  ;;

  let change ?extra_event_fields handle ~get_vdom ~selector ~value =
    let element = get_element handle ~get_vdom ~selector in
    Node_helpers.User_actions.change element ~value ?extra_event_fields
  ;;

  let blur ?extra_event_fields ?related_target handle ~get_vdom ~selector =
    let element = get_element handle ~get_vdom ~selector in
    let related_target =
      match related_target with
      | Some selector -> Some (get_element handle ~get_vdom ~selector)
      | None -> None
    in
    Node_helpers.User_actions.blur ?related_target element ?extra_event_fields
  ;;

  let mousemove ?extra_event_fields handle ~get_vdom ~selector =
    let element = get_element handle ~get_vdom ~selector in
    Node_helpers.User_actions.mousemove element ?extra_event_fields
  ;;

  let mouseenter ?extra_event_fields handle ~get_vdom ~selector =
    let element = get_element handle ~get_vdom ~selector in
    Node_helpers.User_actions.mouseenter element ?extra_event_fields
  ;;

  let wheel ?extra_event_fields handle ~get_vdom ~selector ~delta_y =
    let element = get_element handle ~get_vdom ~selector in
    Node_helpers.User_actions.wheel element ~delta_y ?extra_event_fields
  ;;

  let input_text ?extra_event_fields handle ~get_vdom ~selector ~text =
    let element = get_element handle ~get_vdom ~selector in
    Node_helpers.User_actions.input_text element ~text ?extra_event_fields
  ;;

  let keydown
        ?extra_event_fields
        ?shift_key_down
        ?alt_key_down
        ?ctrl_key_down
        handle
        ~get_vdom
        ~selector
        ~key
    =
    let element = get_element handle ~get_vdom ~selector in
    Node_helpers.User_actions.keydown
      ?shift_key_down
      ?alt_key_down
      ?ctrl_key_down
      ?extra_event_fields
      element
      ~key
  ;;

  let trigger_hook handle ~get_vdom ~selector ~name type_id arg =
    get_element handle ~get_vdom ~selector
    |> Node_helpers.trigger_hook ~type_id ~name ~arg ~f:Fn.id
  ;;

  let trigger_hook_via handle ~get_vdom ~selector ~name type_id ~f arg =
    get_element handle ~get_vdom ~selector
    |> Node_helpers.trigger_hook ~type_id ~name ~arg ~f
  ;;

  let get_hook_value handle ~get_vdom ~selector ~name type_id =
    get_element handle ~get_vdom ~selector |> Node_helpers.get_hook_value ~type_id ~name
  ;;

  module Bulk_size_tracker = struct
    open Bonsai_web_ui_element_size_hooks

    type change =
      { selector : string
      ; width : float
      ; height : float
      }

    let change_sizes handle ~get_vdom changes =
      Bulk_size_tracker.For_testing.change_sizes
        (List.map changes ~f:(fun { selector; height; width } ->
           ( get_hook_value
               handle
               ~get_vdom
               ~selector
               ~name:Bulk_size_tracker.For_testing.hook_name
               Bulk_size_tracker.For_testing.type_id
           , { Bulk_size_tracker.Dimensions.width; height } )))
    ;;
  end

  module Drag_and_drop = struct
    let run handle ~get_vdom ~name action =
      trigger_hook
        handle
        ~get_vdom
        ~selector:[%string "[data-dnd-name=%{name}]"]
        ~name:"dnd-test-hook"
        Bonsai_web_ui_drag_and_drop.For_testing.type_id
        action
    ;;
  end
end
