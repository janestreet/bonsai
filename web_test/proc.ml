open! Core
open! Import

module Result_spec = struct
  include Bonsai_test.Result_spec

  let vdom
    (type result)
    ?filter_printed_attributes
    ?(censor_paths = true)
    ?(censor_hash = true)
    ?path_censoring_message
    ?hash_censoring_message
    get_vdom
    =
    (module struct
      type t = result

      include No_incoming

      let view result =
        result
        |> get_vdom
        |> Virtual_dom_test_helpers.Node_helpers.unsafe_convert_exn
        |> Virtual_dom_test_helpers.Node_helpers.to_string_html
             ?path_censoring_message
             ?hash_censoring_message
             ?filter_printed_attributes
             ~censor_paths
             ~censor_hash
      ;;
    end : S
      with type t = result
       and type incoming = Nothing.t)
  ;;
end

let add_rpc_implementations_to_computation ~rpc_implementations ~connectors computation =
  match rpc_implementations, connectors with
  | None, None -> computation
  | _ ->
    let rpc_implementations = Option.value rpc_implementations ~default:[] in
    let connectors =
      Option.value connectors ~default:(fun _ ->
        Bonsai_web.Rpc_effect.Connector.test_fallback)
    in
    let test_fallback_connector =
      let open Async_rpc_kernel in
      Rpc_effect.Connector.for_test
        (Rpc.Implementations.create_exn
           ~on_unknown_rpc:`Continue
           ~implementations:(Versioned_rpc.Menu.add rpc_implementations))
        ~connection_state:Fn.id
    in
    let connectors where_to_connect =
      let connector = connectors where_to_connect in
      if Bonsai_web.Rpc_effect.Private.is_test_fallback connector
      then test_fallback_connector
      else connector
    in
    Bonsai_web.Rpc_effect.Private.with_connector connectors computation
;;

module Handle = struct
  include Bonsai_test.Handle

  let create
    result_spec
    ?rpc_implementations
    ?connectors
    ?start_time
    ?optimize
    computation
    =
    computation
    |> add_rpc_implementations_to_computation ~rpc_implementations ~connectors
    |> Bonsai_test.Handle.create result_spec ?start_time ?optimize
  ;;

  let flush_async_and_bonsai
    ?(max_iterations = 100)
    ?(silence_between_frames = false)
    handle
    =
    let open Async_kernel in
    let rec loop i =
      if i = 0
      then
        raise_s [%message [%string "not stable after %{max_iterations#Int} iterations"]];
      if i < max_iterations && not silence_between_frames
      then print_endline "------ between bonsai frame ------";
      let%bind.Eager_deferred () =
        Async_kernel_scheduler.yield_until_no_jobs_remain ~may_return_immediately:true ()
      in
      recompute_view handle;
      if has_after_display_events handle || Async_kernel_scheduler.num_pending_jobs () > 0
      then loop (i - 1)
      else Deferred.unit
    in
    recompute_view handle;
    loop max_iterations
  ;;

  open Virtual_dom_test_helpers

  let get_element handle ~get_vdom ~selector =
    let node = handle |> last_result |> get_vdom |> Node_helpers.unsafe_convert_exn in
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

  module Position_tracker = struct
    open Bonsai_web_ui_element_size_hooks

    type change =
      { selector : string
      ; top : int
      ; left : int
      ; width : int
      ; height : int
      }

    let change_positions handle ~get_vdom changes =
      Position_tracker.For_testing.change_positions
        (List.map changes ~f:(fun { selector; top; left; height; width } ->
           ( get_hook_value
               handle
               ~get_vdom
               ~selector
               ~name:Position_tracker.For_testing.hook_name
               Position_tracker.For_testing.type_id
           , { Position_tracker.Position.top; left; height; width } )))
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

module Experimental = struct
  module Result_spec = struct
    module No_incoming = Bonsai_test.Result_spec.No_incoming

    module type Sexpable = Bonsai_test.Result_spec.Sexpable
    module type Stringable = Bonsai_test.Result_spec.Stringable

    module type S = sig
      include Bonsai_test.Result_spec.S

      val to_vdom : t -> Vdom.Node.t
    end

    type ('result, 'incoming) t =
      (module S with type t = 'result and type incoming = 'incoming)

    let vdom
      ?filter_printed_attributes
      ?(censor_paths = true)
      ?(censor_hash = true)
      ?path_censoring_message
      ?hash_censoring_message
      ?(to_string =
        fun vdom ->
          vdom
          |> Virtual_dom_test_helpers.Node_helpers.unsafe_convert_exn
          |> Virtual_dom_test_helpers.Node_helpers.to_string_html
               ?path_censoring_message
               ?hash_censoring_message
               ?filter_printed_attributes
               ~censor_paths
               ~censor_hash)
      ()
      =
      (module struct
        type t = Vdom.Node.t

        include No_incoming

        let to_vdom result = result
        let view result = to_string result
      end : S
        with type t = Vdom.Node.t
         and type incoming = Nothing.t)
    ;;

    let sexp (type a) (module S : Sexpable with type t = a) =
      let (module Sexp) = Bonsai_test.Result_spec.sexp (module S) in
      let to_vdom result = Vdom.Node.text (Sexp.view result) in
      (module struct
        include Sexp

        let to_vdom = to_vdom
      end : S
        with type t = a
         and type incoming = Nothing.t)
    ;;

    let string (type a) (module S : Stringable with type t = a) =
      let (module String) = Bonsai_test.Result_spec.string (module S) in
      let to_vdom result = Vdom.Node.text (String.view result) in
      (module struct
        include String

        let to_vdom = to_vdom
      end : S
        with type t = a
         and type incoming = Nothing.t)
    ;;
  end

  let perform_update vdom =
    let open Js_of_ocaml in
    let document = Dom_html.document in
    let vdom =
      Vdom.Node.div
        ~attrs:
          [ [%css
              {|
    display: block;
    &:not(:last-child)::after {
      content: "test frame";
      font-size: 0.5em;
      color: #7A7A7A;
      display: block;
      width: 100%;
      border-bottom: 1px solid #7A7A7A;
      margin-top: 0.5em;
      margin-bottom: 0.5em;
      text-align: end;
    }
    |}]
          ]
        [ vdom ]
    in
    let html_dom = Vdom.Node.to_dom vdom in
    Dom.appendChild document##.body html_dom
  ;;

  module Handle = struct
    type ('result, 'incoming) t =
      ( unit
      , 'result * Vdom.Node.t * string Lazy.t * ('incoming -> unit Effect.t) )
      Driver.t

    let create
      (type result incoming)
      (result_spec : (result, incoming) Result_spec.t)
      ?rpc_implementations
      ?connectors
      ?(start_time = Time_ns.epoch)
      ?(optimize = true)
      computation
      =
      let computation =
        add_rpc_implementations_to_computation
          ~rpc_implementations
          ~connectors
          computation
      in
      let (module R) = result_spec in
      let component (_ : unit Value.t) =
        let open Bonsai.Let_syntax in
        let%sub result = computation in
        let%arr result = result in
        result, R.to_vdom result, lazy (R.view result), R.incoming result
      in
      let clock = Bonsai.Time_source.create ~start:start_time in
      Driver.create ~optimize ~initial_input:() ~clock component
    ;;

    let recompute_view (handle : (_, 'r) Driver.t) =
      Driver.flush handle;
      let (_ : 'r) = Driver.result handle in
      Driver.trigger_lifecycles handle
    ;;

    let recompute_view_until_stable ?(max_computes = 100) handle =
      recompute_view handle;
      let computes = ref 1 in
      while Driver.has_after_display_events handle do
        recompute_view handle;
        computes := !computes + 1;
        if !computes >= max_computes
        then failwithf "view not stable after %d recomputations" max_computes ()
      done
    ;;

    let flush_async_and_bonsai
      ?(max_iterations = 100)
      ?(silence_between_frames = false)
      driver
      =
      let open Async_kernel in
      let rec loop i =
        if i = 0
        then
          raise_s [%message [%string "not stable after %{max_iterations#Int} iterations"]];
        if i < max_iterations && not silence_between_frames
        then print_endline "------ between bonsai frame ------";
        let%bind.Eager_deferred () =
          Async_kernel_scheduler.yield_until_no_jobs_remain
            ~may_return_immediately:true
            ()
        in
        recompute_view driver;
        if Driver.has_after_display_events driver
           || Async_kernel_scheduler.num_pending_jobs () > 0
        then loop (i - 1)
        else Deferred.unit
      in
      recompute_view driver;
      loop max_iterations
    ;;

    let generic_show driver ~before ~f =
      let before = before driver in
      Driver.flush driver;
      let _, vdom, after, _ = Driver.result driver in
      Driver.store_view driver after;
      (match am_running_how with
       | `Browser | `Browser_benchmark -> perform_update vdom
       | `Node | `Node_benchmark | `Node_test -> ());
      f before after;
      Driver.trigger_lifecycles driver
    ;;

    let show handle =
      generic_show handle ~before:(Fn.const ()) ~f:(fun () view ->
        print_endline (Lazy.force view))
    ;;

    let show_diff
      ?(location_style = Patdiff_kernel.Format.Location_style.None)
      ?(diff_context = 16)
      handle
      =
      generic_show handle ~before:Driver.last_view ~f:(fun a b ->
        Expect_test_patdiff.print_patdiff
          ~location_style
          ~context:diff_context
          (Lazy.force a)
          (Lazy.force b))
    ;;

    let store_view handle = generic_show handle ~before:(Fn.const ()) ~f:(fun () _ -> ())
    let clock driver = Driver.clock driver

    let advance_clock_by driver =
      Bonsai.Time_source.advance_clock_by (Driver.clock driver)
    ;;

    let advance_clock ~to_ driver =
      Bonsai.Time_source.advance_clock ~to_ (Driver.clock driver)
    ;;

    let do_actions handle actions =
      let _, _, _, inject_action = Driver.result handle in
      let event = actions |> List.map ~f:inject_action |> Effect.sequence in
      Driver.schedule_event handle event
    ;;

    let last_result handle =
      let result, vdom, _, _ = Driver.result handle in
      result, vdom
    ;;

    open Virtual_dom_test_helpers

    let get_element driver ~selector =
      let _, vdom = last_result driver in
      let node = Node_helpers.unsafe_convert_exn vdom in
      Node_helpers.select_first_exn node ~selector
    ;;

    let click_on
      ?extra_event_fields
      ?shift_key_down
      ?alt_key_down
      ?ctrl_key_down
      handle
      ~selector
      =
      let element = get_element handle ~selector in
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
      ~selector
      ~checked
      =
      let element = get_element handle ~selector in
      Node_helpers.User_actions.set_checkbox
        element
        ~checked
        ?extra_event_fields
        ?shift_key_down
        ?alt_key_down
        ?ctrl_key_down
    ;;

    let submit_form ?extra_event_fields handle ~selector =
      let element = get_element handle ~selector in
      Node_helpers.User_actions.submit_form element ?extra_event_fields
    ;;

    let focus ?extra_event_fields handle ~selector =
      let element = get_element handle ~selector in
      Node_helpers.User_actions.focus element ?extra_event_fields
    ;;

    let change ?extra_event_fields handle ~selector ~value =
      let element = get_element handle ~selector in
      Node_helpers.User_actions.change element ~value ?extra_event_fields
    ;;

    let blur ?extra_event_fields ?related_target handle ~selector =
      let element = get_element handle ~selector in
      let related_target =
        match related_target with
        | Some selector -> Some (get_element handle ~selector)
        | None -> None
      in
      Node_helpers.User_actions.blur ?related_target element ?extra_event_fields
    ;;

    let mousemove ?extra_event_fields handle ~selector =
      let element = get_element handle ~selector in
      Node_helpers.User_actions.mousemove element ?extra_event_fields
    ;;

    let mouseenter ?extra_event_fields handle ~selector =
      let element = get_element handle ~selector in
      Node_helpers.User_actions.mouseenter element ?extra_event_fields
    ;;

    let wheel ?extra_event_fields handle ~selector ~delta_y =
      let element = get_element handle ~selector in
      Node_helpers.User_actions.wheel element ~delta_y ?extra_event_fields
    ;;

    let input_text ?extra_event_fields handle ~selector ~text =
      let element = get_element handle ~selector in
      Node_helpers.User_actions.input_text element ~text ?extra_event_fields
    ;;

    let keydown
      ?extra_event_fields
      ?shift_key_down
      ?alt_key_down
      ?ctrl_key_down
      handle
      ~selector
      ~key
      =
      let element = get_element handle ~selector in
      Node_helpers.User_actions.keydown
        ?shift_key_down
        ?alt_key_down
        ?ctrl_key_down
        ?extra_event_fields
        element
        ~key
    ;;

    let trigger_hook handle ~selector ~name type_id arg =
      get_element handle ~selector
      |> Node_helpers.trigger_hook ~type_id ~name ~arg ~f:Fn.id
    ;;

    let trigger_hook_via handle ~selector ~name type_id ~f arg =
      get_element handle ~selector |> Node_helpers.trigger_hook ~type_id ~name ~arg ~f
    ;;

    let get_hook_value handle ~selector ~name type_id =
      get_element handle ~selector |> Node_helpers.get_hook_value ~type_id ~name
    ;;

    module Bulk_size_tracker = struct
      open Bonsai_web_ui_element_size_hooks

      type change =
        { selector : string
        ; width : float
        ; height : float
        }

      let change_sizes handle changes =
        Bulk_size_tracker.For_testing.change_sizes
          (List.map changes ~f:(fun { selector; height; width } ->
             ( get_hook_value
                 handle
                 ~selector
                 ~name:Bulk_size_tracker.For_testing.hook_name
                 Bulk_size_tracker.For_testing.type_id
             , { Bulk_size_tracker.Dimensions.width; height } )))
      ;;
    end

    module Position_tracker = struct
      open Bonsai_web_ui_element_size_hooks

      type change =
        { selector : string
        ; top : int
        ; left : int
        ; width : int
        ; height : int
        }

      let change_positions handle changes =
        Position_tracker.For_testing.change_positions
          (List.map changes ~f:(fun { selector; top; left; height; width } ->
             ( get_hook_value
                 handle
                 ~selector
                 ~name:Position_tracker.For_testing.hook_name
                 Position_tracker.For_testing.type_id
             , { Position_tracker.Position.top; left; height; width } )))
      ;;
    end

    module Drag_and_drop = struct
      let run handle ~name action =
        trigger_hook
          handle
          ~selector:[%string "[data-dnd-name=%{name}]"]
          ~name:"dnd-test-hook"
          Bonsai_web_ui_drag_and_drop.For_testing.type_id
          action
      ;;
    end
  end
end

module Expect_test_config = Bonsai_test.Expect_test_config
