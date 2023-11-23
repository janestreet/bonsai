open! Core
module Private_view = View
open! Bonsai_web
open Bonsai.Let_syntax
module View = Private_view

let path =
  let%map.Computation path_id = Bonsai.path_id in
  path_id, Vdom.Attr.id path_id
;;

open Bonsai_web_ui_form2
open Bonsai_web_ui_form2.Elements

module type Model = Model
module type Stringable_model = Stringable_model

module Selectable_style = Selectable_style

module Conversion = struct
  (* [with_extra_attr] adds an id attr to the [extra_attr] argument of a [Form2.Elements]
     function *)
  let with_extra_attr f extra_attr =
    let%sub path_and_id = path in
    let%sub extra_attr =
      match extra_attr with
      | None ->
        let%arr path_and_id = path_and_id in
        let _, id = path_and_id in
        id
      | Some extra_attr ->
        let%arr extra_attr = extra_attr
        and path_and_id = path_and_id in
        let _, id = path_and_id in
        Vdom.Attr.combine id extra_attr
    in
    let%sub form = f ~extra_attr in
    let%arr form = form
    and path_and_id = path_and_id in
    let path, _ = path_and_id in
    map_view form ~f:(fun view -> View.of_vdom ~unique_key:path view)
  ;;

  (* [with_extra_attrs] adds an id attr to the [extra_attrs] argument of a
     [Form2.Elements] function *)
  let with_extra_attrs f extra_attrs =
    let%sub path_and_id = path in
    let%sub extra_attrs =
      match extra_attrs with
      | None ->
        let%arr path_and_id = path_and_id in
        let _, id = path_and_id in
        [ id ]
      | Some extra_attrs ->
        let%arr extra_attrs = extra_attrs
        and path_and_id = path_and_id in
        let _, id = path_and_id in
        id :: extra_attrs
    in
    let%sub form = f ~extra_attrs in
    let%arr form = form
    and path_and_id = path_and_id in
    let path, _ = path_and_id in
    map_view form ~f:(fun view -> View.of_vdom ~unique_key:path view)
  ;;

  (* [don't_attach_id] does not add an id attr to an underlying [Form2.Elements] function *)
  let don't_attach_id form =
    let%sub path = Bonsai.path_id in
    let%sub form = form in
    let%arr form = form
    and path = path in
    map_view form ~f:(fun view -> View.of_vdom ~unique_key:path view)
  ;;
end

module Non_interactive = struct
  open Non_interactive

  let constant view value = Conversion.don't_attach_id (constant view value)
end

module Textbox = struct
  open Textbox

  let string ?extra_attrs ?placeholder ~allow_updates_when_focused () =
    Conversion.with_extra_attrs
      (fun ~extra_attrs ->
        string ~extra_attrs ?placeholder ~allow_updates_when_focused ())
      extra_attrs
  ;;

  let int ?extra_attrs ?placeholder ~allow_updates_when_focused () =
    Conversion.with_extra_attrs
      (fun ~extra_attrs -> int ~extra_attrs ?placeholder ~allow_updates_when_focused ())
      extra_attrs
  ;;

  let float ?extra_attrs ?placeholder ~allow_updates_when_focused () =
    Conversion.with_extra_attrs
      (fun ~extra_attrs -> float ~extra_attrs ?placeholder ~allow_updates_when_focused ())
      extra_attrs
  ;;

  let sexpable ?extra_attrs ?placeholder ~allow_updates_when_focused model =
    Conversion.with_extra_attrs
      (fun ~extra_attrs ->
        sexpable ~extra_attrs ?placeholder ~allow_updates_when_focused model)
      extra_attrs
  ;;

  let stringable ?extra_attrs ?placeholder ~allow_updates_when_focused model =
    Conversion.with_extra_attrs
      (fun ~extra_attrs ->
        stringable ~extra_attrs ?placeholder ~allow_updates_when_focused model)
      extra_attrs
  ;;
end

module Password = struct
  open Password

  let string ?extra_attrs ?placeholder ~allow_updates_when_focused () =
    Conversion.with_extra_attrs
      (fun ~extra_attrs ->
        string ~extra_attrs ?placeholder ~allow_updates_when_focused ())
      extra_attrs
  ;;

  let stringable ?extra_attrs ?placeholder ~allow_updates_when_focused model =
    Conversion.with_extra_attrs
      (fun ~extra_attrs ->
        stringable ~extra_attrs ?placeholder ~allow_updates_when_focused model)
      extra_attrs
  ;;
end

module Textarea = struct
  open Textarea

  let string ?extra_attrs ?placeholder ~allow_updates_when_focused () =
    Conversion.with_extra_attrs
      (fun ~extra_attrs ->
        string ~extra_attrs ?placeholder ~allow_updates_when_focused ())
      extra_attrs
  ;;

  let int ?extra_attrs ?placeholder ~allow_updates_when_focused () =
    Conversion.with_extra_attrs
      (fun ~extra_attrs -> int ~extra_attrs ?placeholder ~allow_updates_when_focused ())
      extra_attrs
  ;;

  let float ?extra_attrs ?placeholder ~allow_updates_when_focused () =
    Conversion.with_extra_attrs
      (fun ~extra_attrs -> float ~extra_attrs ?placeholder ~allow_updates_when_focused ())
      extra_attrs
  ;;

  let sexpable ?extra_attrs ?placeholder ~allow_updates_when_focused model =
    Conversion.with_extra_attrs
      (fun ~extra_attrs ->
        sexpable ~extra_attrs ?placeholder ~allow_updates_when_focused model)
      extra_attrs
  ;;

  let stringable ?extra_attrs ?placeholder ~allow_updates_when_focused model =
    Conversion.with_extra_attrs
      (fun ~extra_attrs ->
        stringable ~extra_attrs ?placeholder ~allow_updates_when_focused model)
      extra_attrs
  ;;
end

module Checkbox = struct
  open Checkbox

  let bool ?extra_attrs ~default () =
    Conversion.with_extra_attrs
      (fun ~extra_attrs -> bool ~extra_attrs ~default ())
      extra_attrs
  ;;

  let set
    ?style
    ?extra_container_attrs
    ?extra_checkbox_attrs
    ?to_string
    ?layout
    comparator
    values
    =
    Conversion.with_extra_attrs
      (fun ~extra_attrs ->
        set
          ?style
          ~extra_container_attrs:extra_attrs
          ?extra_checkbox_attrs
          ?to_string
          ?layout
          comparator
          values)
      extra_container_attrs
  ;;

  module Private = struct
    open Checkbox.Private

    let make_input ~id ~extra_attrs ~state ~set_state =
      make_input ~extra_attrs:(id :: extra_attrs) ~state ~set_state
    ;;
  end
end

module Toggle = struct
  open Toggle

  let bool ?extra_attr ~default () =
    Conversion.with_extra_attr
      (fun ~extra_attr -> bool ~extra_attr ~default ())
      extra_attr
  ;;
end

module Dropdown = struct
  open Dropdown

  let list_opt ?init ?extra_attrs ?extra_option_attrs ?to_string m ~equal all =
    Conversion.with_extra_attrs
      (fun ~extra_attrs ->
        list_opt ?init ~extra_attrs ?extra_option_attrs ?to_string m ~equal all)
      extra_attrs
  ;;

  let enumerable_opt ?init ?extra_attrs ?extra_option_attrs ?to_string m =
    Conversion.with_extra_attrs
      (fun ~extra_attrs ->
        enumerable_opt ?init ~extra_attrs ?extra_option_attrs ?to_string m)
      extra_attrs
  ;;

  let list ?init ?extra_attrs ?extra_option_attrs ?to_string m ~equal all =
    Conversion.with_extra_attrs
      (fun ~extra_attrs ->
        list ?init ~extra_attrs ?extra_option_attrs ?to_string m ~equal all)
      extra_attrs
  ;;

  let enumerable ?init ?extra_attrs ?extra_option_attrs ?to_string m =
    Conversion.with_extra_attrs
      (fun ~extra_attrs -> enumerable ?init ~extra_attrs ?extra_option_attrs ?to_string m)
      extra_attrs
  ;;

  module Private = struct
    open Dropdown.Private
    module Opt = Opt

    let make_input
      ?to_string
      m
      ~equal
      ~id
      ~include_empty
      ~default_value
      ~state
      ~set_state
      ~extra_attrs
      ~extra_option_attrs
      ~all
      =
      make_input
        ?to_string
        m
        ~equal
        ~include_empty
        ~default_value
        ~state
        ~set_state
        ~extra_attrs:(id :: extra_attrs)
        ~extra_option_attrs
        ~all
    ;;
  end
end

module Typeahead = struct
  open Typeahead

  let single_opt
    ?extra_attrs
    ?placeholder
    ?to_string
    ?to_option_description
    ?handle_unknown_option
    model
    ~equal
    ~all_options
    =
    Conversion.with_extra_attrs
      (fun ~extra_attrs ->
        single_opt
          ~extra_attrs
          ?placeholder
          ?to_string
          ?to_option_description
          ?handle_unknown_option
          model
          ~equal
          ~all_options)
      extra_attrs
  ;;

  let single
    ?extra_attrs
    ?placeholder
    ?to_string
    ?to_option_description
    ?handle_unknown_option
    m
    ~equal
    ~all_options
    =
    Conversion.with_extra_attrs
      (fun ~extra_attrs ->
        single
          ~extra_attrs
          ?placeholder
          ?to_string
          ?to_option_description
          ?handle_unknown_option
          m
          ~equal
          ~all_options)
      extra_attrs
  ;;

  let set
    ?extra_attrs
    ?placeholder
    ?to_string
    ?to_option_description
    ?handle_unknown_option
    ?split
    m
    ~all_options
    =
    Conversion.with_extra_attrs
      (fun ~extra_attrs ->
        set
          ~extra_attrs
          ?placeholder
          ?to_string
          ?to_option_description
          ?handle_unknown_option
          ?split
          m
          ~all_options)
      extra_attrs
  ;;

  let list
    ?extra_attrs
    ?placeholder
    ?to_string
    ?to_option_description
    ?handle_unknown_option
    ?split
    m
    ~all_options
    =
    Conversion.with_extra_attrs
      (fun ~extra_attrs ->
        list
          ~extra_attrs
          ?placeholder
          ?to_string
          ?to_option_description
          ?handle_unknown_option
          ?split
          m
          ~all_options)
      extra_attrs
  ;;
end

module Date_time = struct
  open Date_time
  module Span_unit = Span_unit

  let date_opt ?extra_attrs ~allow_updates_when_focused () =
    Conversion.with_extra_attrs
      (fun ~extra_attrs -> date_opt ~extra_attrs ~allow_updates_when_focused ())
      extra_attrs
  ;;

  let date ?extra_attrs ~allow_updates_when_focused () =
    Conversion.with_extra_attrs
      (fun ~extra_attrs -> date ~extra_attrs ~allow_updates_when_focused ())
      extra_attrs
  ;;

  let time_opt ?extra_attrs ~allow_updates_when_focused () =
    Conversion.with_extra_attrs
      (fun ~extra_attrs -> time_opt ~extra_attrs ~allow_updates_when_focused ())
      extra_attrs
  ;;

  let time ?extra_attrs ~allow_updates_when_focused () =
    Conversion.with_extra_attrs
      (fun ~extra_attrs -> time ~extra_attrs ~allow_updates_when_focused ())
      extra_attrs
  ;;

  let time_span_opt
    ?extra_unit_attrs
    ?extra_amount_attrs
    ?default_unit
    ?default
    ~allow_updates_when_focused
    ()
    =
    Conversion.with_extra_attrs
      (fun ~extra_attrs:extra_amount_attrs ->
        Date_time.time_span_opt
          ?extra_unit_attrs
          ~extra_amount_attrs
          ?default_unit
          ?default
          ~allow_updates_when_focused
          ())
      extra_amount_attrs
  ;;

  let time_span
    ?extra_unit_attrs
    ?extra_amount_attrs
    ?default_unit
    ?default
    ~allow_updates_when_focused
    ()
    =
    Conversion.with_extra_attrs
      (fun ~extra_attrs:extra_amount_attrs ->
        Date_time.time_span
          ?extra_unit_attrs
          ~extra_amount_attrs
          ?default_unit
          ?default
          ~allow_updates_when_focused
          ())
      extra_amount_attrs
  ;;

  let datetime_local_opt ?extra_attrs ~allow_updates_when_focused () =
    Conversion.with_extra_attrs
      (fun ~extra_attrs -> datetime_local_opt ~extra_attrs ~allow_updates_when_focused ())
      extra_attrs
  ;;

  let datetime_local ?extra_attrs ~allow_updates_when_focused () =
    Conversion.with_extra_attrs
      (fun ~extra_attrs -> datetime_local ~extra_attrs ~allow_updates_when_focused ())
      extra_attrs
  ;;

  module Range = struct
    open Date_time.Range

    (* We don't attach ids on date range functions because they're not meaningful: it's
       not clear what the cursor should focus when the label is clicked. *)
    let date_opt ?extra_attr ?allow_equal ~allow_updates_when_focused () =
      Conversion.don't_attach_id
        (date_opt ?extra_attr ?allow_equal ~allow_updates_when_focused ())
    ;;

    let date ?extra_attr ?allow_equal ~allow_updates_when_focused () =
      Conversion.don't_attach_id
        (date ?extra_attr ?allow_equal ~allow_updates_when_focused ())
    ;;

    let time_opt ?extra_attr ?allow_equal ~allow_updates_when_focused () =
      Conversion.don't_attach_id
        (time_opt ?extra_attr ?allow_equal ~allow_updates_when_focused ())
    ;;

    let time ?extra_attr ?allow_equal ~allow_updates_when_focused () =
      Conversion.don't_attach_id
        (time ?extra_attr ?allow_equal ~allow_updates_when_focused ())
    ;;

    let datetime_local_opt ?extra_attr ?allow_equal ~allow_updates_when_focused () =
      Conversion.don't_attach_id
        (datetime_local_opt ?extra_attr ?allow_equal ~allow_updates_when_focused ())
    ;;

    let datetime_local ?extra_attr ?allow_equal ~allow_updates_when_focused () =
      Conversion.don't_attach_id
        (datetime_local ?extra_attr ?allow_equal ~allow_updates_when_focused ())
    ;;
  end
end

module Multiselect = struct
  open Multiselect

  let set
    ?extra_attrs
    ?to_string
    ?default_selection_status
    ~allow_updates_when_focused
    m
    input_list
    =
    Conversion.with_extra_attrs
      (fun ~extra_attrs ->
        set
          ~extra_attrs
          ?to_string
          ?default_selection_status
          ~allow_updates_when_focused
          m
          input_list)
      extra_attrs
  ;;

  let list
    ?extra_attrs
    ?to_string
    ?default_selection_status
    ~allow_updates_when_focused
    m
    input_list
    =
    Conversion.with_extra_attrs
      (fun ~extra_attrs ->
        list
          ~extra_attrs
          ?to_string
          ?default_selection_status
          ~allow_updates_when_focused
          m
          input_list)
      extra_attrs
  ;;
end

module Multiple = struct
  open Multiple

  let stringable_list
    ?extra_input_attr
    ?extra_pill_container_attr
    ?extra_pill_attr
    ?placeholder
    m
    ~equal
    =
    Conversion.with_extra_attr
      (fun ~extra_attr:extra_input_attr ->
        stringable_list
          ~extra_input_attr
          ?extra_pill_container_attr
          ?extra_pill_attr
          ?placeholder
          m
          ~equal)
      extra_input_attr
  ;;

  let extract_add_element_text = function
    | Some value ->
      let%arr value = value in
      Some value
    | None -> Bonsai.const None
  ;;

  let map_list_view
    ?element_group_label
    ?add_element_text
    ?(button_placement = `Indented)
    form
    ~view_item
    =
    let%sub add_element_text = extract_add_element_text add_element_text in
    let%arr form = form
    and add_element_text = add_element_text in
    map_view form ~f:(fun { items; add_element = append } ->
      List.map items ~f:(fun { form = { view; value = _; set = _ }; remove } ->
        View.list_item
          ~view:(view_item view)
          ~remove_item:(Remove_info { remove; element_label = element_group_label }))
      |> View.list
           ~append_item:(Append_info { append; text = add_element_text })
           ~legacy_button_position:button_placement)
  ;;

  let map_nonempty_list_view
    ?element_group_label
    ?add_element_text
    ?(button_placement = `Indented)
    form
    ~view_item
    =
    let%sub add_element_text = extract_add_element_text add_element_text in
    let%arr form = form
    and add_element_text = add_element_text in
    map_view form ~f:(fun { hd = { view = hd_view; _ }; tl; add_element = append } ->
      (* [Remove_view Vdom.Node.none] works but leads to a not quite correct
         result in the end, since the empty delete button becomes an empty <tr> that
         takes up 2 pixels of height. It's not that big of a deal though, since people
         should move to Form2 anyways eventually. *)
      let hd_view =
        View.list_item ~view:(view_item hd_view) ~remove_item:(Remove_view Vdom.Node.none)
      in
      let tl_views =
        List.map tl ~f:(fun { form = { view; _ }; remove } ->
          let element_label =
            let%map.Option element_group_label = element_group_label in
            fun ~delete_button i -> element_group_label ~delete_button (i + Int.one)
          in
          View.list_item
            ~view:(view_item view)
            ~remove_item:(Remove_info { remove; element_label }))
      in
      View.list
        (hd_view :: tl_views)
        ~append_item:(Append_info { append; text = add_element_text })
        ~legacy_button_position:button_placement)
  ;;

  let list
    (type a)
    ?element_group_label
    ?add_element_text
    ?button_placement
    (t : a Form.t Computation.t)
    : a list Form.t Computation.t
    =
    let%sub form = list t in
    map_list_view
      ?element_group_label
      ?add_element_text
      ?button_placement
      form
      ~view_item:Fn.id
  ;;

  let nonempty_list
    (type a)
    ?element_group_label
    ?add_element_text
    ?button_placement
    (t : a Form.t Computation.t)
    : a Nonempty_list.t Form.t Computation.t
    =
    let%sub form = nonempty_list t in
    map_nonempty_list_view
      ?element_group_label
      ?add_element_text
      ?button_placement
      form
      ~view_item:Fn.id
  ;;

  let set ?element_group_label ?add_element_text ?button_placement m form =
    let%sub form = set m form in
    map_list_view
      ?element_group_label
      ?add_element_text
      ?button_placement
      form
      ~view_item:Fn.id
  ;;

  let map ?element_group_label ?add_element_text ?button_placement m ~key ~data =
    let%sub form = map m ~key ~data in
    map_list_view
      ?element_group_label
      ?add_element_text
      ?button_placement
      form
      ~view_item:(fun (key_view, data_view) -> View.tuple [ key_view; data_view ])
  ;;
end

module Number = struct
  open Number

  let int ?extra_attrs ?min ?max ?default ~step ~allow_updates_when_focused () =
    Conversion.with_extra_attrs
      (fun ~extra_attrs ->
        int ~extra_attrs ?min ?max ?default ~step ~allow_updates_when_focused ())
      extra_attrs
  ;;

  let float ?extra_attrs ?min ?max ?default ~step ~allow_updates_when_focused () =
    Conversion.with_extra_attrs
      (fun ~extra_attrs ->
        float ~extra_attrs ?min ?max ?default ~step ~allow_updates_when_focused ())
      extra_attrs
  ;;
end

module Range = struct
  open Range

  let int
    ?extra_attrs
    ?min
    ?max
    ?left_label
    ?right_label
    ?default
    ~step
    ~allow_updates_when_focused
    ()
    =
    Conversion.with_extra_attrs
      (fun ~extra_attrs ->
        int
          ~extra_attrs
          ?min
          ?max
          ?left_label
          ?right_label
          ?default
          ~step
          ~allow_updates_when_focused
          ())
      extra_attrs
  ;;

  let float
    ?extra_attrs
    ?min
    ?max
    ?left_label
    ?right_label
    ?default
    ~step
    ~allow_updates_when_focused
    ()
    =
    Conversion.with_extra_attrs
      (fun ~extra_attrs ->
        float
          ~extra_attrs
          ?min
          ?max
          ?left_label
          ?right_label
          ?default
          ~step
          ~allow_updates_when_focused
          ())
      extra_attrs
  ;;
end

module Radio_buttons = struct
  open Radio_buttons

  let list
    ?style
    ?extra_container_attrs
    ?extra_button_attrs
    ?init
    ?to_string
    m
    ~equal
    ~layout
    all
    =
    Conversion.with_extra_attrs
      (fun ~extra_attrs ->
        list
          ?style
          ~extra_container_attrs:extra_attrs
          ?extra_button_attrs
          ?init
          ?to_string
          m
          ~equal
          ~layout
          all)
      extra_container_attrs
  ;;

  let enumerable
    ?style
    ?extra_container_attrs
    ?extra_button_attrs
    ?init
    ?to_string
    m
    ~layout
    =
    Conversion.with_extra_attrs
      (fun ~extra_attrs ->
        enumerable
          ?style
          ~extra_container_attrs:extra_attrs
          ?extra_button_attrs
          ?init
          ?to_string
          m
          ~layout)
      extra_container_attrs
  ;;
end

module Color_picker = struct
  open Color_picker

  let hex ?extra_attr () =
    Conversion.with_extra_attr (fun ~extra_attr -> hex ~extra_attr ()) extra_attr
  ;;
end

module File_select = struct
  open File_select

  let single_opt ?extra_attrs ?accept () =
    Conversion.with_extra_attrs
      (fun ~extra_attrs -> single_opt ~extra_attrs ?accept ())
      extra_attrs
  ;;

  let single ?extra_attrs ?accept () =
    Conversion.with_extra_attrs
      (fun ~extra_attrs -> single ~extra_attrs ?accept ())
      extra_attrs
  ;;

  let multiple ?extra_attrs ?accept () =
    Conversion.with_extra_attrs
      (fun ~extra_attrs -> multiple ~extra_attrs ?accept ())
      extra_attrs
  ;;
end

module Freeform_multiselect = struct
  open Freeform_multiselect

  let set ?extra_attr ?placeholder ?split () =
    Conversion.with_extra_attr
      (fun ~extra_attr -> set ~extra_attr ?placeholder ?split ())
      extra_attr
  ;;

  let list ?extra_attr ?placeholder ?split () =
    Conversion.with_extra_attr
      (fun ~extra_attr -> list ~extra_attr ?placeholder ?split ())
      extra_attr
  ;;
end

module Rank = struct
  open Rank

  let list
    key
    ?enable_debug_overlay
    ?extra_item_attrs
    ?left
    ?right
    ?empty_list_placeholder
    ?default_item_height
    render
    =
    Conversion.don't_attach_id
      (list
         key
         ?enable_debug_overlay
         ?extra_item_attrs
         ?left
         ?right
         ?empty_list_placeholder
         ?default_item_height
         render)
  ;;
end

module Query_box = struct
  open Query_box

  let create_opt
    key
    ?initial_query
    ?max_visible_items
    ?suggestion_list_kind
    ?selected_item_attr
    ?extra_list_container_attr
    ?extra_input_attr
    ?extra_attr
    ~selection_to_string
    ~f
    ()
    =
    Conversion.don't_attach_id
      (create_opt
         key
         ?initial_query
         ?max_visible_items
         ?suggestion_list_kind
         ?selected_item_attr
         ?extra_list_container_attr
         ?extra_input_attr
         ?extra_attr
         ~selection_to_string
         ~f
         ())
  ;;

  let create
    key
    ?initial_query
    ?max_visible_items
    ?suggestion_list_kind
    ?selected_item_attr
    ?extra_list_container_attr
    ?extra_input_attr
    ?extra_attr
    ~selection_to_string
    ~f
    ()
    =
    Conversion.don't_attach_id
      (create
         key
         ?initial_query
         ?max_visible_items
         ?suggestion_list_kind
         ?selected_item_attr
         ?extra_list_container_attr
         ?extra_input_attr
         ?extra_attr
         ~selection_to_string
         ~f
         ())
  ;;

  let single_opt
    ?extra_attrs
    ?to_string
    ?to_option_description
    ?selected_item_attr
    ?extra_list_container_attr
    ?handle_unknown_option
    m
    ~all_options
    =
    Conversion.with_extra_attrs
      (fun ~extra_attrs ->
        single_opt
          ~extra_attrs
          ?to_string
          ?to_option_description
          ?selected_item_attr
          ?extra_list_container_attr
          ?handle_unknown_option
          m
          ~all_options)
      extra_attrs
  ;;

  let single
    ?extra_attrs
    ?to_string
    ?to_option_description
    ?selected_item_attr
    ?extra_list_container_attr
    ?handle_unknown_option
    m
    ~all_options
    =
    Conversion.with_extra_attrs
      (fun ~extra_attrs ->
        single
          ~extra_attrs
          ?to_string
          ?to_option_description
          ?selected_item_attr
          ?extra_list_container_attr
          ?handle_unknown_option
          m
          ~all_options)
      extra_attrs
  ;;
end

module Optional = struct
  open Optional

  let dropdown (type a) ?some_label ?none_label (form : a Form.t Computation.t) =
    let%sub form = dropdown ?some_label ?none_label form in
    let%arr form = form in
    map_view form ~f:(fun (clause_selector, sub_form) ->
      View.variant
        ~clause_selector
        ~selected_clause:
          (Option.map sub_form ~f:(fun view ->
             { View.clause_name = "Some"; clause_view = view })))
  ;;
end
