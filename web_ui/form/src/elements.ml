open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Extendy = Bonsai_web_ui_extendy
module Selectable_style = Vdom_input_widgets.Selectable_style

module type Stringable_model = sig
  type t

  include Bonsai.Model with type t := t
  include Stringable with type t := t
end

module Style =
  [%css.raw
    {|
  .invalid_text_box {
    outline: none;
    border: 2px solid red;
    border-radius: 2px;
  } |}]

let path =
  let%map.Computation path_id = Bonsai.path_id in
  path_id, Vdom.Attr.id path_id
;;

module Basic_stateful = struct
  let make state ~view =
    let%sub path_and_id = path in
    let%sub state_and_set_state = state in
    let%arr view = view
    and path_and_id = path_and_id
    and state_and_set_state = state_and_set_state in
    let path, id = path_and_id in
    let state, set_state = state_and_set_state in
    let view = view ~id ~state ~set_state in
    Form.Expert.create ~value:(Ok state) ~view:(View.of_vdom ~id:path view) ~set:set_state
  ;;
end

let computation_map a ~f =
  let%sub a = a in
  return @@ Value.map ~f a
;;

let optional_to_required =
  Form.project'
    ~parse:(function
      | Some a -> Ok a
      | None -> Error (Error.of_string "a value is required"))
    ~unparse:(fun a -> Some a)
;;

module Textbox = struct
  let string ?(extra_attrs = Value.return []) ?placeholder () =
    let view =
      let%map extra_attrs = extra_attrs in
      fun ~id ~state ~set_state ->
        Vdom_input_widgets.Entry.text
          ?placeholder
          ~extra_attrs:([ id ] @ extra_attrs)
          ~value:(Some state)
          ~on_input:(function
            | Some s -> set_state s
            | None -> set_state "")
          ()
    in
    Basic_stateful.make (Bonsai.state (module String) ~default_model:"") ~view
  ;;

  let int ?extra_attrs ?placeholder here =
    computation_map
      (string ?extra_attrs ?placeholder here)
      ~f:
        (Form.project'
           ~parse:(fun input ->
             match Int.of_string input with
             | exception _ -> Or_error.errorf "Expecting an integer"
             | x -> Or_error.return x)
           ~unparse:Int.to_string_hum)
  ;;

  let float ?extra_attrs ?placeholder () =
    computation_map
      (string ?extra_attrs ?placeholder ())
      ~f:(Form.project ~parse_exn:Float.of_string ~unparse:Float.to_string_hum)
  ;;

  let sexpable (type t) ?extra_attrs ?placeholder (module M : Sexpable with type t = t) =
    let parse_exn s = s |> Sexp.of_string |> M.t_of_sexp in
    let unparse t = t |> M.sexp_of_t |> Sexp.to_string_hum in
    computation_map
      (string ?extra_attrs ?placeholder ())
      ~f:(Form.project ~parse_exn ~unparse)
  ;;

  let stringable
        (type t)
        ?extra_attrs
        ?placeholder
        (module M : Stringable with type t = t)
    =
    computation_map
      (string ?extra_attrs ?placeholder ())
      ~f:(Form.project ~parse_exn:M.of_string ~unparse:M.to_string)
  ;;
end

module Textarea = struct
  let string ?(extra_attrs = Value.return []) ?placeholder () =
    let view =
      let%map extra_attrs = extra_attrs in
      fun ~id ~state ~set_state ->
        Vdom_input_widgets.Entry.text_area
          ?placeholder
          ~extra_attrs:(id :: extra_attrs)
          ~value:state
          ~on_input:set_state
          ()
    in
    Basic_stateful.make (Bonsai.state (module String) ~default_model:"") ~view
  ;;

  let int ?extra_attrs ?placeholder () =
    computation_map
      (string ?extra_attrs ?placeholder ())
      ~f:(Form.project ~parse_exn:Int.of_string ~unparse:Int.to_string_hum)
  ;;

  let float ?extra_attrs ?placeholder () =
    computation_map
      (string ?extra_attrs ?placeholder ())
      ~f:(Form.project ~parse_exn:Float.of_string ~unparse:Float.to_string_hum)
  ;;

  let sexpable (type t) ?extra_attrs ?placeholder (module M : Sexpable with type t = t) =
    let parse_exn s = s |> Sexp.of_string |> M.t_of_sexp in
    let unparse t = t |> M.sexp_of_t |> Sexp.to_string_hum in
    computation_map
      (string ?extra_attrs ?placeholder ())
      ~f:(Form.project ~parse_exn ~unparse)
  ;;

  let stringable
        (type t)
        ?extra_attrs
        ?placeholder
        (module M : Stringable with type t = t)
    =
    computation_map
      (string ?extra_attrs ?placeholder ())
      ~f:(Form.project ~parse_exn:M.of_string ~unparse:M.to_string)
  ;;
end

let sexp_to_pretty_string sexp_of_t t =
  t
  |> sexp_of_t
  |> Sexp.to_string_mach
  |> String.lowercase
  |> String.map ~f:(function
    | '(' | ')' | '-' | '_' -> ' '
    | o -> o)
;;

module Checkbox = struct
  let make_input ~extra_attrs ~state ~set_state =
    Vdom.Node.input
      ~attr:
        (Vdom.Attr.many_without_merge
           ([ Vdom.Attr.style (Css_gen.margin_left (`Px 0))
            ; Vdom.Attr.type_ "checkbox"
            ; Vdom.Attr.on_click (fun evt ->
                (* try to get the actual state of the checkbox, but if
                   that doesn't work, assume that clicking on the
                   element toggled the state. *)
                let checked =
                  let open Option.Let_syntax in
                  let open Js_of_ocaml in
                  let%bind target = evt##.target |> Js.Opt.to_option in
                  let%bind coerced =
                    Dom_html.CoerceTo.input target |> Js.Opt.to_option
                  in
                  return (Js.to_bool coerced##.checked)
                in
                match checked with
                | Some bool -> set_state bool
                | None -> set_state (not state))
            ; Vdom.Attr.bool_property "checked" state
            ]
            @ extra_attrs))
      ()
  ;;

  let checkbox ?(extra_attrs = Value.return []) default_model =
    let view =
      let%map extra_attrs = extra_attrs in
      fun ~id ~state ~set_state ->
        make_input ~extra_attrs:(id :: extra_attrs) ~state ~set_state
    in
    Basic_stateful.make (Bonsai.state (module Bool) ~default_model) ~view
  ;;

  let bool ?extra_attrs ~default () = checkbox ?extra_attrs default

  let set
        (type a cmp)
        ?(style = Value.return Selectable_style.Native)
        ?(extra_attrs = Value.return [])
        ?to_string
        (module M : Bonsai.Comparator with type t = a and type comparator_witness = cmp)
        values
    : (a, cmp) Set.t Form.t Computation.t
    =
    let to_string =
      Option.value to_string ~default:(sexp_to_pretty_string [%sexp_of: M.t])
    in
    let module M = struct
      include M
      include Comparable.Make_using_comparator (M)

      let to_string = to_string
    end
    in
    let view =
      let%map values = values
      and style = style
      and extra_attrs = extra_attrs in
      fun ~id ~state ~set_state ->
        Vdom_input_widgets.Checklist.of_values
          ~extra_attrs:(id :: extra_attrs)
          (module M)
          values
          ~style
          ~is_checked:(Set.mem state)
          ~on_toggle:(fun item ->
            set_state
            @@ if Set.mem state item then Set.remove state item else Set.add state item)
    in
    Basic_stateful.make (Bonsai.state (module M.Set) ~default_model:M.Set.empty) ~view
  ;;
end

module Toggle = struct
  (* Most of this css is from https://www.w3schools.com/howto/howto_css_switch.asp *)

  module Style =
    [%css.raw
      {|
.toggle {
  position: relative;
  display: inline-block;
  width: 40px;
  height: 22px;
}

.invisible {
  opacity: 0;
  width: 0;
  height: 0;
}

.slider {
  position: absolute;
  cursor: pointer;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  background-color: #ccc; /* change */
  transition: 0.2s;
  border-radius: 34px;
}

.slider::before {
  position: absolute;
  content: "";
  height: 18px;
  width: 18px;
  left: 2px;
  bottom: 2px;
  border-radius: 50%;
  background-color: white;
  transition: 0.2s;
}

.invisible:checked + .slider {
  background-color: #2196F3;
}

.invisible:focused + .slider {
  box-shadow: 0 0 1px #2196F3;
}

.invisible:checked + .slider::before {
  transform: translateX(18px);
}
|}]

  let bool ?(extra_attr = Value.return Vdom.Attr.empty) ~default () =
    let view =
      let%map extra_attr = extra_attr in
      fun ~id ~state ~set_state ->
        let checkbox =
          Checkbox.make_input
            ~extra_attrs:
              [ Vdom.Attr.many [ Vdom.Attr.class_ Style.invisible; extra_attr ] ]
            ~state
            ~set_state
        in
        let slider = Vdom.Node.span ~attr:(Vdom.Attr.class_ Style.slider) [] in
        Vdom.Node.label
          ~attr:(Vdom.Attr.many [ Vdom.Attr.class_ Style.toggle; id ])
          [ checkbox; slider ]
    in
    Basic_stateful.make (Bonsai.state (module Bool) ~default_model:default) ~view
  ;;
end

module Dropdown = struct
  let impl
        (type t)
        ?to_string
        ?(extra_attrs = Value.return [])
        (module E : Bonsai.Model with type t = t)
        all
        ~include_empty
        ~init
    =
    let module E = struct
      include E

      let to_string =
        Option.value to_string ~default:(sexp_to_pretty_string [%sexp_of: t])
      ;;
    end
    in
    let module Opt = struct
      type t =
        | Uninitialized
        | Explicitly_none
        | Set of E.t
      [@@deriving sexp, equal]

      let to_option t =
        match t with
        | Uninitialized | Explicitly_none -> None
        | Set v -> Some v
      ;;

      let value t = Option.value (to_option t)
    end
    in
    let default_value =
      match init with
      | `Empty -> Value.return None
      | `First_item -> all >>| List.hd
      | `This item -> item >>| Option.some
      | `Const item -> Value.return (Some item)
    in
    let%sub path, id = path in
    let%sub state, set_state = Bonsai.state (module Opt) ~default_model:Uninitialized in
    let%arr id = id
    and path = path
    and state = state
    and set_state = set_state
    and all = all
    and extra_attrs = extra_attrs
    and default_value = default_value in
    let view =
      let maker =
        match include_empty, default_value with
        | true, _ | false, None ->
          let selected =
            match state with
            | Uninitialized -> default_value
            | Explicitly_none -> None
            | Set v -> Some v
          in
          Vdom_input_widgets.Dropdown.of_values_opt ~selected ~on_change:(function
            | None -> set_state Explicitly_none
            | Some v -> set_state (Set v))
        | false, Some default ->
          Vdom_input_widgets.Dropdown.of_values
            ~selected:(Opt.value state ~default)
            ~on_change:(fun a -> set_state (Set a))
      in
      let extra_attrs =
        [ id; Vdom.Attr.style (Css_gen.width (`Percent (Percent.of_mult 1.0))) ]
        @ extra_attrs
      in
      maker (module E) all ~extra_attrs
    in
    let view = View.of_vdom ~id:path view in
    let value =
      match state, default_value with
      | Uninitialized, Some default_value -> Some default_value
      | _ -> Opt.to_option state
    in
    Form.Expert.create ~value:(Ok value) ~view ~set:(function
      | None -> set_state Explicitly_none
      | Some v -> set_state (Set v))
  ;;

  let list_opt
        (type t)
        ?(init = `Empty)
        ?extra_attrs
        ?to_string
        (module E : Bonsai.Model with type t = t)
        all
    =
    impl ?to_string ?extra_attrs (module E) all ~include_empty:true ~init
  ;;

  let enumerable_opt
        (type t)
        ?(init = `Empty)
        ?extra_attrs
        ?to_string
        (module E : Bonsai.Enum with type t = t)
    =
    impl ?to_string ?extra_attrs (module E) (Value.return E.all) ~include_empty:true ~init
  ;;

  let include_empty_from_init = function
    | `Empty -> true
    | `First_item | `This _ | `Const -> false
  ;;

  let list ?(init = `First_item) ?extra_attrs ?to_string m all =
    computation_map
      (impl
         ?to_string
         ?extra_attrs
         m
         all
         ~init
         ~include_empty:(include_empty_from_init init))
      ~f:optional_to_required
  ;;

  let enumerable
        (type t)
        ?(init = `First_item)
        ?extra_attrs
        ?to_string
        (module E : Bonsai.Enum with type t = t)
    =
    computation_map
      (impl
         ?extra_attrs
         ?to_string
         (module E)
         (Value.return E.all)
         ~init
         ~include_empty:(include_empty_from_init init))
      ~f:optional_to_required
  ;;
end

module Typeahead = struct
  let single_opt ?(extra_attrs = Value.return []) ?placeholder ?to_string m ~all_options =
    let%sub path, id = path in
    let extra_attrs =
      let%map id = id
      and extra_attrs = extra_attrs in
      id :: extra_attrs
    in
    let%sub typeahead =
      Bonsai_web_ui_typeahead.Typeahead.create
        ?placeholder
        ?to_string
        m
        ~all_options
        ~extra_attrs
    in
    let%arr value, view, set = typeahead
    and path = path in
    Form.Expert.create ~value:(Ok value) ~view:(View.of_vdom ~id:path view) ~set
  ;;

  let single ?extra_attrs ?placeholder ?to_string m ~all_options =
    computation_map
      (single_opt ?extra_attrs ?placeholder ?to_string m ~all_options)
      ~f:optional_to_required
  ;;

  let set ?(extra_attrs = Value.return []) ?placeholder ?to_string ?split m ~all_options =
    let%sub path, id = path in
    let extra_attrs =
      let%map id = id
      and extra_attrs = extra_attrs in
      id :: extra_attrs
    in
    let%sub typeahead =
      Bonsai_web_ui_typeahead.Typeahead.create_multi
        ?placeholder
        ?to_string
        ?split
        m
        ~extra_attrs
        ~all_options
    in
    let%arr value, view, set = typeahead
    and path = path in
    Form.Expert.create ~value:(Ok value) ~view:(View.of_vdom ~id:path view) ~set
  ;;

  let list
        (type a cmp)
        ?extra_attrs
        ?placeholder
        ?to_string
        ?split
        (module M : Bonsai.Comparator with type t = a and type comparator_witness = cmp)
        ~all_options
    =
    computation_map
      (set ?extra_attrs ?placeholder ?to_string ?split (module M) ~all_options)
      ~f:(Form.project ~parse_exn:Set.to_list ~unparse:(Set.of_list (module M)))
  ;;
end

module Date_time = struct
  let date_opt ?(extra_attrs = Value.return []) () =
    let view =
      let%map extra_attrs = extra_attrs in
      fun ~id ~state ~set_state ->
        Vdom_input_widgets.Entry.date
          ~extra_attrs:(id :: extra_attrs)
          ~value:state
          ~on_input:set_state
          ()
    in
    Basic_stateful.make (Bonsai.state_opt (module Date)) ~view
  ;;

  let date ?extra_attrs () =
    computation_map (date_opt ?extra_attrs ()) ~f:optional_to_required
  ;;

  let time_opt ?(extra_attrs = Value.return []) () =
    let view =
      let%map extra_attrs = extra_attrs in
      fun ~id ~state ~set_state ->
        Vdom_input_widgets.Entry.time
          ~extra_attrs:(id :: extra_attrs)
          ~value:state
          ~on_input:set_state
          ()
    in
    Basic_stateful.make (Bonsai.state_opt (module Time_ns.Ofday)) ~view
  ;;

  let time ?extra_attrs () =
    computation_map (time_opt ?extra_attrs ()) ~f:optional_to_required
  ;;

  let datetime_local_opt ?(extra_attrs = Value.return []) () =
    let view =
      let%map extra_attrs = extra_attrs in
      fun ~id ~state ~set_state ->
        Vdom_input_widgets.Entry.datetime_local
          ~extra_attrs:(id :: extra_attrs)
          ~value:state
          ~on_input:set_state
          ()
    in
    let module Time_ns = struct
      include Time_ns.Stable.Alternate_sexp.V1

      let equal = Time_ns.equal
    end
    in
    Basic_stateful.make (Bonsai.state_opt (module Time_ns)) ~view
  ;;

  let datetime_local ?extra_attrs () =
    computation_map (datetime_local_opt ?extra_attrs ()) ~f:optional_to_required
  ;;
end

module Multiselect = struct
  let set
        (type a cmp)
        ?(extra_attrs = Value.return [])
        ?to_string
        (module M : Bonsai.Comparator with type t = a and type comparator_witness = cmp)
        input_list
    =
    let module Item = struct
      include M
      include Comparable.Make_using_comparator (M)

      let to_string =
        Option.value to_string ~default:(sexp_to_pretty_string [%sexp_of: t])
      ;;
    end
    in
    let module Single_factor = Bonsai_web_ui_multi_select.Make (Item) in
    let input_set = input_list >>| Item.Set.of_list in
    let%sub path, _id = path in
    let extra_row_attrs ~is_focused =
      if is_focused
      then
        `RGBA (Css_gen.Color.RGBA.create () ~r:0 ~g:0 ~b:0 ~a:(Percent.of_mult 0.3))
        |> Css_gen.background_color
        |> Vdom.Attr.style
      else Vdom.Attr.empty
    in
    let view_config =
      let%map path = path in
      { Single_factor.View_config.header = Vdom.Node.none
      ; extra_row_attrs = Some extra_row_attrs
      ; autofocus_search_box = false
      ; search_box_id = Some path
      }
    in
    let%sub single_factor_result = Single_factor.bonsai ~view_config input_set in
    let%arr { Single_factor.Result.view; inject; selected_items; key_handler; _ } =
      single_factor_result
    and path = path
    and extra_attrs = extra_attrs in
    let set_state set =
      inject
        (Single_factor.Action.Set_all_selection_statuses
           (Map.of_key_set set ~f:(Fn.const Single_factor.Selection_status.Selected)))
    in
    let on_keydown =
      Vdom.Attr.on_keydown
        (Vdom_keyboard.Keyboard_event_handler.handle_or_ignore_event key_handler)
    in
    let view =
      Vdom.Node.div
        ~attr:(Vdom.Attr.many_without_merge (on_keydown :: extra_attrs))
        [ view ]
    in
    Form.Expert.create
      ~value:(Ok selected_items)
      ~view:(View.of_vdom ~id:path view)
      ~set:set_state
  ;;

  let list
        (type a cmp)
        ?extra_attrs
        ?to_string
        (module M : Bonsai.Comparator with type t = a and type comparator_witness = cmp)
        input_list
    =
    computation_map
      (set ?extra_attrs ?to_string (module M) input_list)
      ~f:(Form.project ~parse_exn:Set.to_list ~unparse:(Set.of_list (module M)))
  ;;
end

let list_rev_map2 a b ~f =
  let rec loop a b acc =
    match a, b with
    | x :: xs, y :: ys -> loop xs ys (f x y :: acc)
    | [], [] -> acc
    | _ ->
      eprint_s [%message "BUG! lists of unequal lengths" [%here]];
      acc
  in
  loop a b []
;;

module Multiple = struct
  let stringable_list
        (type a)
        ?(extra_input_attr = Value.return Vdom.Attr.empty)
        ?(extra_pill_container_attr = Value.return Vdom.Attr.empty)
        ?(extra_pill_attr = Value.return Vdom.Attr.empty)
        ?(placeholder = "")
        (module M : Stringable_model with type t = a)
    =
    let module M_list = struct
      type t = M.t list [@@deriving equal, sexp]
    end
    in
    let%sub invalid, inject_invalid = Bonsai.state (module Bool) ~default_model:false in
    let%sub selected_options, inject_selected_options =
      Bonsai.state (module M_list) ~default_model:[]
    in
    let%sub state, set_state = Bonsai.state (module String) ~default_model:"" in
    let%sub path, _ = path in
    let%sub pills =
      Bonsai_web_ui_common_components.Pills.of_list
        ~extra_container_attr:extra_pill_container_attr
        ~extra_pill_attr
        ~to_string:M.to_string
        ~inject_selected_options
        selected_options
    in
    let%arr invalid = invalid
    and inject_invalid = inject_invalid
    and selected_options = selected_options
    and inject_selected_options = inject_selected_options
    and state = state
    and set_state = set_state
    and pills = pills
    and extra_input_attr = extra_input_attr
    and path = path in
    let placeholder_ = placeholder in
    let handle_keydown event =
      match Js_of_ocaml.Dom_html.Keyboard_code.of_event event with
      | Enter ->
        (match Option.try_with (fun () -> M.of_string state) with
         | None -> Effect.Many [ Effect.Prevent_default; inject_invalid true ]
         | Some value ->
           Effect.Many
             [ Effect.Prevent_default
             ; inject_selected_options (value :: selected_options)
             ; set_state ""
             ])
      | _ -> Effect.Ignore
    in
    let invalid_attr =
      if invalid then Vdom.Attr.class_ Style.invalid_text_box else Vdom.Attr.empty
    in
    let input =
      Vdom.Node.input
        ~attr:
          Vdom.Attr.(
            extra_input_attr
            @ invalid_attr
            @ placeholder placeholder_
            @ value_prop state
            @ on_input (fun _ input ->
              Effect.Many [ inject_invalid false; set_state input ])
            @ on_keydown handle_keydown)
        ()
    in
    let view = Vdom.Node.div [ input; pills ] in
    Form.Expert.create
      ~value:(Ok selected_options)
      ~view:(View.of_vdom ~id:path view)
      ~set:inject_selected_options
  ;;

  let list
        (type a)
        ?element_group_label
        ?(add_element_text = Bonsai.Value.return "Add new element")
        ?(button_placement = `Indented)
        (t : a Form.t Computation.t)
    : a list Form.t Computation.t
    =
    let%sub form, _ =
      Bonsai.wrap
        (module Unit)
        ~default_model:()
        ~apply_action:(fun ~inject:_ ~schedule_event (_, forms) () list_of_values ->
          list_rev_map2 (Map.data forms) list_of_values ~f:(fun form value ->
            Form.set form value)
          |> Ui_effect.Many
          |> schedule_event)
        ~f:(fun (_ : unit Value.t) inject_outer ->
          let%sub extendy = Extendy.component t in
          let%sub path, _id = path in
          let%arr { Extendy.contents; append; set_length; remove } = extendy
          and inject_outer = inject_outer
          and path = path
          and add_element_text = add_element_text in
          let elements =
            contents
            |> Map.to_alist
            |> List.mapi ~f:(fun i (key, form) ->
              let delete_button =
                Vdom.Node.button
                  ~attr:
                    (Vdom.Attr.many_without_merge
                       [ Vdom.Attr.type_ "button"
                       ; Vdom.Attr.style
                           Css_gen.(
                             border ~style:`None ()
                             @> create ~field:"cursor" ~value:"pointer"
                             @> color (`Name "blue")
                             @> create ~field:"background" ~value:"none")
                       ; Vdom.Attr.on_click (fun _ -> remove key)
                       ])
                  [ Vdom.Node.text "[ remove ]" ]
              in
              let label =
                match element_group_label with
                | Some label -> label ~delete_button i (Form.value form)
                | None -> Vdom.Node.div [ Vdom.Node.textf "%d - " i; delete_button ]
              in
              View.group label (Form.view form))
          in
          let button =
            match button_placement with
            | `Indented ->
              View.Group
                { label =
                    [ Vdom.Node.text add_element_text ]
                    |> Vdom.Node.button ~attr:(Vdom.Attr.on_click (fun _ -> append))
                    |> Some
                ; tooltip = None
                ; view = Empty
                ; error = None
                }
            | `Inline ->
              View.Row
                { label = None
                ; tooltip = None
                ; form =
                    Vdom.Node.button
                      ~attr:(Vdom.Attr.on_click (fun _ -> append))
                      [ Vdom.Node.text add_element_text ]
                ; id = path
                ; error = None
                }
          in
          let view = List.append elements [ button ] in
          let view = View.List view in
          let value =
            contents |> Map.data |> List.map ~f:Form.value |> Or_error.combine_errors
          in
          let set (list : a list) =
            Vdom.Effect.Many [ set_length (List.length list); inject_outer list ]
          in
          Form.Expert.create ~value ~view ~set, contents)
    in
    return form
  ;;

  let set
        (type a cmp)
        ?element_group_label
        ?add_element_text
        ?button_placement
        (module M : Bonsai.Comparator with type t = a and type comparator_witness = cmp)
        form
    =
    computation_map
      (list ?button_placement ?element_group_label ?add_element_text form)
      ~f:(Form.project ~parse_exn:(Set.of_list (module M)) ~unparse:Set.to_list)
  ;;

  let map
        (type a cmp)
        ?element_group_label
        ?add_element_text
        ?button_placement
        (module M : Bonsai.Comparator with type t = a and type comparator_witness = cmp)
        ~key
        ~data
    =
    let both =
      let%sub key_form = key in
      let%sub value_form = data in
      return @@ Value.map2 key_form value_form ~f:Form.both
    in
    computation_map
      (list ?button_placement ?element_group_label ?add_element_text both)
      ~f:(Form.project ~parse_exn:(Map.of_alist_exn (module M)) ~unparse:Map.to_alist)
  ;;
end

module Number_input_type = struct
  type t =
    | Number
    | Range
end

module type Number_input_specification = sig
  type t [@@deriving sexp_of]

  include Bonsai.Model with type t := t
  include Stringable.S with type t := t

  val min : t option
  val max : t option
  val step : t
  val default : t
  val compare : t -> t -> int
  val to_float : t -> float
end

module Make_number (M : sig
    val input_type : Number_input_type.t
  end) =
struct
  let number_input
        (type a)
        ?(extra_attrs = Value.return [])
        (module S : Number_input_specification with type t = a)
    =
    let compare_opt a b =
      match b with
      | Some b -> S.compare a b
      | None -> 0
    in
    let ( < ) a b = compare_opt a b < 0 in
    let ( > ) a b = compare_opt a b > 0 in
    let view =
      let min = Option.map S.min ~f:(Fn.compose Vdom.Attr.min S.to_float) in
      let max = Option.map S.max ~f:(Fn.compose Vdom.Attr.max S.to_float) in
      let%map extra_attrs = extra_attrs in
      fun ~id ~state ~set_state ->
        let input_widget =
          match M.input_type with
          | Number_input_type.Number -> Vdom_input_widgets.Entry.number
          | Range -> Vdom_input_widgets.Entry.range
        in
        input_widget
          (module S)
          ~extra_attrs:
            (List.concat [ [ id ]; List.filter_map [ min; max ] ~f:Fn.id; extra_attrs ])
          ~value:(Some state)
          ~step:(S.to_float S.step)
          ~on_input:(function
            | Some s -> set_state s
            | None -> set_state S.default)
    in
    let%sub number_input =
      Basic_stateful.make (Bonsai.state (module S) ~default_model:S.default) ~view
    in
    let%arr number_input = number_input in
    Form.validate number_input ~f:(fun value ->
      if value < S.min
      then
        Or_error.error_s
          [%message (value : S.t) "lower than allowed threshold" (S.min : S.t option)]
      else if value > S.max
      then
        Or_error.error_s
          [%message (value : S.t) "higher than allowed threshold" (S.max : S.t option)]
      else Ok ())
  ;;

  let int ?extra_attrs ?min:min_ ?max:max_ ~default ~step () =
    let module Specification = struct
      include Int

      let min = min_
      let max = max_
      let step = step
      let default = default
    end
    in
    number_input ?extra_attrs (module Specification)
  ;;

  let float ?extra_attrs ?min:min_ ?max:max_ ~default ~step () =
    let module Specification = struct
      include Float

      (* We can't just use the default Stringable interface provided by Float, so we
         overwrite it with a custom one. For more information, see
         [Vdom_input_widgets.Entry.number]. *)
      include Vdom_input_widgets.Decimal

      let min = min_
      let max = max_
      let default = default
      let step = step
    end
    in
    number_input ?extra_attrs (module Specification)
  ;;
end

module Number = Make_number (struct
    let input_type = Number_input_type.Number
  end)

module Range = Make_number (struct
    let input_type = Number_input_type.Range
  end)

module Radio_buttons = struct
  let list
        (type t)
        ?(style = Value.return Selectable_style.Native)
        ?(extra_attrs = Value.return [])
        ?to_string
        (module E : Bonsai.Model with type t = t)
        ~layout
        all
    =
    let module E = struct
      include E

      let to_string (item : E.t) =
        match to_string with
        | Some f -> f item
        | None -> sexp_to_pretty_string E.sexp_of_t item
      ;;
    end
    in
    let%sub path, _ = path in
    let view =
      let%map all = all
      and style = style
      and extra_attrs = extra_attrs
      and path = path in
      fun ~id ~state ~set_state ->
        let node_fun =
          match layout with
          | `Vertical -> Vdom_input_widgets.Radio_buttons.of_values ~style
          | `Horizontal -> Vdom_input_widgets.Radio_buttons.of_values_horizontal ~style
        in
        node_fun
          ~extra_attrs:(id :: extra_attrs)
          (module E)
          ~on_click:(fun value -> set_state (Some value))
          ~selected:state
          ~name:path
          all
    in
    Basic_stateful.make (Bonsai.state_opt (module E)) ~view
    |> computation_map ~f:optional_to_required
  ;;

  let enumerable
        (type t)
        ?style
        ?extra_attrs
        ?to_string
        (module E : Bonsai.Enum with type t = t)
        ~layout
    =
    list ?style ?extra_attrs ?to_string (module E) ~layout (Value.return E.all)
  ;;
end

module Color_picker = struct
  let hex ?(extra_attr = Value.return Vdom.Attr.empty) () =
    let view =
      let%map extra_attr = extra_attr in
      fun ~id:id_ ~state ~set_state ->
        Vdom_input_widgets.Entry.color_picker
          ~extra_attr:Vdom.Attr.(id_ @ extra_attr)
          ~value:state
          ~on_input:set_state
          ()
    in
    Basic_stateful.make
      (Bonsai.state
         ~default_model:(`Hex "#000000")
         (module struct
           type t = [ `Hex of string ] [@@deriving equal, sexp]
         end))
      ~view
  ;;
end

module File_select = struct
  module File = struct
    type t = Bonsai_web_ui_file.t [@@deriving sexp_of]

    let equal = phys_equal
    let t_of_sexp = opaque_of_sexp
  end

  let single_opt ?(extra_attrs = Value.return []) ?accept () =
    let view =
      let%map extra_attrs = extra_attrs in
      fun ~id ~state:_ ~set_state ->
        Vdom_input_widgets.File_select.single
          ?accept
          ~extra_attrs:(id :: extra_attrs)
          ~on_input:(fun file ->
            set_state (Option.map file ~f:Bonsai_web_ui_file_from_web_file.create))
          ()
    in
    Basic_stateful.make (Bonsai.state_opt (module File)) ~view
  ;;

  let single ?(extra_attrs = Value.return []) ?accept () =
    Bonsai.Computation.map (single_opt ~extra_attrs ?accept ()) ~f:optional_to_required
  ;;

  let multiple ?(extra_attrs = Value.return []) ?accept () =
    let view =
      let%map extra_attrs = extra_attrs in
      fun ~id ~state:_ ~set_state ->
        Vdom_input_widgets.File_select.list
          ?accept
          ~extra_attrs:(id :: extra_attrs)
          ~on_input:(fun files ->
            let files =
              List.map files ~f:(fun file ->
                let file = Bonsai_web_ui_file_from_web_file.create file in
                Bonsai_web_ui_file.filename file, file)
              |> Filename.Map.of_alist_exn
            in
            set_state files)
          ()
    in
    Basic_stateful.make
      (Bonsai.state
         (module struct
           type t = File.t Filename.Map.t [@@deriving equal, sexp]
         end)
         ~default_model:Filename.Map.empty)
      ~view
  ;;
end

module Freeform_multiselect = struct
  let set ?(extra_attr = Value.return Vdom.Attr.empty) ?placeholder ?split () =
    let%sub path, id = path in
    let%sub extra_attr =
      let%arr id_ = id
      and extra_attr = extra_attr in
      Vdom.Attr.(extra_attr @ id_)
    in
    let%sub freeform_multiselect =
      Bonsai_web_ui_freeform_multiselect.Freeform_multiselect.create
        ?placeholder
        ?split
        ~extra_attr
        ()
    in
    let%arr value, view, set = freeform_multiselect
    and path = path in
    Form.Expert.create ~value:(Ok value) ~view:(View.of_vdom ~id:path view) ~set
  ;;

  let list ?extra_attr ?placeholder ?split () =
    computation_map
      (set ?extra_attr ?placeholder ?split ())
      ~f:(Form.project ~parse_exn:Set.to_list ~unparse:String.Set.of_list)
  ;;
end

module Rank = struct
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
    let%sub path = Bonsai.Private.path in
    let%map.Computation value, view, inject =
      Bonsai_web_ui_reorderable_list.with_inject
        key
        ?enable_debug_overlay
        ?extra_item_attrs
        ?left
        ?right
        ?empty_list_placeholder
        ?default_item_height
        (fun ~index:_ ~source key ->
           let%map.Computation view = render ~source key in
           (), view)
    and path = return (path >>| Bonsai.Private.Path.to_unique_identifier_string) in
    Form.Expert.create
      ~value:(Ok (List.map ~f:fst value))
      ~view:(View.of_vdom ~id:path view)
      ~set:(fun items -> inject [ Overwrite items ])
  ;;
end

module Query_box = struct
  let stringable_opt
        (type k cmp)
        (module Key : Bonsai.Comparator with type t = k and type comparator_witness = cmp)
        ?initial_query
        ?max_visible_items
        ?suggestion_list_kind
        ?selected_item_attr
        ?extra_list_container_attr
        ?extra_input_attr
        ?(extra_attr = Value.return Vdom.Attr.empty)
        ?to_view
        input
    =
    let%sub path, id = path in
    let%sub extra_attr =
      let%arr extra_attr = extra_attr
      and id = id in
      Vdom.Attr.combine id extra_attr
    in
    let%sub last_selected_value, set_last_selected_value =
      Bonsai.state_opt
        (module struct
          type t = Key.t [@@deriving sexp]

          let equal a b = Key.comparator.compare a b = 0
        end)
    in
    let%sub query_box =
      Bonsai_web_ui_query_box.stringable
        (module Key)
        ?initial_query
        ?max_visible_items
        ?suggestion_list_kind
        ?selected_item_attr
        ?extra_list_container_attr
        ?extra_input_attr
        ~extra_attr
        ?to_view
        ~on_select:
          (let%map set_last_selected_value = set_last_selected_value in
           fun key -> set_last_selected_value (Some key))
        input
    in
    let%sub current_selection_view =
      let%arr last_selected_value = last_selected_value
      and input = input in
      match last_selected_value with
      | Some value ->
        (match Map.find input value with
         | Some string -> Vdom.Node.div [ Vdom.Node.text string ]
         | None ->
           Vdom.Node.div
             ~attr:(Vdom.Attr.style (Css_gen.color (`Name "red")))
             [ Vdom.Node.text "Selected item is not an input option" ])
      | None ->
        Vdom.Node.div
          ~attr:(Vdom.Attr.style (Css_gen.color (`Name "gray")))
          [ Vdom.Node.text "Nothing selected" ]
    in
    let%arr last_selected_value = last_selected_value
    and set_last_selected_value = set_last_selected_value
    and query_box = query_box
    and current_selection_view = current_selection_view
    and path = path in
    let view =
      Vdom.Node.div [ current_selection_view; Bonsai_web_ui_query_box.view query_box ]
    in
    Form.Expert.create
      ~value:(Ok last_selected_value)
      ~view:(View.of_vdom ~id:path view)
      ~set:set_last_selected_value
  ;;

  let stringable
        key
        ?initial_query
        ?max_visible_items
        ?suggestion_list_kind
        ?selected_item_attr
        ?extra_list_container_attr
        ?extra_input_attr
        ?extra_attr
        ?to_view
        input
    =
    Computation.map
      (stringable_opt
         key
         ?initial_query
         ?max_visible_items
         ?suggestion_list_kind
         ?selected_item_attr
         ?extra_list_container_attr
         ?extra_input_attr
         ?extra_attr
         ?to_view
         input)
      ~f:optional_to_required
  ;;
end
