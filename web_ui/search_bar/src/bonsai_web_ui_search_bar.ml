open! Core
open Bonsai_web
open Bonsai.Let_syntax

module type Item = sig
  type t [@@deriving compare, sexp, equal]

  val to_string : t -> string
end

module Input = struct
  type 'a t =
    { choices : 'a list
    ; on_select : 'a -> unit Vdom.Effect.t
    }
  [@@deriving fields]
end

module Model = struct
  type 'a t =
    { query : string
    ; current_choices : 'a list
    ; focused_autocomplete_result : int option
    ; autocomplete_box_visible : bool
    }
  [@@deriving compare, equal, fields, sexp]

  let init ~query =
    { query
    ; current_choices = []
    ; focused_autocomplete_result = None
    ; autocomplete_box_visible = false
    }
  ;;

  let filter_choices ~score_choice ~filter_choice ~all_choices ~text =
    let compare = Comparable.lift Int.compare ~f:(score_choice ~input_text:text) in
    all_choices |> List.filter ~f:(filter_choice ~input_text:text) |> List.sort ~compare
  ;;

  let close_autocomplete_box t = { t with autocomplete_box_visible = false }

  let set_text_input ~all_choices ~score_choice ~filter_choice t query =
    match query with
    | "" ->
      { query
      ; autocomplete_box_visible = true
      ; focused_autocomplete_result = None
      ; current_choices = all_choices
      }
    | _ ->
      let current_choices =
        filter_choices ~score_choice ~filter_choice ~all_choices ~text:query
      in
      let focused_autocomplete_result =
        Option.map t.focused_autocomplete_result ~f:(fun i ->
          Int.min i (List.length current_choices - 1))
      in
      { query
      ; current_choices
      ; autocomplete_box_visible = true
      ; focused_autocomplete_result
      }
  ;;

  let bump_focused_autocomplete_result t direction ~max_query_results =
    match List.length t.current_choices with
    | 0 -> { t with focused_autocomplete_result = None }
    | n ->
      let visible_choices_length = Int.min n max_query_results in
      let i =
        match t.focused_autocomplete_result with
        | None ->
          (match direction with
           | `Up -> visible_choices_length - 1
           | `Down -> 0)
        | Some i ->
          let delta =
            match direction with
            | `Up -> -1
            | `Down -> 1
          in
          (i + delta) % visible_choices_length
      in
      { t with focused_autocomplete_result = Some i }
  ;;

  let clear_input t =
    { t with
      focused_autocomplete_result = None
    ; autocomplete_box_visible = false
    ; query = ""
    }
  ;;

  let clear_focused_autocomplete_result t = { t with focused_autocomplete_result = None }

  let set_focused_autocomplete_result t i =
    { t with focused_autocomplete_result = Some i }
  ;;

  let hide_autocomplete t = { t with autocomplete_box_visible = false }
end

module Action = struct
  type t =
    | Set_text_input of string
    | Bump_focused_autocomplete_result of [ `Up | `Down ]
    | Clear_focused_autocomplete_result
    | Set_focused_autocomplete_result of int
    | Close_autocomplete_box
    | Clear_input
    | On_focus
    | On_blur
  [@@deriving sexp_of, compare, variants]
end

let default_autocomplete_item item_to_string item =
  item |> item_to_string |> Vdom.Node.text
;;

let default_filter_choice item_to_string ~input_text item =
  String.is_substring (item_to_string item) ~substring:input_text
;;

let default_score_choice item_to_string ~input_text item =
  if default_filter_choice item_to_string item ~input_text
  then String.length input_text
  else 0
;;

let create
      (type item)
      (module Item : Item with type t = item)
      ?(max_query_results = 10)
      ?(width = Css_gen.Length.percent100)
      ?placeholder:(placeholder_ = "")
      ?(initial_query = "")
      ?(wrap_search_bar = Fn.id)
      ?(autocomplete_item = default_autocomplete_item Item.to_string)
      ?(filter_choice = default_filter_choice Item.to_string)
      ?(score_choice = default_score_choice Item.to_string)
      ?(of_string = Fn.const None)
      ?(extra_textbox_attr = Vdom.Attr.empty)
      ()
      input
  =
  let%sub model, inject =
    Bonsai.state_machine1
      (module struct
        include Model

        type nonrec t = Item.t t [@@deriving sexp, equal]
      end)
      (module Action)
      input
      ~default_model:(Model.init ~query:initial_query)
      ~apply_action:
        (fun ~inject:_
          ~schedule_event:_
          (input : item Input.t)
          (t : item Model.t)
          (action : Action.t) ->
          match (action : Action.t) with
          | Set_text_input query ->
            Model.set_text_input
              ~all_choices:(Input.choices input)
              t
              query
              ~score_choice
              ~filter_choice
          | Bump_focused_autocomplete_result direction ->
            Model.bump_focused_autocomplete_result t direction ~max_query_results
          | Clear_focused_autocomplete_result -> Model.clear_focused_autocomplete_result t
          | Set_focused_autocomplete_result i -> Model.set_focused_autocomplete_result t i
          | Close_autocomplete_box -> Model.close_autocomplete_box t
          | Clear_input -> Model.clear_input t
          | On_focus ->
            Model.set_text_input
              ~all_choices:(Input.choices input)
              t
              (Model.query t)
              ~score_choice
              ~filter_choice
          | On_blur -> Model.hide_autocomplete t)
  in
  let%sub on_select_item =
    let%arr inject = inject
    and input = input in
    fun item ->
      let event_parent = Input.on_select input item in
      let event_self = inject Action.Clear_input in
      Effect.Many [ event_parent; event_self; Effect.Prevent_default ]
  in
  let render_autocomplete_entry index item =
    let%arr model = model
    and on_select_item = on_select_item
    and inject = inject
    and index = index
    and item = item in
    let is_focused =
      Model.focused_autocomplete_result model
      |> Option.value_map ~default:false ~f:(fun i -> index = i)
    in
    let focused_attr =
      if is_focused then Vdom.Attr.class_ "autocomplete-active" else Vdom.Attr.empty
    in
    Vdom.Node.li
      ~attr:
        Vdom.Attr.(
          on_click (fun _ -> on_select_item item)
          @ on_mouseover (fun _ -> inject (Action.Set_focused_autocomplete_result index))
          @ focused_attr
          (* Stop the mousedown event, as we handle on_click above, and
             the blur from mousedown would interact poorly with on_blur for the input *)
          @ on_mousedown (fun _ ->
            Effect.Many [ Effect.Stop_propagation; Effect.Prevent_default ]))
      [ autocomplete_item item ]
  in
  let%sub render_autocomplete =
    let%sub entries =
      Bonsai.assoc
        (module Int)
        (let%map model = model in
         List.take (Model.current_choices model) max_query_results
         |> List.mapi ~f:Tuple2.create
         |> Int.Map.of_alist_exn)
        ~f:render_autocomplete_entry
    in
    let%arr model = model
    and inject = inject
    and entries = entries in
    let draw_width = width in
    let hidden_query_results =
      let len = Model.current_choices model |> List.length in
      if len > max_query_results
      then
        [ Vdom.Node.li
            ~attr:
              Vdom.Attr.(
                on_mousedown (fun _ -> Effect.Prevent_default)
                @ class_ "no-cursor"
                @ style
                    Css_gen.(
                      background_color (`Var "--js-dark-snow-color") @> width draw_width))
            [ Vdom.Node.text (sprintf "+%d more" (len - max_query_results)) ]
        ]
      else []
    in
    [ Vdom.Node.div
        ~attr:
          Vdom.Attr.(
            class_ "autocomplete"
            @ style Css_gen.(width draw_width @> max_width draw_width))
        [ Vdom.Node.ul
            ~attr:
              Vdom.Attr.(
                class_ "autocomplete-items"
                @ id "autocomplete-items"
                @ on_mouseout (fun _ -> inject Action.Clear_focused_autocomplete_result))
            (Map.data entries @ hidden_query_results)
        ]
    ]
  in
  let%sub handle_keyup =
    let%arr on_select_item = on_select_item
    and model = model in
    fun ev ->
      let current_choices = Model.current_choices model in
      match Js_of_ocaml.Dom_html.Keyboard_code.of_event ev with
      | Enter | NumpadEnter ->
        (match
           Model.focused_autocomplete_result model, Model.autocomplete_box_visible model
         with
         | Some i, true -> List.nth_exn current_choices i |> on_select_item
         | None, true ->
           let item = of_string (Model.query model) in
           (match item with
            | None -> Effect.Ignore
            | Some item ->
              if List.mem current_choices item ~equal:Item.equal
              then on_select_item item
              else (
                match current_choices with
                | [ item ] -> on_select_item item
                | _ -> Effect.Ignore))
         | _ -> Effect.Ignore)
      | _ -> Effect.Ignore
  in
  let%sub handle_keydown =
    let%arr model = model
    and inject = inject in
    fun ev ->
      match Js_of_ocaml.Dom_html.Keyboard_code.of_event ev with
      | ArrowUp ->
        Vdom.Effect.Many
          [ inject (Action.Bump_focused_autocomplete_result `Up)
          ; Vdom.Effect.Prevent_default
          ]
      | ArrowDown ->
        Vdom.Effect.Many
          [ inject (Action.Bump_focused_autocomplete_result `Down)
          ; Vdom.Effect.Prevent_default
          ]
      | Escape ->
        if Model.autocomplete_box_visible model
        then inject Action.Close_autocomplete_box
        else Effect.Ignore
      | _ -> Effect.Ignore
  in
  let%arr handle_keydown = handle_keydown
  and handle_keyup = handle_keyup
  and render_autocomplete = render_autocomplete
  and model = model
  and inject = inject in
  let autocomplete_items =
    if Model.autocomplete_box_visible model
    && not (List.is_empty (Model.current_choices model))
    then render_autocomplete
    else []
  in
  let query_as_string = Model.query model in
  let set_text string = string |> Action.set_text_input |> inject in
  Vdom.Node.div
    ~attr:Vdom.Attr.(Vdom.Attr.class_ "wrapper" @ Vdom.Attr.style (Css_gen.width width))
    ([ Vdom.Node.div
         ~attr:(Vdom.Attr.class_ "search-input-container")
         [ Vdom.Node.input
             ~attr:
               Vdom.Attr.(
                 value_prop query_as_string
                 @ placeholder placeholder_
                 @ tabindex 1
                 @ on_keyup handle_keyup
                 @ on_keydown handle_keydown
                 @ style (Css_gen.width width)
                 @ on_change (fun _ -> set_text)
                 @ on_input (fun _ -> set_text)
                 @ on_focus (fun (_ : Js_of_ocaml.Dom_html.focusEvent Js_of_ocaml.Js.t) ->
                   inject Action.On_focus)
                 @ on_blur (fun (_ : Js_of_ocaml.Dom_html.focusEvent Js_of_ocaml.Js.t) ->
                   inject Action.On_blur)
                 @ extra_textbox_attr)
             ()
         ]
     ]
     @ autocomplete_items)
  |> wrap_search_bar
;;
