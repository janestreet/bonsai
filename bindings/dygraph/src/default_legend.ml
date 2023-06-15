open Core
open Import

module Model = struct
  module Series = struct
    type t =
      { label          : string
      ; value          : Raw_html.t option
      ; dash           : Raw_html.t option
      ; color          : string option
      ; is_visible     : bool
      ; is_highlighted : bool
      }
    [@@deriving equal, fields, sexp]

    let toggle_visibility t = { t with is_visible = not t.is_visible }

    let view { label; value; dash; color; is_visible; is_highlighted } ~on_toggle =
      let dash =
        match dash with
        | None      -> Vdom.Node.none
        | Some html -> Raw_html.view ~tag:"span" html
      in
      let value =
        match value with
        | None      -> Vdom.Node.none
        | Some html -> Raw_html.view ~tag:"span" html
      in
      let create_style l = List.filter_opt l |> Css_gen.concat |> Vdom.Attr.style in
      let label_style =
        let margin_left = Css_gen.margin_left (`Px 5) in
        let color =
          Option.map color ~f:(fun value -> Css_gen.create ~field:"color" ~value)
        in
        create_style [ color; Some margin_left ]
      in
      let style =
        create_style
          [ Option.some_if is_highlighted (Css_gen.font_weight `Bold)
          ; Option.some_if
              is_highlighted
              (Css_gen.text_decoration () ~line:[ `Underline ])
          ]
      in
      Vdom.Node.label
        ~attrs:[ style ]
        [ Vdom.Node.input
            ~attrs:
              [ Vdom.Attr.many_without_merge
                  [ Vdom.Attr.type_ "checkbox"
                  ; Vdom.Attr.on_click (fun _ev -> on_toggle ())
                  ; Vdom.Attr.bool_property "checked" is_visible
                  ]
              ]
            ()
        ; dash
        ; Vdom.Node.span ~attrs:[ label_style ] [ Vdom.Node.textf "%s: " label ]
        ; value
        ]
    ;;
  end

  type t =
    { x_label     : string
    ; x_value     : Raw_html.t option
    ; series      : Series.t list
    ; past_series : Series.t Map.M(String).t
    }
  [@@deriving equal, sexp]

  let view
        { x_label; x_value; series; past_series = _ }
        ~on_toggle
        ~select_all
        ~select_none
    =
    let x =
      let value =
        match x_value with
        | None      -> Vdom.Node.none
        | Some html -> Raw_html.view ~tag:"span" html
      in
      Vdom.Node.label [ Vdom.Node.textf "%s: " x_label; value ]
    in
    (* mostly copied from bonsai_multi_select *)
    let select_all_or_none =
      let open Vdom in
      let link ~text ~action ~class_ =
        Node.a
          ~attrs:
            [ Vdom.Attr.many_without_merge
                [ Attr.href "about:blank"
                ; Attr.on_click (fun _ev ->
                    Effect.Many [ action (); Effect.Prevent_default ])
                ; Attr.class_ class_
                ]
            ]
          [ Node.text text ]
      in
      Node.div
        ~attrs:[ Attr.class_ "multi-select-select-all-none" ]
        [ Node.text "Select: "
        ; link ~text:"all" ~action:select_all ~class_:"multi-select-select-all"
        ; Node.text "; "
        ; link ~text:"none" ~action:select_none ~class_:"multi-select-select-none"
        ]
    in
    let list_elements =
      select_all_or_none
      :: x
      :: List.map series ~f:(fun series ->
        Series.view series ~on_toggle:(fun () -> on_toggle series.label))
    in
    (* Mostly copied from vdom_input_widgets *)
    Vdom.Node.div
      ~attrs:
        [ Vdom.Attr.many_without_merge
            [ Vdom.Attr.classes [ "widget-checklist"; "checkbox-container" ]
            ; Vdom.Attr.style
                Css_gen.(create ~field:"list-style" ~value:"none" @> margin_left (`Px 0))
            ]
        ]
      (List.map list_elements ~f:(fun li -> Vdom.Node.div [ li ]))
  ;;
end

module Action = struct
  type t =
    | From_graph        of Legend_data.t
    | Toggle_visibility of string
    | Select_none
    | Select_all
  [@@deriving equal, sexp]
end

let apply_action
      (_ : _ Bonsai.Apply_action_context.t)
      (model : Model.t)
      (action : Action.t)
  =
  let map_series ~f = { model with series = List.map model.series ~f } in
  match action with
  | From_graph legend_data ->
    let series =
      List.map model.series ~f:(fun series ->
        match
          List.find legend_data.series ~f:(fun s -> String.equal series.label s.label)
        with
        | None             -> series
        | Some legend_data ->
          let { Legend_data.Series.dashHTML
              ; isHighlighted
              ; color
              ; yHTML
              ; label     = _
              ; labelHTML = _
              ; isVisible = _
              ; y         = _
              }
            =
            legend_data
          in
          let color =
            (* keep last color if [color] is none *)
            Option.first_some color series.color
          in
          { series with
            dash           = Some dashHTML
          ; color
          ; value          = yHTML
          ; is_highlighted = Option.value ~default:false isHighlighted
          })
    in
    let x_value =
      Option.map legend_data.xHTML ~f:(function
        | `number f      -> Float.to_string f |> Raw_html.of_string
        | `html raw_html -> raw_html)
    in
    { model with x_value; series }
  | Select_none -> map_series ~f:(fun series -> { series with is_visible = false })
  | Select_all  -> map_series ~f:(fun series -> { series with is_visible = true  })
  | Toggle_visibility label ->
    map_series ~f:(fun series ->
      if String.(series.label = label)
      then Model.Series.toggle_visibility series
      else series)
;;

let series_from_info { Per_series_info.label; visible_by_default } =
  { Model.Series.label
  ; is_visible     = visible_by_default
  ; is_highlighted = false
  ; value          = None
  ; dash           = None
  ; color          = None
  }
;;

let create ~x_label ~per_series_info
  : (Model.t * Vdom.Node.t * (Action.t -> unit Vdom.Effect.t)) Bonsai.Computation.t
  =
  let create_model =
    let%map.Bonsai x_label         = x_label
    and            per_series_info = per_series_info in
    function
    | None ->
      { Model.x_label
      ; x_value     = None
      ; series      = List.map per_series_info ~f:series_from_info
      ; past_series = String.Map.empty
      }
    | Some (model : Model.t) ->
      let existing_y_labels = List.map model.series    ~f:Model.Series.label    in
      let model_y_labels    = List.map per_series_info ~f:Per_series_info.label in
      if [%equal: string     ] model.x_label  x_label
      && [%equal: string list] model_y_labels existing_y_labels
      then { model with x_label }
      else (
        let past_series =
          (* Every time the [model_y_labels] changes, we want to remember the visibility
             status of all the series labels we know about so far.  This will help in the
             case where we toggle visibility on series A, flip to a graph which does not
             have that series, and then flip back to the original graph.  Without
             remembering, the visibility status of series A revert back to the default
             status. *)
          List.fold ~init:model.past_series model.series ~f:(fun past_series series ->
            Map.set past_series ~key:series.label ~data:series)
        in
        let series =
          List.map per_series_info ~f:(fun per_series_info ->
            let { Per_series_info.label; visible_by_default = _ } = per_series_info in
            match Map.find past_series label with
            | None        -> series_from_info per_series_info
            | Some series -> series)
        in
        { model with x_label; series; past_series })
  in
  let%sub state =
    Bonsai_extra.state_machine0_dynamic_model
      ~sexp_of_model:[%sexp_of: Model.t]
      ~equal:[%equal: Model.t]
      (module Action)
      ~model:(`Computed create_model)
      ~apply_action
  in
  return
  @@
  let%map model, inject_action = state in
  let view =
    Model.view
      model
      ~on_toggle:(fun label -> inject_action (Toggle_visibility label))
      ~select_all:(fun () -> inject_action Select_all)
      ~select_none:(fun () -> inject_action Select_none)
  in
  model, view, inject_action
;;
