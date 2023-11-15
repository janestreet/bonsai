open! Core
open! Import
module Constants = Constants
module Theme = Theme
include Underlying_intf

type t = (module S)

let make_theme (m : t) : Theme.t =
  (module struct
    include (val m)

    let singleton = new c
  end)
;;

let override_theme ((module M) : Theme.t) ~(f : t -> t) : Theme.t =
  (module struct
    include (val f (module M))

    let singleton = new c
  end)
;;

module Style =
[%css
stylesheet
  {|
@layer bonsai_web_ui_view.app {
  :root:has(.app) {
    font-family: sans-serif;
  }

  :root:has(.app) *,
  :root:has(.app) *::before,
  :root:has(.app) *::after {
    box-sizing: border-box;
  }
}
|}]

let default_theme =
  make_theme
    (module struct
      class c =
        object (self : #Underlying_intf.C.t)
          method theme_name = "default theme"
          method app_attr = Style.app

          method constants =
            let primary =
              { Constants.Fg_bg.foreground = `Name "black"; background = `Name "white" }
            in
            let header =
              { Constants.Fg_bg.foreground = primary.background
              ; background = primary.foreground
              }
            in
            let table_row_even =
              { Constants.Fg_bg.foreground = `Name "black"; background = `Name "#e6e6e6" }
            in
            let extreme_primary_border = `Name "grey" in
            { Constants.primary
            ; extreme = primary
            ; extreme_primary_border
            ; intent =
                { info = { background = `Hex "#e0f7ff"; foreground = `Name "#0a90bf" }
                ; success = { background = `Hex "#ecffe0"; foreground = `Name "#348203" }
                ; warning = { background = `Hex "#ffeb3b"; foreground = `Name "#6b6001" }
                ; error = { background = `Name "#ff2522"; foreground = `Name "#630100" }
                }
            ; table =
                { body_row_even = table_row_even
                ; body_row_odd = primary
                ; header_row = header
                ; header_header_border = extreme_primary_border
                ; header_body_border = extreme_primary_border
                ; body_body_border = extreme_primary_border
                }
            ; form =
                { error_message =
                    { foreground = `Name "black"; background = `Name "pink" }
                ; error_toggle_text = `Hex "#f54646"
                ; error_border = `Name "red"
                ; tooltip_message =
                    { foreground = `Name "black"; background = `Name "azure" }
                ; tooltip_border = `Name "darkblue"
                ; tooltip_toggle_text = `Name "blue"
                }
            ; small_font_size = `Em_float 0.8
            ; large_font_size = `Em_float 1.2
            ; is_dark = false
            }

          method humanize_sexp sexp =
            match sexp with
            | Sexp.Atom text ->
              String.map text ~f:(function
                | '_' -> ' '
                | o -> o)
            | _ -> Sexp.to_string_hum sexp

          method button ~attrs ~disabled ~intent ~tooltip ~on_click content =
            let on_click_attr = Vdom.Attr.on_click (fun _ -> on_click) in
            let disabled_attr =
              if disabled then Vdom.Attr.disabled else Vdom.Attr.empty
            in
            let maybe_colors =
              match intent with
              | None -> Vdom.Attr.empty
              | Some intent ->
                let maybe_transparent =
                  if disabled then Css_gen.opacity 0.3 else Css_gen.empty
                in
                let { Constants.Fg_bg.foreground; background } =
                  Constants.Intent.lookup self#constants.intent intent
                in
                Vdom.Attr.style
                  Css_gen.(
                    background_color background @> color foreground @> maybe_transparent)
            in
            let maybe_title =
              match tooltip with
              | None -> Vdom.Attr.empty
              | Some s -> Vdom.Attr.title s
            in
            let attrs =
              attrs @ [ on_click_attr; disabled_attr; maybe_colors; maybe_title ]
            in
            Vdom.Node.button ~attrs content

          method tabs
            : type a.  attrs:Vdom.Attr.t list
                      -> per_tab_attrs:(a -> is_active:bool -> Vdom.Attr.t list)
                      -> on_change:(from:a -> to_:a -> unit Effect.t)
                      -> equal:(a -> a -> bool)
                      -> active:a
                      -> (a * Vdom.Node.t) list
                      -> Vdom.Node.t =
            fun ~attrs ~per_tab_attrs ~on_change ~equal ~active tabs ->
              let color = self#constants.primary.foreground in
              let all_attr =
                Vdom.Attr.style (Css_gen.create ~field:"cursor" ~value:"pointer")
              in
              let active_attr =
                Vdom.Attr.style
                  (Css_gen.border_bottom ~width:(`Px 3) ~color ~style:`Solid ())
              in
              let inactive_attr i =
                Vdom.Attr.many
                  [ Vdom.Attr.style
                      (Css_gen.concat
                         [ Css_gen.border_bottom ~width:(`Px 1) ~color ~style:`Solid ()
                         ; Css_gen.opacity 0.6
                         ])
                  ; Vdom.Attr.on_click (fun _ -> on_change ~from:active ~to_:i)
                  ]
              in
              List.map tabs ~f:(fun (i, tab) ->
                let is_active = equal active i in
                Vdom.Node.div
                  ~attrs:
                    [ (if is_active then active_attr else inactive_attr i)
                    ; all_attr
                    ; Vdom.Attr.many (per_tab_attrs i ~is_active)
                    ]
                  [ tab ])
              |> Layout.hbox ~attrs ~gap:(`Em_float 0.5)

          method devbar ~attrs ~count ~intent text =
            let intent = Option.value intent ~default:Constants.Intent.Error in
            let even_attr, odd_attr =
              let both_style =
                Css_gen.(padding_left (`Em_float 0.5) @> padding_right (`Em_float 0.5))
              in
              let { Constants.Fg_bg.foreground = normal_fg; background = normal_bg } =
                self#constants.primary
              in
              let { Constants.Fg_bg.foreground = intent_fg; background = intent_bg } =
                Constants.Intent.lookup self#constants.intent intent
              in
              let even_attr =
                Vdom.Attr.style
                  Css_gen.(color normal_bg @> background_color normal_fg @> both_style)
              in
              let odd_attr =
                Vdom.Attr.style
                  Css_gen.(color intent_fg @> background_color intent_bg @> both_style)
              in
              even_attr, odd_attr
            in
            let main_attr =
              Vdom.Attr.style
                Css_gen.(
                  max_width (`Percent Percent.one_hundred_percent) @> overflow `Hidden)
            in
            Layout.hbox
              ~attrs:(attrs @ [ main_attr ])
              (List.init count ~f:(fun i ->
                 Vdom.Node.span
                   ~attrs:[ (if i % 2 = 0 then even_attr else odd_attr) ]
                   [ Vdom.Node.text text ]))

          method tooltip = Tooltip.make self#constants
          method use_intent_fg_or_bg_for_highlighting : [ `Fg | `Bg ] = `Fg

          method themed_text ~attrs ~intent ~style ~size text =
            let maybe_colors =
              match intent with
              | None -> Vdom.Attr.empty
              | Some intent ->
                let highlight_color =
                  let { Constants.Fg_bg.foreground; background } =
                    Constants.Intent.lookup self#constants.intent intent
                  in
                  match self#use_intent_fg_or_bg_for_highlighting with
                  | `Fg -> foreground
                  | `Bg -> background
                in
                let intense_intents =
                  match intent with
                  | Warning | Error ->
                    Css_gen.(font_weight `Bold @> color highlight_color)
                  | _ -> Css_gen.empty
                in
                Vdom.Attr.style
                  Css_gen.(
                    text_decoration
                      ()
                      ~style:`Dashed
                      ~color:highlight_color
                      ~line:[ `Underline ]
                    @> create ~field:"text-underline-offset" ~value:"4px"
                    @> intense_intents)
            in
            let maybe_style =
              match (style : Constants.Font_style.t option) with
              | None | Some Regular -> Vdom.Attr.empty
              | Some Bold -> Vdom.Attr.style (Css_gen.font_weight `Bold)
              | Some Italic -> Vdom.Attr.style (Css_gen.font_style `Italic)
              | Some Underlined ->
                Vdom.Attr.style (Css_gen.text_decoration ~line:[ `Underline ] ())
            in
            let maybe_size =
              match (size : Constants.Font_size.t option) with
              | None | Some Regular -> Vdom.Attr.empty
              | Some Small ->
                Vdom.Attr.style (Css_gen.font_size self#constants.small_font_size)
              | Some Large ->
                Vdom.Attr.style (Css_gen.font_size self#constants.large_font_size)
            in
            Vdom.Node.span
              ~attrs:(attrs @ [ maybe_colors; maybe_size; maybe_style ])
              [ Vdom.Node.text text ]

          method codemirror_theme : For_codemirror.Theme.t option = None

          (* tables *)
          method table = Table.table_attr self#constants
          method table_header = Table.table_header_attr
          method table_header_row = Table.table_header_row
          method table_header_cell = Table.table_header_cell
          method table_body = Table.table_body_attr
          method table_body_row = Table.table_body_row
          method table_body_cell = Table.table_body_cell
          method table_body_cell_empty = Table.table_body_cell_empty

          (* misc forms *)
          method form_view_error = Form.view_error

          method form_view_error_details =
            Form.view_error_details (self :> Underlying_intf.C.t)

          method form_view_tooltip = Form.view_tooltip (self :> Underlying_intf.C.t)
          method form_remove_item = Form.render_remove_item (self :> Underlying_intf.C.t)
          method form_append_item = Form.render_append_item (self :> Underlying_intf.C.t)

          (* form_constructors *)
          method form_empty = Form.empty
          method form_collapsible = Form.collapsible (self :> Underlying_intf.C.t)
          method form_raw = Form.raw (self :> Underlying_intf.C.t)
          method form_record = Form.record (self :> Underlying_intf.C.t)
          method form_variant = Form.variant (self :> Underlying_intf.C.t)
          method form_tuple = Form.tuple (self :> Underlying_intf.C.t)
          method form_option = Form.option (self :> Underlying_intf.C.t)
          method form_list = Form.list (self :> Underlying_intf.C.t)
          method form_view = Form.view (self :> Underlying_intf.C.t)
          method form_toplevel_combine = Form.toplevel_combine

          (* forms *)
          method form_to_vdom = Form.to_vdom (self :> Underlying_intf.C.t)

          (* cards *)
          method card
            ~container_attrs
            ~title_attrs
            ~content_attrs
            ~intent
            ~on_click
            ~title
            ~title_kind
            ~content =
            let open Constants in
            let module Style = Card_style in
            let constants = self#constants in
            let vars =
              let intent_fg =
                Option.value_map
                  intent
                  ~default:constants.primary.foreground
                  ~f:(fun intent -> (Intent.lookup constants.intent intent).foreground)
                |> Css_gen.Color.to_string_css
              in
              let intent_bg =
                Option.value_map
                  intent
                  ~default:constants.primary.background
                  ~f:(fun intent -> (Intent.lookup constants.intent intent).background)
                |> Css_gen.Color.to_string_css
              in
              let extreme_fg =
                constants.extreme.foreground |> Css_gen.Color.to_string_css
              in
              let extreme_bg =
                constants.extreme.background |> Css_gen.Color.to_string_css
              in
              Card_style.Variables.set_all ~extreme_bg ~extreme_fg ~intent_bg ~intent_fg
            in
            let title =
              match title with
              | [] -> Vdom.Node.none
              | _ ->
                let create_title ~f ~extra_attr =
                  f
                    ~attrs:
                      [ Style.title_bar
                      ; Style.title_text
                      ; Vdom.Attr.many title_attrs
                      ; extra_attr
                      ]
                    title
                in
                let title =
                  match title_kind with
                  | Card_title_kind.Prominent ->
                    create_title
                      ~f:(fun ~attrs x -> Layout.hbox ~attrs x)
                      ~extra_attr:Vdom.Attr.empty
                  | Discreet ->
                    create_title
                      ~f:(fun ~attrs x -> Vdom.Node.legend ~attrs x)
                      ~extra_attr:Style.card_legend
                in
                title
            in
            let create_card ~f ~extra_container_attr =
              f
                ~attrs:
                  [ Vdom.Attr.many container_attrs
                  ; Vdom.Attr.on_click (fun _ -> on_click)
                  ; vars
                  ; extra_container_attr
                  ]
                [ title
                ; Layout.vbox
                    ~attrs:[ Vdom.Attr.many content_attrs; Style.content_common ]
                    content
                ]
            in
            match title_kind with
            | Card_title_kind.Prominent ->
              create_card
                ~f:(fun ~attrs x -> Layout.vbox ~attrs x)
                ~extra_container_attr:Style.container
            | Discreet ->
              create_card
                ~f:(fun ~attrs x -> Vdom.Node.fieldset ~attrs x)
                ~extra_container_attr:Style.fieldset_container

          method textbox ?attrs ?placeholder ~disabled ~value ~set_value () =
            Vdom_input_widgets.Entry.text
              ?extra_attrs:attrs
              ?placeholder
              ~disabled
              ~value:(Some value)
              ~on_input:(fun s -> set_value (Option.value ~default:"" s))
              ()

          method password ?attrs ?placeholder ~disabled ~value ~set_value () =
            Vdom_input_widgets.Entry.password
              ?extra_attrs:attrs
              ?placeholder
              ~disabled
              ~value:(Some value)
              ~on_input:(fun s -> set_value (Option.value ~default:"" s))
              ()

          method textarea ?attrs ?placeholder ~disabled ~value ~set_value () =
            Vdom_input_widgets.Entry.text_area
              ?extra_attrs:attrs
              ?placeholder
              ~disabled
              ~value
              ~on_input:set_value
              ()

          method number
            ?(attrs = [])
            ?placeholder
            ?min
            ?max
            ~disabled
            ~step
            ~value
            ~set_value
            () =
            let min = Option.value_map min ~default:Vdom.Attr.empty ~f:Vdom.Attr.min in
            let max = Option.value_map max ~default:Vdom.Attr.empty ~f:Vdom.Attr.max in
            Vdom_input_widgets.Entry.number
              ~extra_attrs:(attrs @ [ min; max ])
              ?placeholder
              ~disabled
              ~value
              ~on_input:set_value
              ~step
              (module Vdom_input_widgets.Decimal)

          method range ?(attrs = []) ?min ?max ~disabled ~step ~value ~set_value () =
            let min = Option.value_map min ~default:Vdom.Attr.empty ~f:Vdom.Attr.min in
            let max = Option.value_map max ~default:Vdom.Attr.empty ~f:Vdom.Attr.max in
            Vdom_input_widgets.Entry.range
              ~extra_attrs:(attrs @ [ min; max ])
              ~disabled
              ~value:(Some value)
              ~on_input:(function
                | None -> Effect.print_s [%message "BUG: no value"]
                | Some value -> set_value value)
              ~step
              (module Vdom_input_widgets.Decimal)
        end
    end)
;;
