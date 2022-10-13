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

let default_theme =
  make_theme
    (module struct
      class c =
        object (self)
          method theme_name = "default theme"

          method constants =
            { Constants.primary =
                { foreground = `Name "black"; background = `Name "white" }
            ; intent =
                { info = { background = `Hex "#e0f7ff"; foreground = `Name "black" }
                ; success = { background = `Hex "#ecffe0"; foreground = `Name "black" }
                ; warning = { background = `Hex "#ffeb3b"; foreground = `Name "black" }
                ; error = { background = `Name "#ff2522"; foreground = `Name "white" }
                }
            }

          method humanize_sexp sexp =
            match sexp with
            | Sexp.Atom text ->
              String.map text ~f:(function
                | '_' -> ' '
                | o -> o)
            | _ -> Sexp.to_string_hum sexp

          method button ~attr ~disabled ~intent ~on_click content =
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
            let attr =
              Vdom.Attr.many [ attr; on_click_attr; disabled_attr; maybe_colors ]
            in
            Vdom.Node.button ~attr content

          method tabs
            : type a.  attr:Vdom.Attr.t
              -> per_tab_attr:(a -> is_active:bool -> Vdom.Attr.t)
              -> on_change:(from:a -> to_:a -> unit Effect.t)
              -> equal:(a -> a -> bool)
              -> active:a
              -> (a * Vdom.Node.t) list
              -> Vdom.Node.t =
            fun ~attr ~per_tab_attr ~on_change ~equal ~active tabs ->
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
              let attr =
                Vdom.Attr.many
                  [ (if is_active then active_attr else inactive_attr i)
                  ; all_attr
                  ; per_tab_attr i ~is_active
                  ]
              in
              Vdom.Node.div ~attr [ tab ])
            |> Layout.hbox ~attr ~gap:(`Em_float 0.5)

          method devbar ~attr ~count ~intent text =
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
              ~attr:(Vdom.Attr.many [ attr; main_attr ])
              (List.init count ~f:(fun i ->
                 let attr = if i % 2 = 0 then even_attr else odd_attr in
                 Vdom.Node.span ~attr [ Vdom.Node.text text ]))

          method text ~attr ~intent text =
            let maybe_colors =
              match intent with
              | None -> Vdom.Attr.empty
              | Some intent ->
                let { Constants.Fg_bg.foreground; background } =
                  Constants.Intent.lookup self#constants.intent intent
                in
                let maybe_bold =
                  match intent with
                  | Warning | Error -> Css_gen.font_weight `Bold
                  | _ -> Css_gen.empty
                in
                Vdom.Attr.style
                  Css_gen.(
                    background_color background
                    @> color foreground
                    @> padding_left (`Em_float 0.25)
                    @> padding_right (`Em_float 0.25)
                    @> maybe_bold)
            in
            let attr = Vdom.Attr.many [ attr; maybe_colors ] in
            Vdom.Node.span ~attr [ Vdom.Node.text text ]
        end
    end)
;;
