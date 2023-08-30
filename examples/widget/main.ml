open! Core
open! Bonsai_web
open Bonsai.Let_syntax
open Js_of_ocaml
module Form = Bonsai_web_ui_form
module Widget = Bonsai_web_ui_widget

module T = struct
  type element = Dom_html.canvasElement
  type input = [ `Hex of string ]

  type state =
    { ctx : Dom_html.canvasRenderingContext2D Js.t
    ; mutable interval : Dom_html.interval_id
    ; mutable frame : int
    }

  let scale_dpi
    :  Dom_html.canvasElement Js.t -> Dom_html.canvasRenderingContext2D Js.t -> float
    -> float -> unit
    =
    let f = Js.Unsafe.get Js.Unsafe.global (Js.string "scale_canvas") in
    fun canvas ctx w h ->
      Js.Unsafe.fun_call
        f
        [| Js.Unsafe.inject canvas
         ; Js.Unsafe.inject ctx
         ; Js.Unsafe.inject w
         ; Js.Unsafe.inject h
        |]
  ;;

  let draw ~state ~ele ~(ctx : Dom_html.canvasRenderingContext2D Js.t) ~get_color =
    let f () =
      let (`Hex color) = get_color () in
      ctx##save;
      scale_dpi ele ctx 200.0 200.0;
      ctx##clearRect 0.0 0.0 100.0 100.0;
      ctx##beginPath;
      ctx##translate 40.0 40.0;
      ctx##rotate (Float.of_int state.frame /. 50.0);
      ctx##translate (-40.0) (-40.0);
      ctx##rect 20.0 20.0 40.0 40.0;
      ctx##.fillStyle := Js.string color;
      ctx##fill;
      state.frame <- state.frame + 1;
      ctx##restore;
      ()
    in
    Dom_html.window##setInterval (Js.wrap_callback f) 16.0
  ;;

  let init ~get_input _input =
    let ele = Dom_html.createCanvas Dom_html.document in
    ele##.width := 200;
    ele##.height := 200;
    let ctx = ele##getContext Dom_html._2d_ in
    let state = { ctx; interval = Obj.magic 0; frame = 0 } in
    let interval = draw ~ele ~state ~ctx ~get_color:get_input in
    state.interval <- interval;
    state, ele
  ;;

  let update ~prev_input:_ _input _state element = element
  let destroy _input state _element = Dom_html.window##clearInterval state.interval
end

let canvas_thing =
  let%sub scope_form = Form.Elements.Number.int ~default:1 ~step:1 () in
  let%sub how_many = Form.Elements.Number.int ~default:1 ~step:1 () in
  let%sub color_picker = Form.Elements.Color_picker.hex () in
  let%sub color =
    Bonsai.pure (Form.value_or_default ~default:(`Hex "#000")) color_picker
  in
  let%sub scope = Bonsai.pure (Form.value_or_default ~default:0) scope_form in
  let%sub widget =
    Bonsai.scope_model (module Int) ~on:scope (Widget.component (module T) color)
  in
  let%sub last_values, set_values =
    Bonsai.state [] ~sexp_of_model:[%sexp_of: int list] ~equal:[%equal: int list]
  in
  let%sub reset_effect =
    let%arr { modify; _ } = widget in
    modify (fun _input state -> state.frame <- 0)
  in
  let%sub read_effect =
    let%arr { read; _ } = widget
    and set_values = set_values in
    let%bind.Effect l = read (fun _input state -> state.frame) in
    set_values l
  in
  let%sub color_picker =
    let%arr color_picker = color_picker in
    color_picker |> Form.label "color" |> Form.view_as_vdom
  in
  let%sub scope_form =
    let%arr scope_form = scope_form in
    scope_form |> Form.label "scope" |> Form.view_as_vdom
  in
  let%sub theme = View.Theme.current in
  let%arr { view; _ } = widget
  and color_picker = color_picker
  and theme = theme
  and reset_effect = reset_effect
  and last_values = last_values
  and read_effect = read_effect
  and scope_form = scope_form
  and how_many = how_many in
  let how_many_view = Form.view_as_vdom how_many in
  let how_many = Form.value_or_default how_many ~default:1 in
  View.hbox
    ~cross_axis_alignment:Start
    [ View.vbox (List.init how_many ~f:(fun _ -> view))
    ; View.vbox
        [ color_picker
        ; scope_form
        ; how_many_view
        ; View.button theme ~on_click:reset_effect "reset"
        ; View.button theme ~on_click:read_effect "fetch"
        ; Vdom.Node.text
            ("last fetch: " ^ Sexp.to_string ([%sexp_of: int list] last_values))
        ]
    ]
;;

let component = canvas_thing
let () = Start.start component
