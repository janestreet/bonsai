open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_automatic_view

module Style =
[%css
stylesheet
  {|
  .form-title {
    justify-content:center;
    padding: 3px;
  }

  .card-content {
    flex-grow: 1;
  }
|}]

module Shared = struct
  type t =
    { left : [ `Hex of string ]
    ; right : [ `Hex of string ]
    }
  [@@deriving typed_fields]

  let form =
    Form.Typed.Record.make
      (module struct
        module Typed_field = Typed_field

        let label_for_field = `Inferred

        let form_for_field : type a. a Typed_field.t -> Bonsai.graph -> a Form.t Bonsai.t =
          fun typed_field graph ->
          match typed_field with
          | Left -> Form.Elements.Color_picker.hex () graph
          | Right -> Form.Elements.Color_picker.hex () graph
        ;;
      end)
  ;;
end

module For_gradient = struct
  type t = { steps : int } [@@deriving typed_fields]

  let form =
    Form.Typed.Record.make
      (module struct
        module Typed_field = Typed_field

        let label_for_field = `Inferred

        let form_for_field : type a. a Typed_field.t -> Bonsai.graph -> a Form.t Bonsai.t =
          fun typed_field graph ->
          match typed_field with
          | Steps ->
            Form.Elements.Range.int
              ~min:1
              ~max:200
              ~default:50
              ~step:1
              ~allow_updates_when_focused:`Never
              ()
              graph
        ;;
      end)
  ;;
end

module For_overlay = struct
  type t =
    { left_alpha : float
    ; right_alpha : float
    }
  [@@deriving typed_fields]

  let form =
    Form.Typed.Record.make
      (module struct
        module Typed_field = Typed_field

        let label_for_field : type a. a Typed_field.t -> string = function
          | Typed_field.Left_alpha -> "left alpha"
          | Right_alpha -> "right alpha"
        ;;

        let label_for_field = `Computed label_for_field

        let form_for_field : type a. a Typed_field.t -> Bonsai.graph -> a Form.t Bonsai.t =
          fun typed_field graph ->
          match typed_field with
          | Left_alpha ->
            Form.Elements.Range.float
              ~min:0.0
              ~max:1.0
              ~default:0.5
              ~step:0.01
              ~allow_updates_when_focused:`Never
              ()
              graph
          | Right_alpha ->
            Form.Elements.Range.float
              ~min:0.0
              ~max:1.0
              ~default:0.5
              ~step:0.01
              ~allow_updates_when_focused:`Never
              ()
              graph
        ;;
      end)
  ;;
end

type t =
  { shared : Shared.t
  ; for_gradient : For_gradient.t
  ; for_overlay : For_overlay.t
  }
[@@deriving typed_fields]

let initial_params =
  { shared = { left = `Hex "#FFFF00"; right = `Hex "#0000FF" }
  ; for_gradient = { steps = 10 }
  ; for_overlay = { left_alpha = 0.5; right_alpha = 0.5 }
  }
;;

let form graph =
  let shared = Shared.form graph in
  let for_gradient = For_gradient.form graph in
  let for_overlay = For_overlay.form graph in
  let all =
    Form.Typed.Record.make
      (module struct
        module Typed_field = Typed_field

        let label_for_field = `Inferred

        let form_for_field : type a. a Typed_field.t -> Bonsai.graph -> a Form.t Bonsai.t =
          fun typed_field _graph ->
          match typed_field with
          | Shared -> shared
          | For_gradient -> for_gradient
          | For_overlay -> for_overlay
        ;;
      end)
      graph
  in
  let all = Form.Dynamic.with_default (Bonsai.return initial_params) all graph in
  let value =
    let%arr all = all in
    (match Form.value all with
     | Error e -> print_s [%message (e : Error.t)]
     | _ -> ());
    Form.value_or_default all ~default:initial_params
  in
  let card_helper theme title form =
    View.card'
      theme
      ~title_attrs:[ Style.form_title ]
      ~content_attrs:[ Style.card_content ]
      ~title:[ Vdom.Node.text title ]
      [ Form.view_as_vdom form ]
  in
  let view =
    let theme = View.Theme.current graph in
    let%arr shared = shared
    and for_gradient = for_gradient
    and for_overlay = for_overlay
    and theme = theme in
    View.hbox
      ~gap:(`Em 1)
      ~main_axis_alignment:Center
      [ card_helper theme "shared" shared
      ; card_helper theme "for gradient" for_gradient
      ; card_helper theme "for overlay" for_overlay
      ]
  in
  Bonsai.both value view
;;
