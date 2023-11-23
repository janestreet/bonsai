open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Animation = Bonsai_experimental_animation
module Form = Bonsai_web_ui_form

let component =
  let%sub interpolator_form =
    Form.Elements.Dropdown.enumerable (module Animation.Interpolator) ~init:`First_item
  in
  let%sub text_picker =
    Form.Elements.Textbox.string ~allow_updates_when_focused:`Never ()
  in
  let%sub text_picker =
    text_picker |> Form.Dynamic.with_default (Bonsai.Value.return "Hello Animation!")
  in
  let interpolator =
    interpolator_form >>| Form.value_or_default ~default:Animation.Interpolator.Linear
  in
  let%sub { value; animate } =
    Animation.Advanced.make
      ~fallback:(Value.return 0.0)
      ~interpolate:Animation.Interpolatable.float
  in
  let%sub forward, set_forward =
    Bonsai.state true ~sexp_of_model:[%sexp_of: Bool.t] ~equal:[%equal: Bool.t]
  in
  let%sub get_forward = Bonsai.yoink forward in
  let%sub get_interpolator = Bonsai.yoink interpolator in
  let%sub get_things_started =
    let%arr animate = animate
    and get_forward = get_forward
    and get_interpolator = get_interpolator
    and set_forward = set_forward in
    let rec switch_directions () =
      let%bind.Effect forward =
        match%bind.Effect get_forward with
        | Active forward -> Effect.return forward
        | Inactive -> Effect.never
      in
      let%bind.Effect interpolator =
        match%bind.Effect get_interpolator with
        | Active interpolator -> Effect.return interpolator
        | Inactive -> Effect.never
      in
      let%bind.Effect () = set_forward (not forward) in
      let target = if forward then 100.0 else 0.0 in
      let duration = `For (Time_ns.Span.of_sec 0.5) in
      animate ~with_:interpolator ~after_finished:(switch_directions ()) duration target
    in
    switch_directions ()
  in
  let%sub () = Bonsai.Edge.lifecycle ~on_activate:get_things_started () in
  let%arr value = value
  and text_picker = text_picker
  and interpolator_form = interpolator_form in
  let margin = Vdom.Attr.style (Css_gen.margin_left (`Px_float value)) in
  let color =
    let v = Float.to_int (value /. 100.0 *. 255.0) in
    Vdom.Attr.style (Css_gen.color (`RGBA (Css_gen.Color.RGBA.create ~r:v ~g:v ~b:v ())))
  in
  let text = Form.value_or_default text_picker ~default:"Marquee" in
  Vdom.Node.div
    [ Form.view_as_vdom text_picker
    ; Form.view_as_vdom interpolator_form
    ; Vdom.Node.h1 ~attrs:[ margin ] [ Vdom.Node.text text ]
    ; Vdom.Node.h1 ~attrs:[ color ] [ Vdom.Node.text text ]
    ]
;;

let () = Bonsai_web.Start.start component
