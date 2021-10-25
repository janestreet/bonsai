open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Animation = Bonsai_experimental_animation
module Form = Bonsai_web_ui_form

let component =
  let%sub interpolator_form =
    Form.Elements.Dropdown.enumerable
      [%here]
      (module Animation.Interpolator)
      ~init:`First_item
  in
  let%sub text_picker = Form.Elements.Textbox.string [%here] in
  let%sub text_picker =
    text_picker |> Form.Dynamic.with_default (Bonsai.Value.return "Marquee!")
  in
  let interpolator =
    interpolator_form >>| Form.value_or_default ~default:Animation.Interpolator.Linear
  in
  let%sub { value; animate } =
    Animation.Advanced.make
      ~fallback:(Value.return 0.0)
      ~interpolate:Animation.Interpolatable.float
  in
  let%sub forward, set_forward = Bonsai.state [%here] (module Bool) ~default_model:true in
  let%sub get_forward = Bonsai_extra.yoink forward in
  let%sub get_interpolator = Bonsai_extra.yoink interpolator in
  let%sub get_things_started =
    let%arr animate = animate
    and get_forward = get_forward
    and get_interpolator = get_interpolator
    and set_forward = set_forward in
    let rec switch_directions () =
      let%bind.Effect forward = get_forward in
      let%bind.Effect interpolator = get_interpolator in
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
  Vdom.Node.div
    [ Form.view_as_vdom text_picker
    ; Form.view_as_vdom interpolator_form
    ; Vdom.Node.h1
        ~attr:(Vdom.Attr.style Css_gen.(margin_left (`Px_float value)))
        [ Vdom.Node.text (Form.value_or_default text_picker ~default:"Marquee") ]
    ]
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
