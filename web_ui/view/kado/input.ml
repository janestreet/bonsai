open! Core
open! Import

let generic_input ~constants ~input_attr ~container_attr ~title f =
  let module Style = Input_style in
  let title =
    match title with
    | None -> Vdom.Node.none
    | Some title -> Vdom.Node.legend [ Vdom.Node.span [ Vdom.Node.text title ] ]
  in
  let colors =
    Style.Variables.set
      ~bg:(Css_gen.Color.to_string_css constants.extreme.background)
      ~border:
        (Css_gen.Color.to_string_css constants.View.Constants.extreme_primary_border)
      ~touch:(Css_gen.Color.to_string_css (`Hex "#1BA1F2"))
      ()
  in
  Vdom.Node.fieldset
    ~attr:(Vdom.Attr.many [ Style.container; container_attr; colors ])
    [ title; f input_attr ]
;;

let dropdown ~constants ~input_attr ~container_attr ~title ~on_change ~options =
  generic_input ~constants ~input_attr ~container_attr ~title (fun attr ->
    Vdom.Node.select
      ~attr:
        (Vdom.Attr.many [ attr; Vdom.Attr.on_change (fun _ value -> on_change value) ])
      (List.map options ~f:(fun (value, selected, view) ->
         Vdom.Node.option
           ~attr:
             (Vdom.Attr.many
                [ Vdom.Attr.bool_property "selected" selected; Vdom.Attr.value value ])
           [ view ])))
;;

let extend ~input_attr ~on_change =
  Vdom.Attr.many [ input_attr; Vdom.Attr.on_input (fun _ s -> on_change s) ]
;;

let textbox ~constants ~input_attr ~container_attr ~title ~on_change ~value =
  let input_attr = extend ~input_attr ~on_change in
  generic_input ~constants ~input_attr ~container_attr ~title (fun attr ->
    Vdom.Node.input ~attr:(Vdom.Attr.many [ Vdom.Attr.value_prop value; attr ]) ())
;;

let date ~constants ~input_attr ~container_attr ~title ~on_change ~value =
  let input_attr = Vdom.Attr.many [ input_attr; Vdom.Attr.type_ "date" ] in
  textbox ~constants ~input_attr ~container_attr ~title ~on_change ~value
;;

let datetime ~constants ~input_attr ~container_attr ~title ~on_change ~value =
  let input_attr = Vdom.Attr.many [ input_attr; Vdom.Attr.type_ "datetime-local" ] in
  textbox ~constants ~input_attr ~container_attr ~title ~on_change ~value
;;

let generic_button_box ~constants ~input_attr ~container_attr ~label ~on_change ~checked f
  =
  let module Style = Checkbox_style in
  let colors =
    Style.Variables.set
      ~bg:(Css_gen.Color.to_string_css constants.extreme.background)
      ~border:
        (Css_gen.Color.to_string_css constants.View.Constants.extreme_primary_border)
      ~touch:(Css_gen.Color.to_string_css (`Hex "#1BA1F2"))
      ()
  in
  let input_attr =
    Vdom.Attr.many
      [ input_attr
      ; Vdom.Attr.bool_property "checked" checked
      ; Vdom.Attr.on_change (fun evt _ ->
          let open Js_of_ocaml in
          match Js.Opt.to_option (Js.Opt.bind evt##.target Dom_html.CoerceTo.input) with
          | Some target -> on_change (Js.to_bool target##.checked)
          | (exception _) | None -> on_change (not checked))
      ]
  in
  Vdom.Node.label
    ~attr:(Vdom.Attr.many [ Style.button_box_container; container_attr; colors ])
    [ f input_attr; label ]
;;

let checkbox =
  generic_button_box (fun attr ->
    Vdom.Node.input ~attr:(Vdom.Attr.many [ attr; Vdom.Attr.type_ "checkbox" ]) ())
;;

(* radio-buttons currently look bad, disable for now *)
let _radiobutton =
  generic_button_box (fun attr ->
    Vdom.Node.input ~attr:(Vdom.Attr.many [ attr; Vdom.Attr.type_ "radio" ]) ())
;;

let button_vbox children = Vdom.Node.div ~attr:Checkbox_style.vbox children
