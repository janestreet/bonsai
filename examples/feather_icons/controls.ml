open! Core
open! Import

type t =
  { size : int
  ; stroke_width : float
  ; stroke : [ `Hex of string ]
  ; fill : [ `Hex of string ] option
  }
[@@deriving sexp, fields]

let default = { size = 24; stroke_width = 2.; stroke = `Hex "#000000"; fill = None }

module Range =
  [%css.raw
    {|
.class_ {
  height: 4px;
  cursor: pointer;
  appearance: none;
  background-color: #d1d5da;
  width: 100%;
}
       |}]

let size_slider =
  Form.Elements.Range.int
    ~extra_attrs:(Value.return [ Vdom.Attr.class_ Range.class_ ])
    [%here]
    ~min:12
    ~max:100
    ~default:default.size
    ~step:8
    ()
;;

let stroke_width_slider =
  Form.Elements.Range.float
    ~extra_attrs:(Value.return [ Vdom.Attr.class_ Range.class_ ])
    [%here]
    ~min:0.5
    ~max:3.
    ~default:default.stroke_width
    ~step:0.25
    ()
;;

module Color_input =
  [%css.raw
    {|
.class_ {
  cursor: pointer;
  height: 3em;
  width: 100%;
}
       |}]

let display_none = Vdom.Attr.style (Css_gen.display `None)

let color_input ?(display = Value.return true) () =
  let classes_ = Vdom.Attr.classes [ Card_like.class_; Color_input.class_ ] in
  let%sub extra_attr =
    let%arr display = display in
    if display then classes_ else Vdom.Attr.(classes_ @ display_none)
  in
  Form.Elements.Color_picker.hex ~extra_attr [%here]
;;

module Style =
  [%css.raw
    {|
.header {
  display: flex;
  justify-content: space-between;
  align-items: center;
}

.reset {
  cursor: pointer;
}

.label {
  font-size: 14px;
}

.control_container {
  display: flex;
  flex-direction: column;
  gap: 8px;
}

.controls {
  display: flex;
  flex-direction: column;
  align-items: stretch;
  flex: 0 0 200px;
  gap: 16px;
}
|}]

module Fill = struct
  type t =
    { value : [ `Hex of string ] option
    ; reset : unit Effect.t
    ; view : Vdom.Node.t
    }

  module Fill =
    [%css.raw
      {|
.class_ {
  display: flex;
  justify-content: space-between;
}
|}]

  let component : t Computation.t =
    (* Equal to --js-primary-color *)
    let default_fill_color = `Hex "#2085ef" in
    let%sub fill_toggle = Form.Elements.Toggle.bool ~default:false () in
    let%sub fill_on =
      let%arr fill_toggle = fill_toggle in
      Form.value_or_default fill_toggle ~default:false
    in
    let%sub fill_input =
      let%sub form = color_input ~display:fill_on () in
      Form.Dynamic.with_default (Value.return default_fill_color) form
    in
    let%arr fill_toggle = fill_toggle
    and fill_on = fill_on
    and fill_input = fill_input in
    let value =
      match fill_on with
      | false -> None
      | true ->
        (match Form.value fill_input with
         | Error _ -> default.fill
         | Ok color -> Some color)
    in
    let view =
      Vdom.Node.div
        ~attr:(Vdom.Attr.class_ Style.control_container)
        (Vdom.Node.div
           ~attr:(Vdom.Attr.class_ Fill.class_)
           [ Vdom.Node.label
               ~attr:(Vdom.Attr.class_ Style.label)
               [ Vdom.Node.text "Fill" ]
           ; Form.view_as_vdom fill_toggle
           ]
         :: (Form.view fill_input |> Form.View.to_vdom_plain))
    in
    let reset =
      Effect.Many [ Form.set fill_toggle false; Form.set fill_input default_fill_color ]
    in
    { value; reset; view }
  ;;
end

let component =
  let%sub size_slider = size_slider in
  let%sub stroke_width_slider = stroke_width_slider in
  let%sub stroke_input = color_input () in
  let%sub fill = Fill.component in
  let%arr size_slider = size_slider
  and stroke_width_slider = stroke_width_slider
  and stroke_input = stroke_input
  and { value = fill; reset = reset_fill; view = fill_view } = fill in
  (* We could have several of these forms using Form.Typed.Record, but
     refrained in order to retain control over the UI layout. *)
  let t =
    let size = Form.value_or_default size_slider ~default:default.size in
    let stroke_width =
      Form.value_or_default stroke_width_slider ~default:default.stroke_width
    in
    let stroke = Form.value_or_default stroke_input ~default:default.stroke in
    { size; stroke_width; stroke; fill }
  in
  let reset =
    Vdom.Node.button
      ~attr:
        (Vdom.Attr.many
           [ Vdom.Attr.classes [ Card_like.class_; Style.reset ]
           ; Vdom.Attr.on_click (fun _ ->
               let { size; stroke_width; stroke; fill = _ } = default in
               Fields.to_list
                 ~size:(fun _ -> Form.set size_slider size)
                 ~stroke_width:(fun _ -> Form.set stroke_width_slider stroke_width)
                 ~stroke:(fun _ -> Form.set stroke_input stroke)
                 ~fill:(fun _ -> reset_fill)
               |> Effect.Many)
           ])
      [ Vdom.Node.text "Reset" ]
  in
  let header =
    let title = Vdom.Node.h3 [ Vdom.Node.text "Controls" ] in
    Vdom.Node.div ~attr:(Vdom.Attr.class_ Style.header) [ title; reset ]
  in
  let control ~form ~label =
    Vdom.Node.div
      ~attr:(Vdom.Attr.class_ Style.control_container)
      (Vdom.Node.label ~attr:(Vdom.Attr.class_ Style.label) [ Vdom.Node.text label ]
       :: (Form.view form |> Form.View.to_vdom_plain))
  in
  let size = control ~form:size_slider ~label:(sprintf "Size: %dpx" t.size) in
  let stroke_width =
    control ~form:stroke_width_slider ~label:(sprintf "Stroke width: %gpx" t.stroke_width)
  in
  let stroke = control ~form:stroke_input ~label:"Stroke" in
  let view =
    Vdom.Node.div
      ~attr:(Vdom.Attr.class_ Style.controls)
      [ header; size; stroke_width; stroke; fill_view ]
  in
  t, view
;;
