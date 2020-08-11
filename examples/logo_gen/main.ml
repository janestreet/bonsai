open! Core_kernel
open Bonsai_web
open Virtual_dom_svg
module Form = Bonsai_form_experimental

module Config = struct
  type t =
    { ring_thickness : float
    ; spacer_thickness : float
    ; inner_gap : float
    ; outer_gap : float
    ; roundness : float
    ; start_radius : float
    }
  [@@deriving fields]

  let default =
    Fields.create
      ~ring_thickness:50.0
      ~spacer_thickness:40.0
      ~inner_gap:35.0
      ~outer_gap:40.0
      ~roundness:10.0
      ~start_radius:100.0
  ;;
end

let build_shape (config : Config.t) should_debug (others, radius) =
  let a_ord = Char.to_int 'a' |> Float.of_int in
  function
  | ' ' -> others, radius +. config.spacer_thickness +. config.ring_thickness
  | char ->
    let r2 = radius in
    let r1 = r2 +. config.ring_thickness in
    let debug_view, path =
      Logo_gen.circle
        ~r1
        ~r2
        ~gap1:config.outer_gap
        ~gap2:config.inner_gap
        ~rounding:config.roundness
    in
    let out =
      if should_debug
      then debug_view
      else (
        let rad_to_deg x = x *. (180.0 /. Float.pi) in
        let rotate_neg_90 x = x -. (Float.pi /. 2.0) in
        let radian_of_mul x = x *. Float.pi *. 2.0 in
        let rotation =
          (Float.of_int (Char.to_int char) -. a_ord) /. 26.0
          |> radian_of_mul
          |> rotate_neg_90
          |> rad_to_deg
        in
        Node.path
          [ Attr.transform [ Attr.Rotate { x = 0.0; y = 0.0; a = `Deg rotation } ]
          ; Attr.fill (`Hex "#000000")
          ; Attr.d path
          ]
          [])
    in
    out :: others, r1 +. config.spacer_thickness
;;

let shape_view text should_debug config =
  let open Bonsai.Let_syntax in
  return
  @@ let%map text = text
  and should_debug = should_debug
  and config : Config.t Bonsai.Value.t = config in
  let elts, size =
    text
    |> String.to_list
    |> List.fold ~init:([], config.start_radius) ~f:(build_shape config should_debug)
  in
  Node.svg
    [ Vdom.Attr.create "xmlns" "http://www.w3.org/2000/svg"
    ; Vdom.Attr.style (Css_gen.create ~field:"min-width" ~value:"50vw")
    ; Attr.viewbox
        ~min_x:(-.size)
        ~min_y:(-.size)
        ~width:(size *. 2.0)
        ~height:(size *. 2.0)
    ]
    elts
;;

let config =
  let open Form in
  let open Combine.Let_syntax in
  let o default field =
    text_input ~default
    |> Validated.make_via_string (module Float)
    |> Combine.lift ~f:field
  in
  let%map ring_thickness = o "50.0" Config.ring_thickness
  and spacer_thickness = o "40.0" Config.spacer_thickness
  and inner_gap = o "35.0" Config.inner_gap
  and outer_gap = o "45.0" Config.outer_gap
  and roundness = o "10.0" Config.roundness
  and start_radius = o "90.0" Config.start_radius in
  let value =
    let%map.Or_error ring_thickness = ring_thickness.value
    and spacer_thickness = spacer_thickness.value
    and inner_gap = inner_gap.value
    and outer_gap = outer_gap.value
    and roundness = roundness.value
    and start_radius = start_radius.value in
    Config.Fields.create
      ~ring_thickness
      ~spacer_thickness
      ~inner_gap
      ~outer_gap
      ~roundness
      ~start_radius
  in
  let view =
    let o name view =
      Vdom.Node.tr
        []
        [ Vdom.Node.td [] [ Vdom.Node.text name ]; Vdom.Node.td [] [ view ] ]
    in
    Vdom.Node.table
      []
      [ o "ring thickness" ring_thickness.view
      ; o "spacer thickness" spacer_thickness.view
      ; o "inner gap" inner_gap.view
      ; o "outer_gap" outer_gap.view
      ; o "roundness" roundness.view
      ; o "start radius" start_radius.view
      ]
  in
  { Product.With_view.view; value }
;;

let config = config (Bonsai.Value.return ())

let component =
  let open Form.Product in
  let open Bonsai.Let_syntax in
  let%sub text_input = Form.text_input ~default:"jsc" (Bonsai.Value.return ()) in
  let%sub debug =
    Form.checkbox_input ~label:"debug" ~default:false () (Bonsai.Value.return ())
  in
  let%sub config = config in
  let%sub svg =
    shape_view
      (let%map text_input = text_input in
       text_input.value.value)
      (let%map debug = debug in
       debug.value.value)
      (let%map config = config in
       match config.value.value with
       | Error _ -> Config.default
       | Ok config -> config)
  in
  return
  @@ let%map text_input = text_input
  and debug = debug
  and config = config
  and svg = svg in
  let svg_text =
    svg
    |> Virtual_dom_test_helpers.Node_helpers.unsafe_convert_exn
    |> Virtual_dom_test_helpers.Node_helpers.to_string_html
    |> sprintf {|<?xml version="1.0" encoding="UTF-8" standalone="no"?>
%s|}
    |> Vdom.Node.text
    |> List.return
    |> Vdom.Node.pre
         [ Vdom.Attr.style
             Css_gen.(max_width (`Vw (Percent.of_string "90%")) @> overflow `Auto)
         ]
  in
  let flex_with_space =
    Css_gen.(
      create ~field:"display" ~value:"flex"
      @> create ~field:"justify-content" ~value:"space-around")
  in
  let control_panel =
    Vdom.Node.div [] [ text_input.value.view; debug.value.view; config.value.view ]
  in
  Vdom.Node.div
    []
    [ Vdom.Node.div [ Vdom.Attr.style flex_with_space ] [ control_panel; svg ]
    ; svg_text
    ]
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
