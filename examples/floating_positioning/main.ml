open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_automatic_view

module Config = struct
  module Virtual = struct
    type t =
      { x : int
      ; y : int
      ; width : int
      ; height : int
      }
    [@@deriving typed_fields]

    let form_for_field : type a. a Typed_field.t -> Bonsai.graph -> a Form.t Bonsai.t =
      fun typed_field graph ->
      match typed_field with
      | X ->
        Form.Elements.Number.int
          ~default:50
          ~step:25
          ~min:0
          ~allow_updates_when_focused:`Always
          ()
          graph
      | Y ->
        Form.Elements.Number.int
          ~default:100
          ~step:25
          ~min:0
          ~allow_updates_when_focused:`Always
          ()
          graph
      | Width ->
        Form.Elements.Number.int
          ~default:10
          ~step:10
          ~min:0
          ~allow_updates_when_focused:`Always
          ()
          graph
      | Height ->
        Form.Elements.Number.int
          ~default:10
          ~step:10
          ~min:0
          ~allow_updates_when_focused:`Always
          ()
          graph
    ;;

    let label_for_field = `Inferred
  end

  let virtual_form = Form.Typed.Record.make (module Virtual)

  module Anchor = struct
    type t =
      | Dom of int
      | Virtual of Virtual.t
    [@@deriving typed_variants]

    let form_for_variant : type a. a Typed_variant.t -> Bonsai.graph -> a Form.t Bonsai.t =
      fun typed_field graph ->
      match typed_field with
      | Dom ->
        Form.Elements.Number.int
          ~default:0
          ~step:1
          ~allow_updates_when_focused:`Always
          ()
          graph
      | Virtual -> virtual_form graph
    ;;

    let variant_to_string : type a. a Typed_variant.t -> string = function
      | Dom -> "DOM (px per shift)"
      | Virtual -> "Virtual"
    ;;

    let label_for_variant = `Computed variant_to_string
    let initial_choice = `First_constructor
  end

  let anchor_form = Form.Typed.Variant.make (module Anchor)

  module Offset = struct
    type t =
      { main_axis : float
      ; cross_axis : float
      }
    [@@deriving typed_fields]

    let form_for_field : type a. a Typed_field.t -> Bonsai.graph -> a Form.t Bonsai.t =
      fun typed_field graph ->
      match typed_field with
      | Main_axis ->
        Form.Elements.Number.float
          ~default:0.
          ~step:1.
          ~allow_updates_when_focused:`Always
          ()
          graph
      | Cross_axis ->
        Form.Elements.Number.float
          ~default:0.
          ~step:1.
          ~allow_updates_when_focused:`Always
          ()
          graph
    ;;

    let label_for_field = `Inferred
  end

  let offset_form graph =
    let base = Form.Typed.Record.make (module Offset) graph in
    let%map base = base in
    Form.project
      base
      ~parse_exn:(fun { main_axis; cross_axis } ->
        { Floating_positioning_new.Offset.main_axis; cross_axis })
      ~unparse:(fun { main_axis; cross_axis } -> { main_axis; cross_axis })
  ;;

  module One_popover = struct
    type t =
      { position : Floating_positioning_new.Position.t
      ; alignment : Floating_positioning_new.Alignment.t
      ; offset : Floating_positioning_new.Offset.t
      }
    [@@deriving typed_fields]

    let form_for_field : type a. a Typed_field.t -> Bonsai.graph -> a Form.t Bonsai.t =
      fun typed_field graph ->
      match typed_field with
      | Position ->
        Form.Elements.Dropdown.enumerable
          ~init:`First_item
          (module Floating_positioning_new.Position)
          graph
      | Alignment ->
        Form.Elements.Dropdown.enumerable
          ~init:`First_item
          (module Floating_positioning_new.Alignment)
          graph
      | Offset -> offset_form graph
    ;;

    let label_for_field = `Inferred
  end

  let popovers_form = Form.Typed.Record.make_table (module One_popover)

  module Full = struct
    type t =
      { anchor_type : Anchor.t
      ; popovers : One_popover.t list
      }
    [@@deriving typed_fields]

    let form_for_field : type a. a Typed_field.t -> Bonsai.graph -> a Form.t Bonsai.t =
      fun typed_field graph ->
      match typed_field with
      | Popovers -> popovers_form graph
      | Anchor_type -> anchor_form graph
    ;;

    let label_for_field = `Inferred
  end

  let form = Form.Typed.Record.make (module Full)
end

(* Aiming for 60fps... *)
let interval_ms = 16

let get_viewport_dimensions () =
  let open Js_of_ocaml in
  Dom_html.window##.innerWidth, Dom_html.window##.innerHeight
;;

let with_dom_anchor shift popovers graph =
  let anchor_pos, move =
    Bonsai.state_machine1
      ~default_model:(0, 0)
      ~apply_action:(fun _ input (x, y) () ->
        let shift =
          match input with
          | Active shift -> shift
          | Inactive -> 0
        in
        let max_x, max_y = get_viewport_dimensions () in
        (x + shift) mod max_x, (y + shift) mod max_y)
      shift
      graph
  in
  let () =
    Bonsai.Clock.every
      ~when_to_start_next_effect:`Every_multiple_of_period_non_blocking
      (Time_ns.Span.of_int_ms interval_ms)
      (let%map move = move in
       move ())
      graph
  in
  let%map left, top = anchor_pos
  and popovers = popovers in
  let popover_attrs =
    List.mapi popovers ~f:(fun i { Config.One_popover.position; alignment; offset } ->
      Vdom_toplayer.popover
        ~position
        ~alignment
        ~offset
        (Vdom.Node.div
           ~attrs:[ [%css {|border: 1px solid red|}] ]
           [ Vdom.Node.text [%string "Popover %{i#Int}!"] ]))
  in
  Vdom.Node.div
    [ Vdom.Node.div
        ~attrs:
          ([ [%css
               {|position: fixed; left: %{(`Px left)#Css_gen.Length}; top: %{(`Px top)#Css_gen.Length};|}]
           ; [%css {|border: 2px solid black; padding: 5px|}]
           ]
           @ popover_attrs)
        [ Vdom.Node.text "Anchor!!!" ]
    ]
;;

let with_virtual_anchor coords popovers =
  let%map { Config.Virtual.x; y; width; height } = coords
  and popovers = popovers in
  let popover_nodes =
    List.mapi popovers ~f:(fun i { Config.One_popover.position; alignment; offset } ->
      Vdom_toplayer.popover_custom
        ~position
        ~alignment
        ~offset
        ~popover_content:
          (Vdom.Node.div
             ~attrs:[ [%css {|border: 1px solid red|}] ]
             [ Vdom.Node.text [%string "Virtual Popover %{i#Int}!"] ])
        (Floating_positioning_new.Anchor.of_bounding_box
           ~top:(Float.of_int y)
           ~left:(Float.of_int x)
           ~bottom:(Float.of_int (y + height))
           ~right:(Float.of_int (x + width))))
  in
  Vdom.Node.div
    ([ Vdom.Node.div
         ~attrs:
           [ [%css
               {|position: fixed; left: %{(`Px x)#Css_gen.Length}; top: %{(`Px y)#Css_gen.Length}; width: %{(`Px width)#Css_gen.Length}; height: %{(`Px height)#Css_gen.Length};|}]
           ; [%css {|border : 2px dotted green;|}]
           ]
         []
     ]
     @ popover_nodes)
;;

let component graph =
  let form = Config.form graph in
  let body =
    match%sub Bonsai.map form ~f:Form.value with
    | Error e ->
      let%map e = e in
      View.text (Error.to_string_hum e)
    | Ok { anchor_type = Dom shift; popovers } -> with_dom_anchor shift popovers graph
    | Ok { anchor_type = Virtual coords; popovers } -> with_virtual_anchor coords popovers
  in
  let%map form = form
  and body = body in
  let form_div =
    Vdom.Node.div
      ~attrs:[ [%css {|position: fixed; right: 0; top: 0;|}] ]
      [ Form.view_as_vdom form ]
  in
  Vdom.Node.div ~attrs:[ [%css {|&* {box-sizing: border-box;}|}] ] [ form_div; body ]
;;

let () = Bonsai_web.Start.start component
