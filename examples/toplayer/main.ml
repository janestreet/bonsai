open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax
module Gallery = Bonsai_web_ui_gallery

module Popover = struct
  let name = "Popover"

  let description =
    {|A popover is positioned relative to a target element. `Vdom_toplayer`'s popovers
    are always displayed; for "sometimes open" popovers, use Bonsai-state-backed
    `Bonsai_web_ui_toplayer` popovers.

    We can use the `~position` and `~alignment` attributes to control where the tooltip
    is placed. By default, `Auto` and `Center` are used, which will place the tooltip on the side
    with the most space available.

    The optional `arrow` argument allows us to place a "pointer" element from the popover
    to the anchor. In this example, we've added tiny arrows to show how the arrow pointer
    rotates. |}
  ;;

  let view _graph =
    [%demo
      let popover position alignment =
        Vdom_toplayer.popover
          ~position
          ~alignment
          ~offset:{ main_axis = 6.; cross_axis = 0. }
          ~arrow:
            (Vdom.Node.div
               ~attrs:
                 [ [%css
                     {|background-color: lightskyblue; height: 8px; width:8px; transform: rotate(45deg);|}]
                 ]
               [ Vdom.Node.div
                   ~attrs:
                     [ [%css
                         {|top: -10px; position: relative; transform: rotate(-45deg)|}]
                     ]
                   [ View.text "↑" ]
               ])
          (Vdom.Node.div
             ~attrs:
               [ [%css
                   {|background-color: lightskyblue; border: 1px solid black; padding: 4px|}]
               ]
             [ View.text "Hi, I am a popover" ])
      in
      Vdom.Node.div
        ~attrs:[ popover Top Center; popover Left End ]
        [ View.text "oooo, popovers!" ]]
    |> return
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Popover_virtual = struct
  let name = "Virtually-positioned Popovers"

  let description =
    {|You can also position popovers relative to a "virtual" anchor, which is typically
    a bounding box or coordinate. |}
  ;;

  let view graph =
    let vdom, demo =
      [%demo
        let coords, set_coords = Bonsai.state_opt graph in
        let contents =
          match%sub coords with
          | None -> return [ Vdom.Node.text "Click to place!" ]
          | Some (x, y) ->
            let%arr x = x
            and y = y
            and set_coords = set_coords in
            let anchor = Floating_positioning_new.Anchor.of_coordinate ~x ~y in
            [ Vdom_toplayer.popover_custom
                ~position:Right
                ~alignment:Start
                ~popover_content:
                  (Vdom.Node.div
                     ~attrs:
                       [ [%css
                           {|background-color: lightskyblue; border: 1px solid black; padding: 4px|}]
                       ]
                     [ View.text "Click around to move me!" ])
                anchor
            ; Vdom.Node.button
                ~attrs:
                  [ Vdom.Attr.on_click (fun _ ->
                      Effect.Many [ set_coords None; Effect.Stop_propagation ])
                  ]
                [ View.text "remove popover" ]
            ]
        in
        let%arr contents = contents
        and set_coords = set_coords in
        Vdom.Node.div
          ~attrs:
            [ [%css {|width: 100%; height: 300px;|}]
            ; Vdom.Attr.on_click (fun evt ->
                set_coords
                  (Some (evt##.clientX |> Int.to_float, evt##.clientY |> Int.to_float)))
            ]
          contents]
    in
    let%arr vdom = vdom in
    vdom, demo
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Tooltip = struct
  let name = "Tooltip"

  let description =
    {|Tooltips are hover-triggered popovers. Only one tooltip may be open at a time,
    unless they are nested. Also, unlike popovers, only one tooltip may be attached to
    any given element.
    |}
  ;;

  let view _graph =
    [%demo
      let tooltip position alignment =
        Vdom_toplayer.tooltip
          ~position
          ~alignment
          ~offset:{ main_axis = 6.; cross_axis = 0. }
          ~hide_grace_period:(Time_ns.Span.of_int_ms 100)
          ~hoverable_inside:true
          ~arrow:
            (Vdom.Node.div
               ~attrs:
                 [ [%css
                     {|background-color: lightskyblue; height: 8px; width:8px; transform: rotate(45deg);|}]
                 ]
               [ Vdom.Node.div
                   ~attrs:
                     [ [%css
                         {|top: -10px; position: relative; transform: rotate(-45deg)|}]
                     ]
                   [ View.text "↑" ]
               ])
          (Vdom.Node.div
             ~attrs:
               [ [%css
                   {|background-color: lightskyblue; border: 1px solid black; padding: 4px|}]
               ]
             [ View.text "Hi, I am a tooltip" ])
      in
      View.vbox
        ~gap:(`Px 10)
        [ View.hbox
            ~main_axis_alignment:Space_between
            ~gap:(`Px 10)
            [ Vdom.Node.div ~attrs:[ tooltip Top Start ] [ View.text "Top, start" ]
            ; Vdom.Node.div ~attrs:[ tooltip Top Center ] [ View.text "Top, center" ]
            ; Vdom.Node.div ~attrs:[ tooltip Top End ] [ View.text "Top, end" ]
            ]
        ; View.hbox
            ~main_axis_alignment:Space_between
            ~gap:(`Px 10)
            [ Vdom.Node.div ~attrs:[ tooltip Bottom Start ] [ View.text "Bottom, start" ]
            ; Vdom.Node.div
                ~attrs:[ tooltip Bottom Center ]
                [ View.text "Bottom, center" ]
            ; Vdom.Node.div ~attrs:[ tooltip Bottom End ] [ View.text "Bottom, end" ]
            ]
        ; View.hbox
            ~main_axis_alignment:Space_between
            ~gap:(`Px 10)
            [ Vdom.Node.div ~attrs:[ tooltip Left Start ] [ View.text "Left, start" ]
            ; Vdom.Node.div ~attrs:[ tooltip Left Center ] [ View.text "Left, center" ]
            ; Vdom.Node.div ~attrs:[ tooltip Left End ] [ View.text "Left, end" ]
            ]
        ; View.hbox
            ~main_axis_alignment:Space_between
            ~gap:(`Px 10)
            [ Vdom.Node.div ~attrs:[ tooltip Right Start ] [ View.text "Right, start" ]
            ; Vdom.Node.div ~attrs:[ tooltip Right Center ] [ View.text "Right, center" ]
            ; Vdom.Node.div ~attrs:[ tooltip Right End ] [ View.text "Right, end" ]
            ]
        ]]
    |> return
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Tooltip_hoverable = struct
  let name = "Hoverable Tooltips"

  let description =
    {|The `hoverable_inside` argument controls whether tooltips will stay around if
      hovered inside. You'll need to also provide a `hide_grace_period`, so the user has
      enough time to move their mouse into the tooltip. |}
  ;;

  let view _graph =
    [%demo
      let tooltip ~hoverable_inside =
        Vdom_toplayer.tooltip
          ~show_delay:(Time_ns.Span.of_int_ms 100)
          ~hide_grace_period:(Time_ns.Span.of_int_ms 100)
          ~hoverable_inside
          (Vdom.Node.div
             ~attrs:[ [%css {|border: 1px solid black; padding: 4px|}] ]
             [ View.text "Hi, I am a tooltip" ])
      in
      View.hbox
        ~gap:(`Px 10)
        [ View.vbox
            ~attrs:[ tooltip ~hoverable_inside:false ]
            [ View.text "Show on hover" ]
        ; View.vbox
            ~attrs:[ tooltip ~hoverable_inside:true ]
            [ View.text "Show on hover (interactive)" ]
        ]]
    |> return
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Tooltip_nested = struct
  let name = "Nested tooltips"
  let description = {| Demonstration of nested tooltips, controlled via hover.|}

  let view _ =
    [%demo
      let make_tooltip content =
        Vdom_toplayer.tooltip
          ~position:Right
          ~alignment:Start
          ~offset:{ main_axis = 2.; cross_axis = 0. }
          ~show_delay:(Time_ns.Span.of_int_ms 100)
          ~hide_grace_period:(Time_ns.Span.of_int_ms 100)
          ~hoverable_inside:true
          (Vdom.Node.div ~attrs:[ [%css {|padding: 0; border: none|}] ] content)
      in
      let menu_element ?(tooltip_content = []) content =
        let tooltip =
          match tooltip_content with
          | [] -> Vdom.Attr.empty
          | content -> make_tooltip content
        in
        View.hbox
          ~attrs:[ [%css {|padding: 4px; border: 1px solid black|}]; tooltip ]
          [ content ]
      in
      menu_element
        (View.text "Hover for menu")
        ~tooltip_content:
          [ View.vbox
              [ menu_element
                  (View.text "Hover for Submenu 1")
                  ~tooltip_content:
                    [ menu_element
                        (View.text "Nested thrice!")
                        ~tooltip_content:[ menu_element (View.text "hi") ]
                    ]
              ; menu_element
                  (View.text "Hover for Submenu 2")
                  ~tooltip_content:
                    [ menu_element
                        (View.text
                           "It would be nice to demonstrate how this all would look if \
                            there was quite a lot of text. I'm not sure what the best \
                            way to do this is, but maybe eventually something will come \
                            to mind? ")
                        ~tooltip_content:
                          [ menu_element
                              (View.text "level 4")
                              ~tooltip_content:
                                [ menu_element (View.text "Ok, that's enough") ]
                          ]
                    ]
              ]
          ]]
    |> Bonsai.return
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

let component graph =
  let%sub theme, theme_picker = Gallery.Theme_picker.component () graph in
  let view =
    Gallery.make_sections
      ~theme_picker
      [ ( "Vdom Toplayer"
        , {| vdom_toplayer provides basic, vdom-only primitives used to implement
             `bonsai_web_ui_toplayer`. This library is intended for component library
             and theme authors. |}
        , [ Gallery.make_demo (module Popover)
          ; Gallery.make_demo (module Popover_virtual)
          ; Gallery.make_demo (module Tooltip)
          ; Gallery.make_demo (module Tooltip_hoverable)
          ; Gallery.make_demo (module Tooltip_nested)
          ] )
      ]
  in
  View.Theme.set_for_app theme view graph
;;

let () =
  Async_js.init ();
  Bonsai_web.Start.start component
;;
