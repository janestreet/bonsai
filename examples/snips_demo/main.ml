open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
module Snips = Bonsai_experimental_snips

let main graph =
  let open Snips.Infix in
  let theme = View.Theme.current graph in
  let%arr theme = theme in
  let colors =
    let f = Css_gen.Color.to_string_css in
    let c = View.constants theme in
    Style.Variables.set
      ~header_bg:(f c.extreme.background)
      ~border:(f c.extreme_primary_border)
      ()
  in
  let tabs =
    View.tabs
      theme
      ~attrs:[ Style.tabs ]
      ~equal:[%equal: string]
      ~active:"live"
      ~on_change:(fun ~from:_ ~to_:_ -> Effect.Ignore)
      (List.map [ "live"; "historical" ] ~f:(fun s -> s, Vdom.Node.text s))
  in
  let table =
    Snips.top ~attr:Style.table_header (Vdom.Node.text "table-header")
    |+| Snips.body
          ~attr:Style.table
          (Vdom.Node.div ~attrs:[ Style.table2 ] [ Vdom.Node.text "table" ])
  in
  let layout =
    Snips.top
      (View.devbar
         ~attrs:[ Vdom.Attr.style (Css_gen.create ~field:"line-height" ~value:"1.25rem") ]
         theme
         "DEV")
    |+| Snips.top
          ~attr:Style.header
          (View.hbox
             ~gap:(`Em_float 0.5)
             ~cross_axis_alignment:Baseline
             [ Vdom.Node.text "Snips"; tabs ])
    |+| Snips.left
          ~size:(`Min_max (`Em_float 10.0, `Auto))
          ~attr:Style.sidebar
          (Vdom.Node.text "sidebar")
    |+| Snips.bottom ~attr:Style.footer (Vdom.Node.text "footer")
    |+| Snips.split_h
          [ Snips.split_v [ table; table; table ]
          ; Snips.body ~attr:Style.details (Vdom.Node.text "details")
          ]
  in
  print_s [%message (layout : _ Snips.t)];
  Snips.render ~container_attr:colors layout
;;

let () =
  Async_js.init ();
  main
  |> View.Theme.set_for_app (Bonsai.return (Kado.theme ~version:Bleeding ()))
  |> Bonsai_web.Start.start
;;
