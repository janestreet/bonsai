open! Core
open! Import
module Style = Tabs_style

let make ~attr ~per_tab_attr ~on_change ~equal ~active tabs =
  let attr = Vdom.Attr.many [ Style.tab_container; attr ] in
  List.map tabs ~f:(fun (i, tab) ->
    let is_active = equal active i in
    let classes =
      if is_active then [ Style.tab_button; Style.selected ] else [ Style.tab_button ]
    in
    let on_click =
      Vdom.Attr.on_click (fun _ ->
        if equal i active then Effect.Ignore else on_change ~from:active ~to_:i)
    in
    let attr =
      Vdom.Attr.many [ Vdom.Attr.many classes; on_click; per_tab_attr i ~is_active ]
    in
    Vdom.Node.div ~attr [ tab ])
  |> View.hbox ~attr ~gap:(`Em_float 0.5)
;;
