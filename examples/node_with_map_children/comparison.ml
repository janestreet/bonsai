open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax

let to_vdom_node_map ~with_key map graph =
  Bonsai.assoc
    (module Int)
    map
    ~f:(fun key data _graph ->
      let%arr key = key
      and data = data in
      let key = Int.to_string key in
      let text = Vdom.Node.textf "%s" key in
      let key = if with_key then Some key else None in
      Style.chip ?key data text)
    graph
;;

let make_comparison_list node = Vdom.Node.div ~attrs:[ Style.comparison_list ] [ node ]

let view ~tag ~attr nodes graph =
  let nodes_with_key = to_vdom_node_map ~with_key:true nodes graph in
  let nodes_without_key = to_vdom_node_map ~with_key:false nodes graph in
  let%arr nodes_with_key = nodes_with_key
  and nodes_without_key = nodes_without_key
  and tag = tag
  and attr = attr in
  let with_key_via_alist =
    Vdom.Node.create tag ~attrs:[ attr ] (Map.data nodes_with_key)
  in
  let with_key_via_map_with_node_children =
    Vdom_node_with_map_children.make ~tag ~attr nodes_with_key
  in
  let without_key_via_alist =
    Vdom.Node.create tag ~attrs:[ attr ] (Map.data nodes_without_key)
  in
  let without_key_via_map_with_node_children =
    Vdom_node_with_map_children.make ~tag ~attr nodes_without_key
  in
  Vdom.Node.div
    ~attrs:[ Style.color_list; Style.results ]
    [ Vdom.Node.div ~attrs:[ Style.header ] [ Vdom.Node.text "out" ]
    ; Vdom.Node.div
        ~attrs:
          [ Vdom.Attr.id "results"
          ; Vdom.Attr.many [ Style.comparison_container; Style.body ]
          ]
        [ make_comparison_list with_key_via_alist
        ; make_comparison_list without_key_via_alist
        ; make_comparison_list with_key_via_map_with_node_children
        ; make_comparison_list without_key_via_map_with_node_children
        ]
    ]
;;
