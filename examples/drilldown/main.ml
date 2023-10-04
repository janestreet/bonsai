open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open! Bonsai_web_ui_drilldown

module Style = [%css stylesheet {|
.app {
  width: 600px;
}
|}]

let example : Tree.String.t =
  let open Tree.String in
  let a = { name = "a"; children = Leaf 3 } in
  let b =
    { name = "b"
    ; children =
        Branch [ { name = "ba"; children = Leaf 4 }; { name = "bb"; children = Leaf 1 } ]
    }
  in
  let c =
    { name = "c"
    ; children =
        Branch
          [ { name = "ca"
            ; children =
                Branch
                  [ { name = "caa"; children = Leaf 2 }
                  ; { name = "cab"; children = Leaf 4 }
                  ]
            }
          ; { name = "cb"; children = Branch [ { name = "cba"; children = Leaf 1 } ] }
          ; { name = "cc"; children = Leaf 1 }
          ]
    }
  in
  { name = "all"; children = Branch [ a; b; c ] }
;;

let tree_layout : Vdom.Node.t =
  let open Bonsai_web_ui_tree_layout in
  let rec loop (tree : Tree.String.t) =
    let name = Vdom.Node.text tree.name in
    match tree.children with
    | Leaf _ -> leaf name
    | Branch children -> branch name (List.map children ~f:loop)
  in
  loop example |> to_vdom
;;

let app : Vdom.Node.t Bonsai.Computation.t =
  let module N = Vdom.Node in
  let%map.Computation drilldown_with_breadcrumbs =
    Drilldown_with_breadcrumbs.component (Value.return example)
  in
  let explanation1 =
    N.div
      [ N.text
          "This demo shows off an interactive drilldown component. Click around to drill \
           in."
      ]
  in
  let explanation2 =
    N.div
      [ N.text
          "The key feature is that each child takes up space proportional to the sum of \
           the values of that child's leaves."
      ]
  in
  let sexp =
    N.div
      [ N.text
          "Below, you can see (a sexp of) the tree-like data structure that we're \
           visualizing in the drilldown."
      ; [%sexp_of: Tree.String.t] example |> N.sexp_for_debugging
      ]
  in
  let tree_layout =
    N.div
      [ N.text "And here is a different (static) tree layout to visualize the same thing:"
      ; tree_layout
      ]
  in
  let add_breaks l = List.intersperse ~sep:(N.br ()) l in
  N.div
    ~attrs:[ Style.app ]
    (add_breaks
       [ explanation1
       ; explanation2
       ; N.div [ drilldown_with_breadcrumbs ]
       ; sexp
       ; tree_layout
       ])
;;

let () = Bonsai_web.Start.start app
