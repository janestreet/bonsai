open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
module Tabs = Bonsai_web_ui_tabs

module T = struct
  type t =
    | A
    | B
    | C
  [@@deriving sexp, equal, compare, enumerate]
end

let component graph =
  let tab_state = Tabs.tab_state (module T) ~initial:T.A ~equal:[%equal: T.t] graph in
  let contents =
    Tabs.tab_ui
      (module T)
      ~equal:[%equal: T.t]
      tab_state
      ~all_tabs:(Bonsai.return T.all)
      ~f:(fun ~change_tab tab graph ->
        Bonsai.enum
          (module T)
          ~match_:tab
          ~with_:(fun tab _graph ->
            match tab with
            | A ->
              let%arr change_tab = change_tab in
              Vdom.Node.button
                ~attrs:[ Vdom.Attr.on_click (fun _ -> change_tab T.C) ]
                [ Vdom.Node.text "jump to c" ]
            | B -> Bonsai.return (Vdom.Node.text "why are you even here")
            | C -> Bonsai.return (Vdom.Node.text "hello!"))
          graph)
      graph
  in
  let%arr contents = contents in
  Tabs.Result.combine_trivially contents
;;

let () = Bonsai_web.Start.start component
