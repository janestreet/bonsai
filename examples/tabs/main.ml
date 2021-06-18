open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Tabs = Bonsai_web_ui_tabs

module T = struct
  type t =
    | A
    | B
    | C
  [@@deriving sexp, equal, compare, enumerate]
end

let component =
  let%sub tab_state = Tabs.tab_state (module T) ~initial:T.A in
  let%sub contents =
    Tabs.tab_ui
      (module T)
      tab_state
      ~all_tabs:(Value.return T.all)
      ~f:(fun ~change_tab tab ->
        Bonsai.enum
          (module T)
          ~match_:tab
          ~with_:(function
            | A ->
              return
              @@ let%map change_tab = change_tab in
              Vdom.Node.button
                ~attr:(Vdom.Attr.on_click (fun _ -> change_tab T.C))
                [ Vdom.Node.text "jump to c" ]
            | B -> Bonsai.const (Vdom.Node.text "why are you even here")
            | C -> Bonsai.const (Vdom.Node.text "hello!")))
  in
  return
  @@ let%map contents = contents in
  Tabs.Result.combine_trivially contents
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
