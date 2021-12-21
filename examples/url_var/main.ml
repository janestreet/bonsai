open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Tabs = Bonsai_web_ui_tabs
module Url_var = Bonsai_web_ui_url_var

module T = struct
  type t =
    | A
    | B of string option
    | C
  [@@deriving sexp, equal, compare]

  let parse_exn { Url_var.Components.path; query; fragment = _ } =
    match path, Map.find query "extra" with
    | "a", _ -> A
    | "b", None -> B None
    | "b", Some [ extra ] -> B (Some extra)
    | "c", _ -> C
    | _ -> raise_s [%message "no url match"]
  ;;

  let unparse = function
    | A -> Url_var.Components.create ~path:"a" ()
    | B (Some extra) ->
      let query = String.Map.singleton "extra" [ extra ] in
      Url_var.Components.create ~path:"b" ~query ()
    | B None -> Url_var.Components.create ~path:"b" ()
    | C -> Url_var.Components.create ~path:"c" ()
  ;;
end

let url_var = Url_var.create_exn (module T) ~fallback:T.A
let set_url = Url_var.set_effect url_var

let tab_state =
  let set a = set_url a in
  let%arr current = Url_var.value url_var in
  Tabs.State.create ~current ~set
;;

let component =
  let%sub tab_state = tab_state in
  let%sub extra_state = Bonsai.state_opt [%here] (module String) in
  let%sub all_tabs =
    let%arr extra_state, _ = extra_state in
    [ T.A; T.B extra_state; T.C ]
  in
  let%sub contents =
    Tabs.tab_ui
      (module T)
      tab_state
      ~all_tabs
      ~decorate:
        (Value.return (function
           | T.A -> Vdom.Node.text "Tab A"
           | B None -> Vdom.Node.text "Tab B"
           | B (Some extra) -> Vdom.Node.textf "Tab B: %s" extra
           | C -> Vdom.Node.text "Tab C"))
      ~f:(fun ~change_tab tab ->
        match%sub tab with
        | A ->
          let%arr change_tab = change_tab in
          Vdom.Node.button
            ~attr:(Vdom.Attr.on_click (fun _ -> change_tab T.C))
            [ Vdom.Node.text "jump to c" ]
        | B None -> Bonsai.const (Vdom.Node.text "why are you even here")
        | B (Some extra) ->
          let%arr extra = extra in
          Vdom.Node.textf "b with extra: %s" extra
        | C ->
          let%arr extra, set_extra = extra_state in
          Vdom.Node.input
            ~attr:
              Vdom.Attr.(
                Vdom.Attr.string_property "value" (Option.value extra ~default:"")
                @ Vdom.Attr.on_input (fun _ s ->
                  set_extra (if String.is_empty s then None else Some s)))
            [])
  in
  let%arr contents = contents in
  Tabs.Result.combine_trivially contents
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
