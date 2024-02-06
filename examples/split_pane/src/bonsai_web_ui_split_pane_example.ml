open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
module Parameters = Bonsai_web_ui_split_pane.For_testing.Parameters
module Form = Bonsai_web_ui_form.With_automatic_view

module Styles =
[%css
stylesheet
  {|
.container {
  display: flex;
  width: 100%;
  box-sizing: border-box;
}

.demo {
  flex: 1;
}

.parameters {
  margin: 3px;
  padding: 3px;
  border: 1px solid;
  min-width: 300px;
}

.resize_pane {
  width: 600px;
  height: 600px;
  resize: both;
  overflow: auto;
  border: 3px solid;
}

.first_panel, .second_panel {
  display: flex;
  justify-content: center;
  align-items: center;
  box-sizing: border-box;
  font-size: 24px;
  height: 100%;
  width: 100%;
  font: 96px Open Sans, sans-serif;
}

.first_panel {
  background-color: #6e91cc;
}

.second_panel {
  background-color: #6ecc8a;
}

|}]

module Parameters_or_error = struct
  type t = Parameters.t Or_error.t [@@deriving sexp, equal]
end

let create_demo ~parameters =
  let first_panel =
    Value.return (Vdom.Node.div ~attrs:[ Styles.first_panel ] [ Vdom.Node.text "1" ])
  in
  let second_panel =
    Value.return (Vdom.Node.div ~attrs:[ Styles.second_panel ] [ Vdom.Node.text "2" ])
  in
  let%sub pane =
    Bonsai_web_ui_split_pane.For_testing.create_from_parameters
      parameters
      ~first_panel
      ~second_panel
  in
  let%arr pane = pane in
  let view =
    Node.div ~attrs:[ Styles.resize_pane ] [ Bonsai_web_ui_split_pane.to_vdom pane ]
  in
  view, Bonsai_web_ui_split_pane.inject_set_size pane
;;

let create_parameters_form =
  let%sub form = Bonsai_web_ui_auto_generated.form (module Parameters) () in
  let%sub form =
    Bonsai_web_ui_form.With_automatic_view.Dynamic.with_default
      (Value.return Parameters.default)
      form
  in
  let%sub last_ok =
    Bonsai.most_recent_value_satisfying
      ~sexp_of_model:[%sexp_of: Parameters_or_error.t]
      ~equal:[%equal: Parameters_or_error.t]
      (form >>| Form.value)
      ~condition:(function
      | Ok _ -> true
      | Error _ -> false)
  in
  let%arr last_ok = last_ok
  and form = form in
  let form_vdom = Bonsai_web_ui_auto_generated.view_as_vdom form in
  let error =
    match Bonsai_web_ui_form.With_automatic_view.value form with
    | Ok _ -> Node.none
    | Error e -> Node.sexp_for_debugging [%sexp (e : Error.t)]
  in
  let view = Vdom.Node.div [ form_vdom; error ] in
  let parameters =
    match last_ok with
    | Some (Ok parameters) -> parameters
    | None | Some (Error _) -> Parameters.default
  in
  parameters, view
;;

let app =
  let%sub parameters, parameters_form = create_parameters_form in
  let%sub demo, inject_set_size = create_demo ~parameters in
  let%sub inject_reset =
    let%arr parameters = parameters
    and inject_set_size = inject_set_size in
    inject_set_size parameters.initial_size
  in
  let%arr parameters_form = parameters_form
  and demo = demo
  and inject_reset = inject_reset in
  Node.div
    ~attrs:[ Styles.container ]
    [ Node.div ~attrs:[ Styles.demo ] [ demo ]
    ; Node.div
        ~attrs:[ Styles.parameters ]
        [ parameters_form
        ; Node.button
            ~attrs:[ Attr.on_click (fun _ -> inject_reset) ]
            [ Node.text "Reset to initial size" ]
        ]
    ]
;;
