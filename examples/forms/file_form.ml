open! Core
open Bonsai_web.Cont
open Bonsai.Let_syntax

let form graph =
  let file_picker =
    Bonsai_web_ui_form.With_automatic_view.Elements.File_select.single () graph
  in
  let file_from_form =
    let%arr file_picker = file_picker in
    Bonsai_web_ui_form.With_automatic_view.value file_picker |> Or_error.ok
  in
  let result = Bonsai_web_ui_file.Read_on_change.create_single_opt file_from_form graph in
  let result =
    match%sub result with
    | None -> Bonsai.return Vdom.Node.none
    | Some (_, (Bonsai_web_ui_file.Read_on_change.Status.Starting | In_progress _)) ->
      Bonsai.return (View.text "file still loading")
    | Some (filename, Complete (Error error)) ->
      let%arr error = error
      and filename = filename in
      Vdom.Node.sexp_for_debugging [%message filename ~_:(error : Error.t)]
    | Some (filename, Complete (Ok contents)) ->
      let%arr filename = filename
      and contents = contents in
      Vdom.Node.sexp_for_debugging [%message filename (contents : Bigstring.t)]
  in
  let%arr result = result
  and file_picker = file_picker in
  View.vbox
    [ Vdom.Node.h1 [ View.text "File form" ]
    ; Bonsai_web_ui_form.With_automatic_view.view_as_vdom file_picker
    ; result
    ]
;;
