open! Core
open! Bonsai_web

let components =
  let open! Bonsai.Let_syntax in
  let open! Bonsai_web_ui_freeform_multiselect in
  let%sub control =
    Freeform_multiselect.create ~placeholder:"Enter something here.." ()
  in
  return
  @@ let%map selected, control, (_ : String.Set.t -> unit Ui_effect.t) = control in
  let have_you_selected_something =
    match Set.to_list selected with
    | []       -> Vdom.Node.none
    | selected ->
      Vdom.Node.p
        [ Vdom.Node.text ("You've selected: " ^ String.concat ~sep:", " selected) ]
  in
  Vdom.Node.section
    [ Vdom.Node.h4 [ Vdom.Node.text "Test out the freeform multiselect!" ]
    ; have_you_selected_something
    ; control
    ]
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" components
;;
