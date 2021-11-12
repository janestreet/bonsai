open! Core
open! Bonsai_web

(** This control is similar to the typeahead control, differing in the fact that it
    doesn't aim to complete your input. Think of this control as a multi-select that
    you're free to add random values to. *)

let[@warning "-16"] input ?(placeholder = "") ?(value = "") ~extra_attrs ~id ~on_input =
  Vdom.Node.input
    ~attr:
      (Vdom.Attr.many_without_merge
         (extra_attrs
          @ [ Vdom.Attr.type_ "text"
            ; Vdom.Attr.create "list" id
            ; Vdom.Attr.placeholder placeholder
            (* Both Attr.value and Attr.string_property value must be set. The former only affects
               initial control state while the latter affects the control state whilst the form is
               being used. *)
            ; Vdom.Attr.value value
            ; Vdom.Attr.string_property "value" value
            ; Vdom.Attr.on_change (fun _ input -> on_input input)
            ]))
    []
;;

let pills ~selected_options ~on_set_change ~inject_selected_options =
  let pill option =
    let remove_option _ =
      let selected_options = Set.remove selected_options option in
      Ui_effect.Many
        [ on_set_change selected_options; inject_selected_options selected_options ]
    in
    Vdom.Node.span
      ~attr:
        (Vdom.Attr.many_without_merge
           [ Vdom.Attr.tabindex 0
           ; Vdom.Attr.create "data-value" option
           ; Vdom.Attr.on_click remove_option
           ; Vdom.Attr.on_keyup (fun ev ->
               match Js_of_ocaml.Dom_html.Keyboard_code.of_event ev with
               | Space | Enter | NumpadEnter | Backspace | Delete -> remove_option ev
               | _ -> Ui_effect.Ignore)
           ])
      [ Vdom.Node.text (option ^ " ×") ]
  in
  if Set.is_empty selected_options
  then Vdom.Node.none
  else
    Vdom.Node.div
      ~attr:(Vdom.Attr.class_ "bonsai-web-ui-freeform-multiselect-pills")
      (Set.to_list selected_options |> List.map ~f:pill)
;;

let input ~placeholder ~extra_attrs ~split ~id ~selected_options ~on_set_change =
  let open! Bonsai.Let_syntax in
  (* This state is held internally to force the typeahead to clear the text contents
     of the input field when an option is selected. *)
  let%sub select = Bonsai.state [%here] (module String) ~default_model:"" in
  return
  @@ let%map select, inject_select = select
  and selected_options, inject_selected_options = selected_options
  and extra_attrs = extra_attrs
  and id = id
  and on_set_change = on_set_change in
  let on_input user_input =
    let maybe_changed_options =
      split user_input
      |> String.Set.of_list
      |> Set.filter ~f:(fun item -> not (String.strip item |> String.is_empty))
      |> Set.union selected_options
    in
    let ui_events =
      if Set.equal maybe_changed_options selected_options
      then [ inject_select user_input ]
      else [ inject_select ""; on_set_change maybe_changed_options ]
    in
    Ui_effect.Many (inject_selected_options maybe_changed_options :: ui_events)
  in
  input ~extra_attrs ~value:select ~placeholder ~id ~on_input
;;

let create
      ?(extra_attrs = Value.return [])
      ?(placeholder = "")
      ?(on_set_change = Value.return (const Ui_effect.Ignore))
      ?(split = List.return)
      ()
  =
  let open Bonsai.Let_syntax in
  let%sub selected_options =
    Bonsai.state [%here] (module String.Set) ~default_model:String.Set.empty
  in
  let%sub id = Bonsai.path_id in
  let%sub input =
    input ~placeholder ~extra_attrs ~id ~on_set_change ~split ~selected_options
  in
  return
  @@ let%map selected_options, inject_selected_options = selected_options
  and input         = input
  and on_set_change = on_set_change in
  let pills = pills ~selected_options ~on_set_change ~inject_selected_options in
  selected_options, Vdom.Node.div [ input; pills ], inject_selected_options
;;
