open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

module Pills = struct
  let component
        ~extra_container_attr
        ~extra_pill_attr
        ~to_string
        ~to_list
        ~inject_selected_options
        ~remove_option
        selected_options
    =
    let%arr extra_container_attr = extra_container_attr
    and extra_pill_attr         = extra_pill_attr
    and selected_options        = selected_options
    and inject_selected_options = inject_selected_options
    and to_string               = to_string in
    let pill option =
      let remove_option event =
        if Bonsai_web.am_within_disabled_fieldset event
        then Effect.Ignore
        else (
          let selected_options = remove_option selected_options option in
          inject_selected_options selected_options)
      in
      Vdom.Node.span
        ~attr:
          Vdom.Attr.(
            extra_pill_attr
            @ tabindex 0
            @ create   "data-value" (to_string option)
            @ on_click remove_option
            @ on_keyup (fun ev ->
              match Js_of_ocaml.Dom_html.Keyboard_code.of_event ev with
              | Space | Enter | NumpadEnter | Backspace | Delete -> remove_option ev
              | _ -> Ui_effect.Ignore))
        [ Vdom.Node.text (to_string option ^ " ×") ]
    in
    match to_list selected_options with
    | []               -> Vdom.Node.none
    | selected_options ->
      Vdom.Node.div ~attr:extra_container_attr (List.map selected_options ~f:pill)
  ;;

  let of_list
        ?(extra_container_attr = Value.return Vdom.Attr.empty)
        ?(extra_pill_attr      = Value.return Vdom.Attr.empty)
        ~to_string
        ~inject_selected_options
        selected_options
    =
    let%sub selected_options =
      let%arr selected_options = selected_options in
      List.mapi selected_options ~f:(fun i option -> i, option)
    in
    let%sub inject_selected_options =
      let%arr inject_selected_options = inject_selected_options in
      fun selected_options ->
        List.map selected_options ~f:(fun (_, option) -> option)
        |> inject_selected_options
    in
    let%sub to_string =
      let%arr to_string = to_string in
      fun (_, option) -> to_string option
    in
    component
      ~extra_container_attr
      ~extra_pill_attr
      ~to_string
      ~to_list:Fn.id
      ~inject_selected_options
      ~remove_option:(fun selected_options (index, _) ->
        List.filter selected_options ~f:(fun (i, _) -> i <> index))
      selected_options
  ;;

  let of_set
        ?(extra_container_attr = Value.return Vdom.Attr.empty)
        ?(extra_pill_attr      = Value.return Vdom.Attr.empty)
        ~to_string
        ~inject_selected_options
        selected_options
    =
    component
      ~extra_container_attr
      ~extra_pill_attr
      ~to_string
      ~to_list:Set.to_list
      ~inject_selected_options
      ~remove_option:Set.remove
      selected_options
  ;;
end
