open! Core
open! Bonsai_web

(** This provides an implementation of [inputs] with associated [datalist] element.

    See the full spec here:

    [datalist]: https://developer.mozilla.org/en-US/docs/Web/HTML/Element/datalist
    [list attr]: https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input#htmlattrdeflist

    As to why this alternate typeahead implementation exists:

    Existing typeahead controls implement a custom select using divs. While this isn't
    strictly a negative (native select dropdowns cannot be styled), using the native
    controls where possible is nice.
*)

let path =
  let open Bonsai.Let_syntax in
  let%sub path = Bonsai.Private.path in
  return
  @@ let%map path = path in
  Bonsai.Private.Path.to_unique_identifier_string path
;;

module Search = struct
  type 'a t =
    | Nothing_found
    | Partial_match of 'a
    | Only_exact_matches_allowed

  let find ~to_string ~haystack ~needle =
    List.fold_until
      haystack
      ~init:Nothing_found
      ~f:(fun state value ->
        let value_string = to_string value in
        (* For a full string-equal, we always end the search *)
        if String.Caseless.equal value_string needle
        then Stop (Some value)
        else if String.Caseless.is_substring value_string ~substring:needle
        then (
          match state with
          (* Nothing -> partial -> partial-match*)
          | Nothing_found   -> Continue (Partial_match value)
          (* Two partial matches means that we continue, but will only succeed if
             finding an exact-match *)
          | Partial_match _ -> Continue Only_exact_matches_allowed
          (* If we are in the only-exact-matches state, don't change it *)
          | Only_exact_matches_allowed -> Continue Only_exact_matches_allowed)
        else Continue state)
      ~finish:(function
        | Only_exact_matches_allowed -> None
        | Nothing_found              -> None
        | Partial_match value        -> Some value)
  ;;
end

let[@warning "-16"] input
                      ?(placeholder = "")
                      ?(value       = "")
                      ~extra_attrs
                      ~to_string
                      ~id
                      ~all_options
                      ~on_input
  =
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
            ; Vdom.Attr.on_change (fun _ input ->
                let maybe_t =
                  Search.find ~to_string ~needle:input ~haystack:all_options
                in
                on_input maybe_t input)
            ]))
    []
;;

let datalist ?filter_options_by ~id ~all_options ~to_string () =
  let option_of_t t =
    Vdom.Node.option
      ~attr:(Vdom.Attr.value (to_string t))
      [ Vdom.Node.text (to_string t) ]
  in
  let all_options =
    match filter_options_by with
    | None -> List.map ~f:option_of_t all_options
    | Some filter_options_by ->
      List.filter_map all_options ~f:(fun item ->
        if filter_options_by item then Some (option_of_t item) else None)
  in
  Vdom.Node.create "datalist" ~attr:(Vdom.Attr.id id) all_options
;;

let create
      (type t)
      ?(extra_attrs = Value.return [])
      ?placeholder
      ?on_select_change
      ?to_string
      (module M : Bonsai.Model with type t = t)
      ~all_options
  =
  let open! Bonsai.Let_syntax in
  let to_string =
    Option.value
      to_string
      ~default:(Bonsai.Value.return (fun a -> a |> M.sexp_of_t |> Sexp.to_string_hum))
  in
  let on_select_change =
    Option.value
      on_select_change
      ~default:(Value.return (fun (_ : M.t option) -> Ui_effect.Ignore))
  in
  let%sub selected = Bonsai.state_opt [%here] (module M) in
  let%sub id = path in
  return
  @@ let%map selected, inject_selected = selected
  and to_string        = to_string
  and on_select_change = on_select_change
  and id               = id
  and extra_attrs      = extra_attrs
  and all_options      = all_options in
  let on_input t (_ : string) =
    Ui_effect.Many [ inject_selected t; on_select_change t ]
  in
  let datalist = datalist ~to_string ~id ~all_options () in
  let input =
    input
      ?placeholder
      ~extra_attrs
      ~id
      ~all_options
      ~on_input
      ~to_string
      ~value:(Option.value_map ~default:"" selected ~f:to_string)
  in
  selected, Vdom.Node.div [ input; datalist ], inject_selected
;;

let pills ~to_string ~selected_options ~on_set_change ~inject_selected_options =
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
           ; Vdom.Attr.create "data-value" (to_string option)
           ; Vdom.Attr.on_click remove_option
           ; Vdom.Attr.on_keyup (fun ev ->
               match Js_of_ocaml.Dom_html.Keyboard_code.of_event ev with
               | Space | Enter | NumpadEnter | Backspace | Delete -> remove_option ev
               | _ -> Ui_effect.Ignore)
           ])
      [ Vdom.Node.text (to_string option ^ " ×") ]
  in
  if Set.is_empty selected_options
  then Vdom.Node.none
  else
    Vdom.Node.div
      ~attr:(Vdom.Attr.class_ "bonsai-web-ui-typeahead-pills")
      (Set.to_list selected_options |> List.map ~f:pill)
;;

let[@warning "-16"] input
                      ?(placeholder = "")
                      ~extra_attrs
                      ~to_string
                      ~split
                      ~id
                      ~all_options
                      ~selected_options
                      ~on_set_change
  =
  let open! Bonsai.Let_syntax in
  (* This state is held internally to force the typeahead to clear the text contents
     of the input field when an option is selected. *)
  let%sub select = Bonsai.state [%here] (module String) ~default_model:"" in
  return
  @@ let%map select, inject_select = select
  and all_options = all_options
  and selected_options, inject_selected_options = selected_options
  and extra_attrs = extra_attrs
  and id = id
  and on_set_change = on_set_change in
  let on_input maybe_t user_input =
    match maybe_t with
    | None ->
      let attempted_input_items = split user_input |> String.Set.of_list in
      let new_selected_options =
        List.filter all_options ~f:(fun option ->
          let option = to_string option in
          Set.mem attempted_input_items option)
      in
      let selected_options =
        new_selected_options |> List.fold ~init:selected_options ~f:Set.add
      in
      Ui_effect.Many
        [ inject_selected_options selected_options
        ; (match new_selected_options with
           | []     -> inject_select user_input
           | _ :: _ -> inject_select "")
        ]
    | Some t ->
      let selected_options = Set.add selected_options t in
      Ui_effect.Many
        [ on_set_change selected_options
        ; inject_selected_options selected_options
        ; inject_select ""
        ]
  in
  input ~extra_attrs ~value:select ~placeholder ~id ~all_options ~on_input ~to_string
;;

let create_multi
      (type comparator_witness t)
      ?(extra_attrs = Value.return [])
      ?placeholder
      ?(on_set_change = Value.return (const Ui_effect.Ignore))
      ?to_string
      ?(split = List.return)
      (module M : Bonsai.Comparator
        with type comparator_witness = comparator_witness
         and type t = t)
      ~all_options
  =
  let open Bonsai.Let_syntax in
  let module M = struct
    include M
    include Comparable.Make_using_comparator (M)
  end
  in
  let to_string =
    Option.value to_string ~default:(fun a -> a |> M.sexp_of_t |> Sexp.to_string_hum)
  in
  let selected_options = Bonsai.state [%here] (module M.Set) ~default_model:M.Set.empty in
  let%sub selected_options = selected_options in
  let%sub id = path in
  let%sub input =
    input
      ?placeholder
      ~extra_attrs
      ~to_string
      ~id
      ~all_options
      ~selected_options
      ~on_set_change
      ~split
  in
  return
  @@ let%map selected_options, inject_selected_options = selected_options
  and input         = input
  and on_set_change = on_set_change
  and id            = id
  and all_options   = all_options in
  let datalist =
    datalist
      ~id
      ~all_options
      ~to_string
      ~filter_options_by:
        (let all_options = Set.of_list (module M) all_options in
         let remaining_options = Set.diff all_options selected_options in
         fun option -> Set.mem remaining_options option)
      ()
  in
  let pills =
    pills ~selected_options ~on_set_change ~inject_selected_options ~to_string
  in
  selected_options, Vdom.Node.div [ input; datalist; pills ], inject_selected_options
;;
