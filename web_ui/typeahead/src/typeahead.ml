open! Core
open! Bonsai_web
open! Bonsai_web_ui_common_components

(** This provides an implementation of [inputs] with associated [datalist] element.

    See the full spec here:

    [datalist]: https://developer.mozilla.org/en-US/docs/Web/HTML/Element/datalist
    [list attr]: https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input#htmlattrdeflist

    As to why this alternate typeahead implementation exists:

    Existing typeahead controls implement a custom select using divs. While this isn't
    strictly a negative (native select dropdowns cannot be styled), using the native
    controls where possible is nice. *)

module Search = struct
  type 'a t =
    | Nothing_found
    | Partial_match of 'a
    | Only_exact_matches_allowed

  let find ~to_string ~haystack ~needle ~handle_unknown_option =
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
          | Nothing_found -> Continue (Partial_match value)
          (* Two partial matches means that we continue, but will only succeed if
             finding an exact-match *)
          | Partial_match _ -> Continue Only_exact_matches_allowed
          (* If we are in the only-exact-matches state, don't change it *)
          | Only_exact_matches_allowed -> Continue Only_exact_matches_allowed)
        else Continue state)
      ~finish:(fun state ->
        let haystack_result =
          match state with
          | Only_exact_matches_allowed -> None
          | Nothing_found -> None
          | Partial_match value -> Some value
        in
        let unknown_option_result = handle_unknown_option needle in
        (* Use either one of the potential results. If both methods yielded potential
           results, use the unknown option result. *)
        Option.first_some unknown_option_result haystack_result)
  ;;
end

type 'a t =
  { selected : 'a
  ; set_selected : 'a -> unit Ui_effect.t
  ; current_input : string
  ; view : Vdom.Node.t
  }

let input
  ?(placeholder = "")
  ?(value = "")
  ~set_focused
  ~extra_attrs
  ~to_string
  ~id
  ~handle_unknown_option
  ~all_options
  ~on_change
  ~on_input
  ()
  =
  Vdom.Node.lazy_
    (lazy
      (Vdom.Node.input
         ~attrs:
           [ Vdom.Attr.many_without_merge
               (extra_attrs
                @ [ Vdom.Attr.type_ "text"
                  ; Vdom.Attr.create "list" id
                  ; Vdom.Attr.placeholder placeholder
                    (* Both Attr.value and Attr.string_property value must be set. The former only affects
                     initial control state while the latter affects the control state whilst the form is
                     being used. *)
                  ; Vdom.Attr.value value
                  ; Vdom.Attr.on_focus (fun _ -> set_focused true)
                  ; Vdom.Attr.on_blur (fun _ -> set_focused false)
                  ; Vdom.Attr.string_property "value" value
                  ; Vdom.Attr.on_input (fun _ -> on_input)
                  ; Vdom.Attr.on_change (fun _ input ->
                      let maybe_t =
                        Search.find
                          ~to_string
                          ~needle:input
                          ~haystack:all_options
                          ~handle_unknown_option
                      in
                      on_change maybe_t input)
                  ])
           ]
         ()))
;;

let datalist ?filter_options_by ~id ~all_options ~to_string ~to_option_description () =
  Vdom.Node.lazy_
    (lazy
      (let option_of_t t =
         Vdom.Node.option
           ~attrs:[ Vdom.Attr.value (to_string t) ]
           [ Vdom.Node.text (to_option_description t) ]
       in
       let all_options =
         match filter_options_by with
         | None -> List.map ~f:option_of_t all_options
         | Some filter_options_by ->
           List.filter_map all_options ~f:(fun item ->
             if filter_options_by item then Some (option_of_t item) else None)
       in
       Vdom.Node.datalist ~attrs:[ Vdom.Attr.id id ] all_options))
;;

let show_datalist ~focused ~show_datalist_in_test =
  if focused
  then true
  else (
    match Bonsai_web.am_running_how with
    | `Browser | `Browser_benchmark | `Node | `Node_benchmark -> false
    | `Node_test -> show_datalist_in_test)
;;

let create_internal
  (type t)
  ?(extra_attrs = Value.return [])
  ?placeholder
  ?on_select_change
  ?to_string
  ?to_option_description
  ?(handle_unknown_option = Value.return (Fn.const None))
  (module M : Bonsai.Model with type t = t)
  ~equal
  ~all_options
  ~show_datalist_in_test
  =
  let open! Bonsai.Let_syntax in
  let to_string =
    Option.value
      to_string
      ~default:(Value.return (fun a -> a |> M.sexp_of_t |> Sexp.to_string_hum))
  in
  let to_option_description = Option.value to_option_description ~default:to_string in
  let on_select_change =
    Option.value
      on_select_change
      ~default:(Value.return (fun (_ : M.t option) -> Ui_effect.Ignore))
  in
  let%sub focused, set_focused =
    Bonsai.state false ~sexp_of_model:[%sexp_of: Bool.t] ~equal:[%equal: Bool.t]
  in
  let%sub current_input, set_current_input =
    Bonsai.state "" ~sexp_of_model:[%sexp_of: String.t] ~equal:[%equal: String.t]
  in
  let%sub selected, set_selected =
    Bonsai.state_opt () ~sexp_of_model:[%sexp_of: M.t] ~equal
  in
  let%sub id = Bonsai.path_id in
  let%sub input =
    let%arr set_focused = set_focused
    and set_selected = set_selected
    and extra_attrs = extra_attrs
    and id = id
    and handle_unknown_option = handle_unknown_option
    and all_options = all_options
    and on_select_change = on_select_change
    and current_input = current_input
    and set_current_input = set_current_input
    and to_string = to_string in
    let on_input input = set_current_input input in
    let on_change t _ = Ui_effect.Many [ set_selected t; on_select_change t ] in
    input
      ?placeholder
      ~set_focused
      ~extra_attrs
      ~id
      ~handle_unknown_option
      ~all_options
      ~on_change
      ~on_input
      ~to_string
      ~value:current_input
      ()
  in
  let%sub datalist =
    let%sub show_datalist =
      let%arr focused = focused in
      show_datalist ~focused ~show_datalist_in_test
    in
    match%sub show_datalist with
    | false -> Bonsai.const (Vdom.Node.text "")
    | true ->
      let%arr to_option_description = to_option_description
      and id = id
      and to_string = to_string
      and all_options = all_options in
      datalist ~to_option_description ~to_string ~id ~all_options ()
  in
  let%sub view =
    let%arr input = input
    and datalist = datalist in
    Vdom.Node.div [ input; datalist ]
  in
  let%sub set_selected =
    let%arr set_selected = set_selected
    and set_current_input = set_current_input
    and to_string = to_string in
    fun selected ->
      Effect.lazy_
        (lazy
          (let current_input = Option.value_map selected ~f:to_string ~default:"" in
           Ui_effect.Many [ set_selected selected; set_current_input current_input ]))
  in
  let%arr selected = selected
  and current_input = current_input
  and view = view
  and set_selected = set_selected in
  { selected; current_input; view; set_selected }
;;

let input
  ?(placeholder = "")
  ~current_input
  ~inject_current_input
  ~extra_attrs
  ~to_string
  ~split
  ~id
  ~handle_unknown_option
  ~all_options
  ~selected_options
  ~inject_selected_options
  ~on_set_change
  ~set_focused
  ()
  =
  let open! Bonsai.Let_syntax in
  let%arr current_input = current_input
  and inject_current_input = inject_current_input
  and handle_unknown_option = handle_unknown_option
  and all_options = all_options
  and selected_options = selected_options
  and inject_selected_options = inject_selected_options
  and extra_attrs = extra_attrs
  and id = id
  and on_set_change = on_set_change
  and to_string = to_string
  and set_focused = set_focused in
  let on_input input = inject_current_input input in
  let on_change maybe_t user_input =
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
           | [] -> inject_current_input user_input
           | _ :: _ -> inject_current_input "")
        ]
    | Some t ->
      let selected_options = Set.add selected_options t in
      Ui_effect.Many
        [ on_set_change selected_options
        ; inject_selected_options selected_options
        ; inject_current_input ""
        ]
  in
  input
    ~extra_attrs
    ~value:current_input
    ~placeholder
    ~id
    ~handle_unknown_option
    ~all_options
    ~on_input
    ~on_change
    ~to_string
    ~set_focused
    ()
;;

let create_multi_internal
  (type comparator_witness t)
  ?(extra_attrs = Value.return [])
  ?placeholder
  ?(on_set_change = Value.return (const Ui_effect.Ignore))
  ?to_string
  ?to_option_description
  ?(handle_unknown_option = Value.return (Fn.const None))
  ?(split = List.return)
  (module M : Bonsai.Comparator
    with type comparator_witness = comparator_witness
     and type t = t)
  ~all_options
  ~show_datalist_in_test
  =
  let open Bonsai.Let_syntax in
  let module M = struct
    include M
    include Comparable.Make_plain_using_comparator (M)
  end
  in
  let to_string =
    Option.value
      to_string
      ~default:(Value.return (fun a -> a |> M.sexp_of_t |> Sexp.to_string_hum))
  in
  let to_option_description = Option.value to_option_description ~default:to_string in
  let selected_options =
    Bonsai.state M.Set.empty ~sexp_of_model:[%sexp_of: M.Set.t] ~equal:[%equal: M.Set.t]
  in
  let focused =
    Bonsai.state false ~sexp_of_model:[%sexp_of: Bool.t] ~equal:[%equal: Bool.t]
  in
  let%sub selected_options, inject_selected_options = selected_options in
  let%sub focused, set_focused = focused in
  let%sub inject_selected_options =
    let%arr inject_selected_options = inject_selected_options
    and on_set_change = on_set_change in
    fun selected_options ->
      Effect.Many
        [ on_set_change selected_options; inject_selected_options selected_options ]
  in
  (* This state is held internally to force the typeahead to clear the text contents
     of the input field when an option is selected, and we give users access to the value
     as well *)
  let%sub current_input, inject_current_input =
    Bonsai.state "" ~sexp_of_model:[%sexp_of: String.t] ~equal:[%equal: String.t]
  in
  let%sub id = Bonsai.path_id in
  let%sub input =
    input
      ?placeholder
      ~extra_attrs
      ~current_input
      ~inject_current_input
      ~to_string
      ~id
      ~handle_unknown_option
      ~all_options
      ~selected_options
      ~inject_selected_options
      ~on_set_change
      ~split
      ~set_focused
      ()
  in
  let%sub pills =
    Pills.of_set
      ~extra_container_attr:
        (Value.return Vdom.Attr.(class_ "bonsai-web-ui-typeahead-pills"))
      ~to_string
      ~inject_selected_options
      selected_options
  in
  let%sub datalist =
    let%sub show_datalist =
      let%arr focused = focused in
      show_datalist ~focused ~show_datalist_in_test
    in
    match%sub show_datalist with
    | false -> Bonsai.const (Vdom.Node.datalist [])
    | true ->
      let%arr all_options = all_options
      and selected_options = selected_options
      and id = id
      and to_string = to_string
      and to_option_description = to_option_description in
      datalist
        ~id
        ~all_options
        ~to_string
        ~to_option_description
        ~filter_options_by:
          (let all_options = Set.of_list (module M) all_options in
           let remaining_options = Set.diff all_options selected_options in
           fun option -> Set.mem remaining_options option)
        ()
  in
  let%arr selected_options = selected_options
  and datalist = datalist
  and inject_selected_options = inject_selected_options
  and current_input = current_input
  and input = input
  and pills = pills in
  { selected = selected_options
  ; set_selected = inject_selected_options
  ; current_input
  ; view = Vdom.Node.div [ input; datalist; pills ]
  }
;;

let create = create_internal ~show_datalist_in_test:true
let create_multi = create_multi_internal ~show_datalist_in_test:true

module Private = struct
  module For_testing = struct
    let create_with_browser_behavior_in_test =
      create_internal ~show_datalist_in_test:false
    ;;

    let create_multi_with_browser_behavior_in_test =
      create_multi_internal ~show_datalist_in_test:false
    ;;
  end
end
