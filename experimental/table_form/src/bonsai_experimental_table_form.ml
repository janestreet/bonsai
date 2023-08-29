open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form
module Table = Bonsai_web_ui_partial_render_table.Basic
module C = Table.Columns.Dynamic_columns

let header text = C.Header_helpers.legacy (Vdom.Node.text text)

module type S = sig
  type t [@@deriving sexp, equal, compare]

  include Comparator.S with type t := t
end

type ('k, 'cmp) comparator =
  (module S with type t = 'k and type comparator_witness = 'cmp)

module Style =
  [%css
    stylesheet
      {|
.key_column {
  white-space: pre;
}

.remove_button {
  border: none;
  cursor: pointer;
  color: blue;
  background: none;
}

.key_input_container {
  display: flex;
}

.key_input_textbox {
  flex: 1;
}
  |}]

module Column = struct
  type t =
    { name : string
    ; initial_width : Css_gen.Length.t option
    }

  let create ?initial_width name = { name; initial_width }
end

let table_form
      (type k cmp)
      ?key_column_initial_width
      (key : (k, cmp) comparator)
      form_of_t
      ~columns
  =
  let module Key = (val key) in
  let module M_map = struct
    type t = Unit.t Map.M(Key).t [@@deriving sexp_of, equal, compare]
  end
  in
  let%sub id =
    Computation.map Bonsai.Private.path ~f:Bonsai.Private.Path.to_unique_identifier_string
  in
  let%sub map, set_map =
    Bonsai.state
      (Map.empty (module Key))
      ~sexp_of_model:[%sexp_of: M_map.t]
      ~equal:[%equal: M_map.t]
  in
  let%sub textbox =
    let%sub text_state =
      Bonsai.state "" ~sexp_of_model:[%sexp_of: String.t] ~equal:[%equal: String.t]
    in
    let%arr text, set_text = text_state
    and map = map
    and set_map = set_map in
    let on_submit () =
      Effect.Many
        [ set_map (Map.set map ~key:(Key.t_of_sexp (Sexp.of_string text)) ~data:())
        ; set_text ""
        ; Vdom.Effect.Prevent_default
        ]
    in
    let textbox =
      Vdom.Node.input
        ~attrs:
          [ Style.key_input_textbox
          ; Vdom.Attr.value_prop text
          ; Vdom.Attr.placeholder "key for new row"
          ; Vdom.Attr.on_input (fun _ s -> set_text s)
          ; Vdom.Attr.on_keydown (fun evt ->
              match Js_of_ocaml.Dom_html.Keyboard_code.of_event evt with
              | Enter -> on_submit ()
              | _ -> Effect.Ignore)
          ]
        ()
    in
    let button =
      Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> on_submit ()) ]
        [ Vdom.Node.text "+" ]
    in
    Vdom.Node.div ~attrs:[ Style.key_input_container ] [ textbox; button ]
  in
  let columns =
    let%map columns = columns
    and set_map = set_map
    and map = map
    and textbox = textbox in
    let editable_columns =
      List.mapi columns ~f:(fun i { Column.name; initial_width } ->
        C.column
          ?initial_width
          ~header:(header name)
          ~cell:(fun ~key:_ ~data:form ->
            form
            |> Form.view
            |> Form.View.to_vdom_plain
            |> Fn.flip List.nth i
            |> Option.value ~default:(Vdom.Node.text "-"))
          ())
    in
    List.concat
      [ [ C.column
            ?initial_width:key_column_initial_width
            ~header:(header "key")
            ~cell:(fun ~key ~data:_ ->
              let click_handler =
                Vdom.Attr.on_click (fun _ -> set_map (Map.remove map key))
              in
              Vdom.Node.div
                ~attrs:[ Style.key_column ]
                [ Vdom.Node.button
                    ~attrs:[ click_handler; Style.remove_button ]
                    [ Vdom.Node.text "⊗" ]
                ; Vdom.Node.text (Key.sexp_of_t key |> Sexp.to_string)
                ])
            ()
        ]
      ; editable_columns
      ]
    |> C.group ~label:textbox
    |> List.return
  in
  let%sub forms, table =
    let%sub forms = Bonsai.assoc (module Key) map ~f:(fun _key _data -> form_of_t) in
    let%sub { view; _ } =
      let columns = C.lift columns in
      Table.component
        (module Key)
        ~focus:None
        ~row_height:(Value.return (`Px 25))
        ~columns
        forms
    in
    let%arr forms = forms
    and view = view in
    forms, view
  in
  let%sub get_forms = Bonsai.yoink forms in
  let%arr forms = forms
  and get_forms = get_forms
  and table = table
  and set_map = set_map
  and id = id in
  let view = Form.View.of_vdom ~unique_key:id table in
  let value =
    forms
    |> Map.to_alist
    |> List.map ~f:(fun (k, form) ->
      form |> Form.value |> Or_error.map ~f:(fun v -> k, v))
    |> Or_error.combine_errors
    |> Or_error.map ~f:(Map.of_alist_exn (module Key))
  in
  let set new_data =
    let%bind.Effect () = set_map (Map.map new_data ~f:(Fn.const ())) in
    let%bind.Effect forms =
      match%bind.Effect get_forms with
      | Active forms -> Effect.return forms
      | Inactive ->
        Effect.never
    in
    Effect.Many
      (List.map2_exn (Map.data forms) (Map.data new_data) ~f:(fun form data ->
         Form.set form data))
  in
  Form.Expert.create ~view ~value ~set
;;
