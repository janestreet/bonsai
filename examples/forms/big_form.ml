open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form
module E = Form.Elements

module Query_box_css =
  [%css.raw
    {|
  .list {
    background: white;
    border: solid 1px black;
    width: 200px;
    padding: 5px;
  }

  .selected_item {
    background: yellow;
  }
  |}]

module A_B_or_C = struct
  module T = struct
    type t =
      | Aaaa
      | Abbb
      | Cccc
    [@@deriving enumerate, compare, equal, sexp]
  end

  include T
  include Comparator.Make (T)
end

module My_variant = struct
  type t =
    | A
    | B of string
    | C of int * float
  [@@deriving sexp, equal, typed_variants]

  let form_for_variant : type a. a Typed_variant.t -> a Form.t Computation.t = function
    | A -> Bonsai.const (Form.return ())
    | B -> E.Textbox.string [%here]
    | C -> Computation.map2 ~f:Form.both (E.Textbox.int [%here]) (E.Textbox.float [%here])
  ;;
end

let my_variant_form = Form.Typed.Variant.make (module My_variant)

module Nested_record = struct
  let checkbox = Form.Elements.Checkbox.bool ~default:false

  module Inner = struct
    type t =
      { b_1 : bool
      ; b_2 : bool
      }
    [@@deriving typed_fields, sexp]

    let form_for_field : type a. a Typed_field.t -> a Form.t Computation.t = function
      | B_1 -> checkbox [%here]
      | B_2 -> checkbox [%here]
    ;;
  end

  let inner_form = Form.Typed.Record.make (module Inner)

  module Outer = struct
    type t =
      { a_1 : bool
      ; a_2 : Inner.t
      }
    [@@deriving typed_fields, sexp]

    let form_for_field : type a. a Typed_field.t -> a Form.t Computation.t = function
      | A_1 -> checkbox [%here]
      | A_2 -> inner_form
    ;;
  end

  let form = Form.Typed.Record.make (module Outer)
end

type t =
  { variant : My_variant.t
  ; int_from_range : int
  ; string_from_text : string
  ; string_from_vert_radio : string
  ; string_from_horiz_radio : string
  ; date : Date.t
  ; time_ns_of_day : Time_ns.Ofday.t
  ; date_time : Time_ns.Stable.Alternate_sexp.V1.t
  ; date_from_string : Date.t
  ; sexp_from_string : Sexp.t
  ; bool_from_checkbox : bool
  ; bool_from_toggle : bool
  ; bool_from_dropdown : bool
  ; typeahead : A_B_or_C.t
  ; color_picker : [ `Hex of string ]
  ; string_option : string option
  ; a_b_or_c : A_B_or_C.t
  ; many : A_B_or_C.t list
  ; many2 : A_B_or_C.t list
  ; string_set : String.Set.t
  ; files : Bonsai_web_ui_file.t Filename.Map.t
  ; rank : string list
  ; query_box : string
  ; nested_record : Nested_record.Outer.t
  }
[@@deriving typed_fields, fields, sexp_of]

let ( >>|| ) a f = Bonsai.Computation.map a ~f

let form_for_field : type a. a Typed_field.t -> a Form.t Computation.t = function
  | Variant -> my_variant_form >>|| Form.tooltip "Tooltips can also be on header groups"
  | Int_from_range -> E.Range.int [%here] ~min:0 ~max:100 ~default:0 ~step:1 ()
  | String_from_text -> E.Textbox.string [%here]
  | String_from_vert_radio ->
    E.Radio_buttons.list
      [%here]
      (module String)
      ~layout:`Vertical
      (Value.return [ "first"; "second"; "third" ])
  | String_from_horiz_radio ->
    E.Radio_buttons.list
      [%here]
      (module String)
      ~layout:`Horizontal
      (Value.return [ "first"; "second"; "third" ])
  | Date -> E.Date_time.date [%here]
  | Time_ns_of_day -> E.Date_time.time [%here]
  | Date_time -> E.Date_time.datetime_local [%here]
  | Date_from_string ->
    E.Textbox.string [%here]
    >>|| Form.project ~parse_exn:Date.of_string ~unparse:Date.to_string
  | Sexp_from_string -> E.Textbox.sexpable [%here] (module Sexp)
  | Bool_from_toggle -> E.Toggle.bool ~default:false ()
  | Bool_from_checkbox -> E.Checkbox.bool [%here] ~default:false
  | Bool_from_dropdown ->
    E.Dropdown.enumerable [%here] (module Bool) ~to_string:Bool.to_string
  | Typeahead ->
    E.Typeahead.single
      [%here]
      (module A_B_or_C)
      ~placeholder:"Typeahead here!"
      ~all_options:(Value.return A_B_or_C.all)
  | String_option ->
    E.Dropdown.list_opt [%here] (module String) (Value.return [ "hello"; "world" ])
  | A_b_or_c -> E.Dropdown.enumerable [%here] (module A_B_or_C)
  | Many ->
    let%sub multi_select =
      E.Multiselect.list [%here] (module A_B_or_C) (Value.return A_B_or_C.all)
    in
    Form.Dynamic.collapsible_group (Value.return "collapsible group") multi_select
  | Many2 ->
    let%sub multi_select =
      E.Multiselect.list [%here] (module A_B_or_C) (Value.return A_B_or_C.all)
    in
    let%sub multi_select2 =
      E.Multiselect.list
        [%here]
        (module A_B_or_C)
        (multi_select >>| Form.value_or_default ~default:[])
    in
    let%arr multi_select = multi_select
    and multi_select2 = multi_select2 in
    Form.both multi_select multi_select2
    |> Form.project ~parse_exn:snd ~unparse:(fun selected -> selected, selected)
  | String_set ->
    E.Checkbox.set
      [%here]
      (module String)
      (Value.return [ "first"; "second"; "third"; "fourth" ])
  | Files ->
    E.File_select.multiple
      [%here]
      ~accept:[ `Mimetype "application/pdf"; `Extension ".csv" ]
      ()
  | Rank ->
    let%sub rank =
      E.Rank.list
        (module String)
        (fun ~source item ->
           let%arr item = item
           and source = source in
           Vdom.Node.div ~attr:source [ Vdom.Node.text item ])
    in
    Form.Dynamic.with_default (Value.return [ "aaaaaa"; "bbbbbb"; "cccccc" ]) rank
  | Query_box ->
    Form.Elements.Query_box.stringable
      (module String)
      ~selected_item_attr:(Value.return (Vdom.Attr.class_ Query_box_css.selected_item))
      ~extra_list_container_attr:(Value.return (Vdom.Attr.class_ Query_box_css.list))
      (Value.return
         (String.Map.of_alist_exn [ "abc", "abc"; "def", "def"; "ghi", "ghi" ]))
  | Nested_record -> Nested_record.form
  | Color_picker -> E.Color_picker.hex [%here]
;;

let form =
  Form.Typed.Record.make
    (module struct
      module Typed_field = Typed_field

      let form_for_field = form_for_field
    end)
;;

let component =
  let%sub form = form in
  let%sub editable, toggle_editable = Bonsai_extra.toggle [%here] ~default_model:true in
  let%arr editable = editable
  and toggle_editable = toggle_editable
  and form = form in
  let output =
    Vdom.Node.sexp_for_debugging ([%sexp_of: t Or_error.t] (Form.value form))
  in
  let editable = if editable then `Currently_yes else `Currently_no in
  Vdom.Node.div
    [ Vdom.Node.h1 [ Vdom.Node.text "Big Form" ]
    ; Form.view_as_vdom ~editable form
    ; Vdom.Node.button
        ~attr:Vdom.Attr.(on_click (fun _ -> toggle_editable))
        [ Vdom.Node.text "Toggle Editing" ]
    ; output
    ]
;;
