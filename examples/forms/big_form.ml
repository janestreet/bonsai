open! Core
open! Bonsai_web
open! Import
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form
module E = Form.Elements

module A_B_or_C = struct
  module T = struct
    type t =
      | Aaaa
      | Abbb
      | Cccc
    [@@deriving enumerate, compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

module My_variant = struct
  type t =
    | A
    | B of string
    | C of int * float
  [@@deriving sexp, equal]

  module Kind = struct
    type t =
      [ `A
      | `B
      | `C
      ]
    [@@deriving sexp, enumerate, compare, equal]
  end

  let form =
    let%sub chooser = E.Dropdown.enumerable [%here] (module Kind) in
    let%sub b_textbox = E.Textbox.string [%here] in
    let%sub c_int = E.Textbox.int [%here] in
    let%sub c_float = E.Textbox.float [%here] in
    return
    @@ let%map chooser = chooser
    and b_textbox = b_textbox
    and c_int = c_int
    and c_float = c_float in
    let b_textbox = Form.label "B string" b_textbox in
    let c_int = Form.label "C int" c_int in
    let c_float = Form.label "C float" c_float in
    let which = Form.value_or_default chooser ~default:`A in
    let value =
      match which with
      | `A -> Ok A
      | `B -> b_textbox |> Form.value |> Or_error.map ~f:(fun s -> B s)
      | `C ->
        let%map.Or_error int_value = Form.value c_int
        and float_value = Form.value c_float in
        C (int_value, float_value)
    in
    let extra_view =
      match which with
      | `A -> []
      | `B -> [ Form.view b_textbox ]
      | `C -> [ Form.view c_int; Form.view c_float ]
    in
    let view =
      Form.View.Private.Header_group
        { label = None
        ; header_view = Form.view chooser
        ; view = Form.View.Private.List extra_view
        }
    in
    let set = function
      | A -> Form.set chooser `A
      | B s -> Ui_effect.Many [ Form.set chooser `B; Form.set b_textbox s ]
      | C (i, f) ->
        Ui_effect.Many [ Form.set chooser `C; Form.set c_int i; Form.set c_float f ]
    in
    Form.Expert.create ~value ~view ~set
  ;;
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
  ; bool_from_dropdown : bool
  ; typeahead : A_B_or_C.t
  ; string_option : string option
  ; a_b_or_c : A_B_or_C.t
  ; many : A_B_or_C.t list
  ; many2 : A_B_or_C.t list
  ; string_set : String.Set.t
  ; files : Bonsai_web_ui_file.t Filename.Map.t
  ; rank : string list
  }
[@@deriving fields, sexp_of]

let form =
  let%sub range = E.Range.int [%here] ~min:0 ~max:100 ~default:0 ~step:1 () in
  let%sub string = E.Textbox.string [%here] in
  let%sub date = E.Date_time.date [%here] in
  let%sub time = E.Date_time.time [%here] in
  let%sub date_time = E.Date_time.datetime_local [%here] in
  let%sub string_vert_radio =
    E.Radio_buttons.list
      [%here]
      ~to_string:Fn.id
      (module String)
      ~layout:`Vertical
      (Value.return [ "first"; "second"; "third" ])
  in
  let%sub string_horiz_radio =
    E.Radio_buttons.list
      [%here]
      ~to_string:Fn.id
      (module String)
      ~layout:`Horizontal
      (Value.return [ "first"; "second"; "third" ])
  in
  let%sub bool_from_checkbox = E.Checkbox.bool [%here] ~default:false in
  let%sub date_from_string = E.Textbox.string [%here] in
  let date_from_string =
    date_from_string >>| Form.project ~parse_exn:Date.of_string ~unparse:Date.to_string
  in
  let%sub bool_from_dropdown =
    E.Dropdown.enumerable [%here] (module Bool) ~to_string:Bool.to_string
  in
  let%sub multi_select =
    E.Multiselect.list [%here] (module A_B_or_C) (Value.return A_B_or_C.all)
  in
  let%sub multi_select =
    Form.Dynamic.collapsible_group (Value.return "collapsible group") multi_select
  in
  let%sub multi_select2 =
    E.Multiselect.list
      [%here]
      (module A_B_or_C)
      (multi_select >>| Form.value_or_default ~default:[])
  in
  let%sub string_drop_option =
    E.Dropdown.list_opt [%here] (module String) (Value.return [ "hello"; "world" ])
  in
  let%sub typeahead =
    E.Typeahead.single
      [%here]
      ~placeholder:"Typeahead here!"
      (module A_B_or_C)
      ~all_options:(Value.return A_B_or_C.all)
  in
  let%sub a_b_or_c = E.Dropdown.enumerable [%here] (module A_B_or_C) in
  let%sub sexp_from_string = E.Textbox.sexpable [%here] (module Sexp) in
  let%sub sexp_from_string = Form.Dynamic.error_hint sexp_from_string in
  let%sub variant = My_variant.form in
  let%sub string_set =
    E.Checkbox.set
      [%here]
      ~to_string:Fn.id
      (module String)
      (Value.return [ "first"; "second"; "third"; "fourth" ])
  in
  let%sub files =
    E.File_select.multiple
      [%here]
      ~accept:[ `Mimetype "application/pdf"; `Extension ".csv" ]
      ()
  in
  let%sub rank =
    E.Rank.list
      (module String)
      (fun ~source item ->
         return
           (let%map item = item
            and source = source in
            Vdom.Node.div ~attr:source [ Vdom.Node.text item ]))
  in
  let%sub rank =
    Form.Dynamic.with_default (Value.return [ "aaaaaa"; "bbbbbb"; "cccccc" ]) rank
  in
  let open Form.Dynamic.Record_builder in
  Fields.make_creator
    ~variant:(field variant)
    ~int_from_range:(field range)
    ~string_from_text:(field string)
    ~string_from_vert_radio:(field string_vert_radio)
    ~string_from_horiz_radio:(field string_horiz_radio)
    ~date:(field date)
    ~time_ns_of_day:(field time)
    ~date_time:(field date_time)
    ~date_from_string:(field date_from_string)
    ~bool_from_checkbox:(field bool_from_checkbox)
    ~bool_from_dropdown:(field bool_from_dropdown)
    ~many:(field multi_select)
    ~many2:(field multi_select2)
    ~string_option:(field string_drop_option)
    ~typeahead:(field typeahead)
    ~a_b_or_c:(field a_b_or_c)
    ~sexp_from_string:(field sexp_from_string)
    ~string_set:(field string_set)
    ~files:(field files)
    ~rank:(field rank)
  |> build_for_record
;;

let component =
  let%sub form = form in
  return
  @@ let%map form = form in
  let output = view_t ~sexp_of:[%sexp_of: t Or_error.t] (Form.value form) in
  Vdom.Node.div
    [ Vdom.Node.h1 [ Vdom.Node.text "Big Form" ]
    ; Form.View.to_vdom (Form.view form)
    ; output
    ]
;;
