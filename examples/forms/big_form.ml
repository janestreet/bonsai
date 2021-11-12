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

  let form =
    Form.Typed.Variant.make
      (module struct
        module Typed_variant = Typed_variant

        let form_for_variant : type a. a Typed_variant.t -> a Form.t Computation.t
          = function
            | A -> Bonsai.const (Form.return ())
            | B -> E.Textbox.string [%here]
            | C ->
              Computation.map2
                ~f:Form.both
                (E.Textbox.int [%here])
                (E.Textbox.float [%here])
        ;;
      end)
  ;;
end

module Nested_record = struct
  let checkbox = Form.Elements.Checkbox.bool ~default:false

  module Nested = struct
    type t =
      { b_1 : bool
      ; b_2 : bool
      }
    [@@deriving typed_fields, sexp]

    let form () =
      Form.Typed.Record.make
        (module struct
          module Typed_field = Typed_field

          let form_for_field : type a. a Typed_field.t -> a Form.t Computation.t
            = function
              | B_1 -> checkbox [%here]
              | B_2 -> checkbox [%here]
          ;;
        end)
    ;;
  end

  type t =
    { a_1 : bool
    ; a_2 : Nested.t
    }
  [@@deriving typed_fields, sexp]

  let form () =
    Form.Typed.Record.make
      (module struct
        module Typed_field = Typed_field

        let form_for_field : type a. a Typed_field.t -> a Form.t Computation.t = function
          | A_1 -> checkbox [%here]
          | A_2 -> Nested.form ()
        ;;
      end)
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
  ; nested_record : Nested_record.t
  }
[@@deriving typed_fields, fields, sexp_of]

let ( >>|| ) a f = Bonsai.Computation.map a ~f

let form =
  let%sub multi_select =
    E.Multiselect.list [%here] (module A_B_or_C) (Value.return A_B_or_C.all)
  in
  Form.Typed.Record.make
    (module struct
      module Typed_field = Typed_field

      let form_for_field : type a. a Typed_field.t -> a Form.t Computation.t = function
        | Variant ->
          My_variant.form >>|| Form.tooltip "Tooltips can also be on header groups"
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
          Form.Dynamic.collapsible_group (Value.return "collapsible group") multi_select
        | Many2 ->
          E.Multiselect.list
            [%here]
            (module A_B_or_C)
            (multi_select >>| Form.value_or_default ~default:[])
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
        | Nested_record -> Nested_record.form ()
      ;;
    end)
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
