open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Username = Username_kernel.Username
module Form = Bonsai_web_ui_form
module Table_form = Bonsai_experimental_table_form

(* All of the fields in this record are going to be editable in the form. *)
type t =
  { name : string
  ; age : int
  ; likes_cats : bool
  }
[@@deriving typed_fields]

let form_of_t =
  Form.Typed.Record.make
    (module struct
      module Typed_field = Typed_field

      let form_for_field (type a) (field : a Typed_field.t) : a Form.t Computation.t =
        match field with
        | Name -> Form.Elements.Textbox.string [%here]
        | Age -> Form.Elements.Textbox.int [%here]
        | Likes_cats -> Form.Elements.Checkbox.bool ~default:true [%here]
      ;;
    end)
;;

let starting_data =
  [ Username.of_string "amin", { name = "Alice"; age = 30; likes_cats = true }
  ; Username.of_string "bmax", { name = "Bob"; age = 26; likes_cats = true }
  ; Username.of_string "mclamp", { name = "Mal"; age = 27; likes_cats = false }
  ]
  |> Map.of_alist_exn (module Username)
;;

let table_form =
  Table_form.table_form
    (module Username)
    ~key_column_initial_width:(`Px 100)
    form_of_t
    ~columns:
      (Value.return
         [ Table_form.Column.create "name"
         ; Table_form.Column.create "age"
         ; Table_form.Column.create "likes cats"
         ])
;;

let (_ : _ Start.Handle.t) =
  let app =
    let%sub table_form = table_form in
    let%sub table_form =
      (* We call with_default, which sets the form to contain the starting data that we
         care about. *)
      Form.Dynamic.with_default (Value.return starting_data) table_form
    in
    (* The application doesn't really do anything with this form other than view it, so we
       just project out the view and return that for the application component. *)
    Bonsai.pure Form.view_as_vdom table_form
  in
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" app
;;
