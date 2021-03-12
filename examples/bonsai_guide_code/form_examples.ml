open! Core_kernel
open! Async_kernel
open! Bonsai_web
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form

(* $MDX part-begin=form_textbox_value *)
let textbox_value =
  let%sub textbox = Form.Elements.Textbox.string [%here] in
  return
    (let%map textbox = textbox >>| Form.label "my textbox" in
     let value = Form.value textbox in
     Vdom.Node.div
       []
       [ Form.view_as_vdom textbox
       ; Vdom.Node.sexp_for_debugging ([%sexp_of: string Or_error.t] value)
       ])
;;

(* $MDX part-end *)

let () = Util.run textbox_value ~id:"form_textbox_value"

(* $MDX part-begin=form_set *)
let form_set =
  let%sub textbox = Form.Elements.Textbox.string [%here] in
  return
    (let%map textbox = textbox >>| Form.label "my textbox" in
     Vdom.Node.div
       []
       [ Form.view_as_vdom textbox
       ; Vdom.Node.button
           [ Vdom.Attr.on_click (fun _ -> Form.set textbox "hello world") ]
           [ Vdom.Node.text "click me" ]
       ])
;;

(* $MDX part-end *)

let () = Util.run form_set ~id:"form_set"

(* $MDX part-begin=form_two_textboxes *)
let two_textboxes =
  let%sub textbox_a = Form.Elements.Textbox.string [%here] in
  let%sub textbox_b = Form.Elements.Textbox.string [%here] in
  let both_textboxes =
    Bonsai.Value.map2
      ~f:Form.both
      (textbox_a >>| Form.label "textbox a")
      (textbox_b >>| Form.label "textbox b")
  in
  return
    (let%map both_textboxes = both_textboxes in
     let text_a, text_b = Form.value_or_default both_textboxes ~default:("", "") in
     let display = Vdom.Node.textf "a: %s, b: %s" text_a text_b in
     Vdom.Node.div
       [ Vdom.Attr.style (Css_gen.display `Inline_grid) ]
       [ Form.view_as_vdom both_textboxes; display ])
;;

(* $MDX part-end *)

let () = Util.run two_textboxes ~id:"form_two_textboxes"

(* $MDX part-begin=record_form_type *)

type t =
  { some_string : string
  ; an_int : int
  ; on_or_off : bool
  }
[@@deriving fields, sexp_of]

(* $MDX part-end *)

(* $MDX part-begin=record_form *)
let form_of_t : t Form.t Bonsai.Computation.t =
  let%sub some_string = Form.Elements.Textbox.string [%here] in
  let%sub an_int = Form.Elements.Number.int [%here] ~default:0 ~step:1 () in
  let%sub on_or_off = Form.Elements.Checkbox.bool [%here] ~default:false in
  (* this [open] is for the [field] and [build_for_record] functions *)
  let open Form.Dynamic.Record_builder in
  Fields.make_creator
    ~some_string:(field some_string)
    ~an_int:(field an_int)
    ~on_or_off:(field on_or_off)
  |> build_for_record
;;

(* $MDX part-end *)

let () =
  Util.run
    (let%map.Bonsai.Computation record_form = form_of_t in
     Form.view_as_vdom record_form)
    ~id:"record_form"
;;

(* $MDX part-begin=record_form_view *)
let view_for_form : Vdom.Node.t Bonsai.Computation.t =
  let%sub form = form_of_t in
  return
    (let%map form = form in
     let value = Form.value form in
     Vdom.Node.div
       []
       [ Form.view_as_vdom form
       ; Vdom.Node.sexp_for_debugging ([%sexp_of: t Or_error.t] value)
       ])
;;

(* $MDX part-end *)

let () = Util.run view_for_form ~id:"record_form_view"

(* $MDX part-begin=int_textbox *)
let int_textbox : int Form.t Bonsai.Computation.t =
  let%sub form = Form.Elements.Textbox.string [%here] in
  return
    (let%map form = form in
     Form.project form ~parse_exn:Int.of_string ~unparse:Int.to_string)
;;

(* $MDX part-end *)

let () =
  Util.run
    (let%map.Bonsai.Computation form = int_textbox in
     Vdom.Node.div
       []
       [ Form.view_as_vdom form
       ; Vdom.Node.sexp_for_debugging ([%sexp_of: int Or_error.t] (Form.value form))
       ])
    ~id:"int_textbox"
;;
