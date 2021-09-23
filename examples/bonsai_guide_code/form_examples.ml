open! Core
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
       [ Form.view_as_vdom textbox
       ; Vdom.Node.sexp_for_debugging ([%sexp_of: string Or_error.t] value)
       ])
;;

(* $MDX part-end *)
let () = Util.run textbox_value ~id:"form_textbox_value"

let alert =
  let eff =
    Effect.of_sync_fun (fun s ->
      Js_of_ocaml.Dom_html.window##alert (Js_of_ocaml.Js.string s))
  in
  fun s -> eff s
;;

(* $MDX part-begin=view_with_submission *)

let textbox_on_submit =
  let%sub textbox = Form.Elements.Textbox.string [%here] in
  return
    (let%map textbox = textbox in
     textbox
     |> Form.label "text to alert"
     |> Form.view_as_vdom ~on_submit:(Form.Submit.create () ~f:alert))
;;

(* $MDX part-end *)

let () = Util.run textbox_on_submit ~id:"form_textbox_on_submit"

(* $MDX part-begin=form_set *)
let form_set =
  let%sub textbox = Form.Elements.Textbox.string [%here] in
  return
    (let%map textbox = textbox >>| Form.label "my textbox" in
     Vdom.Node.div
       [ Form.view_as_vdom textbox
       ; Vdom.Node.button
           ~attr:(Vdom.Attr.on_click (fun _ -> Form.set textbox "hello world"))
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
    Value.map2
      ~f:Form.both
      (textbox_a >>| Form.label "textbox a")
      (textbox_b >>| Form.label "textbox b")
  in
  return
    (let%map both_textboxes = both_textboxes in
     let text_a, text_b = Form.value_or_default both_textboxes ~default:("", "") in
     let display = Vdom.Node.textf "a: %s, b: %s" text_a text_b in
     Vdom.Node.div
       ~attr:(Vdom.Attr.style (Css_gen.display `Inline_grid))
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
[@@deriving typed_fields, sexp_of]

(* $MDX part-end *)

(* $MDX part-begin=record_form *)
let form_of_t : t Form.t Computation.t =
  Form.Typed.Record.make
    (module struct
      (* reimport the module that typed_fields just derived *)
      module Typed_field = Typed_field

      (* provide a form computation for each field in the record *)
      let form_for_field : type a. a Typed_field.t -> a Form.t Computation.t = function
        | Some_string -> Form.Elements.Textbox.string [%here]
        | An_int -> Form.Elements.Number.int [%here] ~default:0 ~step:1 ()
        | On_or_off -> Form.Elements.Checkbox.bool [%here] ~default:false
      ;;
    end)
;;

(* $MDX part-end *)

(* $MDX part-begin=variant_form *)

type v =
  | A
  | B of int
  | C of string
[@@deriving typed_variants, sexp_of]

let form_of_v : v Form.t Computation.t =
  Form.Typed.Variant.make
    (module struct
      (* reimport the module that typed_fields just derived *)
      module Typed_variant = Typed_variant_of_v

      (* provide a form computation for constructor in the variant *)
      let form_for_variant : type a. a Typed_variant.t -> a Form.t Computation.t
        = function
          | A -> Bonsai.const (Form.return ())
          | B -> Form.Elements.Textbox.int [%here]
          | C -> Form.Elements.Textbox.string [%here]
      ;;
    end)
;;

(* $MDX part-end *)

let () =
  Util.run
    (let%map.Computation record_form = form_of_t in
     Form.view_as_vdom record_form)
    ~id:"record_form"
;;

(* $MDX part-begin=record_form_view *)
let view_for_form : Vdom.Node.t Computation.t =
  let%sub form_t = form_of_t in
  let%sub form_v = form_of_v in
  return
    (let%map form_t = form_t
     and form_v = form_v in
     let form = Form.both form_t form_v in
     let value = Form.value form in
     Vdom.Node.div
       [ Form.view_as_vdom form
       ; Vdom.Node.sexp_for_debugging ([%sexp_of: (t * v) Or_error.t] value)
       ])
;;

(* $MDX part-end *)

let () = Util.run view_for_form ~id:"record_form_view"

(* $MDX part-begin=int_textbox *)
let int_textbox : int Form.t Computation.t =
  let%sub form = Form.Elements.Textbox.string [%here] in
  return
    (let%map form = form in
     Form.project form ~parse_exn:Int.of_string ~unparse:Int.to_string)
;;

(* $MDX part-end *)

let () =
  Util.run
    (let%map.Computation form = int_textbox in
     Vdom.Node.div
       [ Form.view_as_vdom form
       ; Vdom.Node.sexp_for_debugging ([%sexp_of: int Or_error.t] (Form.value form))
       ])
    ~id:"int_textbox"
;;
