open! Core
open! Async_kernel
open! Bonsai_web.Cont
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_automatic_view

(* $MDX part-begin=form_textbox_value *)
let textbox_value graph =
  let textbox =
    Form.Elements.Textbox.string ~allow_updates_when_focused:`Always () graph
  in
  let%arr textbox = textbox >>| Form.label "my textbox" in
  let value = Form.value textbox in
  Vdom.Node.div
    [ Form.view_as_vdom textbox
    ; Vdom.Node.sexp_for_debugging ([%sexp_of: string Or_error.t] value)
    ]
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

let textbox_on_submit graph =
  let textbox =
    Form.Elements.Textbox.string ~allow_updates_when_focused:`Always () graph
  in
  let%arr textbox = textbox in
  textbox
  |> Form.label "text to alert"
  |> Form.view_as_vdom ~on_submit:(Form.Submit.create () ~f:alert)
;;

(* $MDX part-end *)

let () = Util.run textbox_on_submit ~id:"form_textbox_on_submit"

(* $MDX part-begin=form_set *)
let form_set graph =
  let textbox =
    Form.Elements.Textbox.string ~allow_updates_when_focused:`Always () graph
  in
  let%arr textbox = textbox >>| Form.label "my textbox" in
  Vdom.Node.div
    [ Form.view_as_vdom textbox
    ; Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> Form.set textbox "hello world") ]
        [ Vdom.Node.text "click me" ]
    ]
;;

(* $MDX part-end *)

let () = Util.run form_set ~id:"form_set"

(* $MDX part-begin=form_two_textboxes *)
let two_textboxes graph =
  let textbox_a =
    Form.Elements.Textbox.string ~allow_updates_when_focused:`Always () graph
  in
  let textbox_b =
    Form.Elements.Textbox.string ~allow_updates_when_focused:`Always () graph
  in
  let both_textboxes =
    Bonsai.map2
      ~f:Form.both
      (textbox_a >>| Form.label "textbox a")
      (textbox_b >>| Form.label "textbox b")
  in
  let%arr both_textboxes = both_textboxes in
  let text_a, text_b = Form.value_or_default both_textboxes ~default:("", "") in
  let display = Vdom.Node.textf "a: %s, b: %s" text_a text_b in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.style (Css_gen.display `Inline_grid) ]
    [ Form.view_as_vdom both_textboxes; display ]
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
module Record = struct
  type t =
    { some_string : string
    ; an_int : int
    ; on_or_off : bool
    }
  [@@deriving typed_fields, sexp_of]

  let form : Bonsai.graph -> t Form.t Bonsai.t =
    Form.Typed.Record.make
      (module struct
        (* reimport the module that typed_fields just derived *)
        module Typed_field = Typed_field

        let label_for_field = `Inferred

        (* provide a form computation for each field in the record *)
        let form_for_field : type a. a Typed_field.t -> Bonsai.graph -> a Form.t Bonsai.t =
          fun typed_field graph ->
          match typed_field with
          | Some_string ->
            Form.Elements.Textbox.string ~allow_updates_when_focused:`Always () graph
          | An_int ->
            Form.Elements.Number.int
              ~default:0
              ~step:1
              ~allow_updates_when_focused:`Always
              ()
              graph
          | On_or_off -> Form.Elements.Checkbox.bool ~default:false () graph
        ;;
      end)
  ;;
end

(* $MDX part-end *)

(* $MDX part-begin=variant_form *)
module Variant = struct
  type t =
    | A
    | B of int
    | C of string
  [@@deriving typed_variants, sexp_of]

  let form : Bonsai.graph -> t Form.t Bonsai.t =
    Form.Typed.Variant.make
      (module struct
        (* reimport the module that typed_fields just derived *)
        module Typed_variant = Typed_variant

        let label_for_variant = `Inferred
        let initial_choice = `First_constructor

        (* provide a form computation for constructor in the variant *)
        let form_for_variant
          : type a. a Typed_variant.t -> Bonsai.graph -> a Form.t Bonsai.t
          =
          fun typed_field graph ->
          match typed_field with
          | A -> Form.return () |> Bonsai.return
          | B -> Form.Elements.Textbox.int ~allow_updates_when_focused:`Always () graph
          | C -> Form.Elements.Textbox.string ~allow_updates_when_focused:`Always () graph
        ;;
      end)
  ;;
end

(* $MDX part-end *)

let () =
  Util.run
    (fun graph ->
      let%arr record_form = Record.form graph in
      Form.view_as_vdom record_form)
    ~id:"record_form"
;;

(* $MDX part-begin=record_form_view *)
let view_for_form : Bonsai.graph -> Vdom.Node.t Bonsai.t =
  fun graph ->
  let%arr record_form = Record.form graph
  and variant_form = Variant.form graph in
  let form = Form.both record_form variant_form in
  let value = Form.value form in
  Vdom.Node.div
    [ Form.view_as_vdom form
    ; Vdom.Node.sexp_for_debugging ([%sexp_of: (Record.t * Variant.t) Or_error.t] value)
    ]
;;

(* $MDX part-end *)

let () = Util.run view_for_form ~id:"record_form_view"

(* $MDX part-begin=int_textbox *)
let int : Bonsai.graph -> int Form.t Bonsai.t =
  fun graph ->
  let form = Form.Elements.Textbox.string ~allow_updates_when_focused:`Always () graph in
  let%arr form = form in
  Form.project form ~parse_exn:Int.of_string ~unparse:Int.to_string
;;

(* $MDX part-end *)

let () =
  Util.run
    (fun graph ->
      let%arr form = int graph in
      Vdom.Node.div
        [ Form.view_as_vdom form
        ; Vdom.Node.sexp_for_debugging ([%sexp_of: int Or_error.t] (Form.value form))
        ])
    ~id:"int_textbox"
;;
