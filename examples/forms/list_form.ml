open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form
module E = Form.Elements

module S =
  [%css.raw
    {|
    .pill {
      padding: 2px;
      background-color: #d0d0d0;
      margin-right: 2px;
      margin-bottom: 2px;
      border-radius: 2px;
      cursor: pointer;
    }

    .container {
      margin-top: 5px;
      display: flex;
      flex-flow: row wrap;
    }

    .list_forms {
      display: flex;
      flex-direction: column;
    }
  |}]

module Simple_list = struct
  module T = struct
    type t =
      | Bulbasaur
      | Squirtle
      | Charmander
      | Chikorita
      | Totodile
      | Cyndaquil
      | Treecko
      | Mudkip
      | Torchic
      | Turtwig
      | Piplup
      | Chimchar
      | Snivy
      | Oshawott
      | Tepig
      | Chespin
      | Froakie
      | Fennekin
      | Rowlett
      | Poplio
      | Litten
      | Grookie
      | Sobble
      | Scorbunny
    [@@deriving equal, sexp, variants]

    let to_string = Variants.to_name
    let of_string s = s |> String.lowercase |> Sexp.of_string |> t_of_sexp
  end

  include T

  let component =
    E.Multiple.stringable_list
      [%here]
      (module T)
      ~extra_pill_container_attr:(Value.return (Vdom.Attr.class_ S.container))
      ~extra_pill_attr:(Value.return (Vdom.Attr.class_ S.pill))
  ;;
end

module Advanced_list = struct
  type t =
    { a : int
    ; b : string
    }
  [@@deriving sexp_of, fields, typed_fields]

  let form_of_t =
    Form.Typed.Record.make
      (module struct
        module Typed_field = Typed_field

        let form_for_field : type a. a Typed_field.t -> a Form.t Computation.t = function
          | A -> E.Textbox.int [%here]
          | B -> E.Textbox.string [%here]
        ;;
      end)
  ;;

  let inner_form =
    Form.Elements.Multiple.list
      [%here]
      form_of_t
      ~button_placement:`Indented
      ~add_element_text:(Value.return "add inner")
  ;;

  let outer_form =
    Form.Elements.Multiple.list
      [%here]
      inner_form
      ~button_placement:`Indented
      ~add_element_text:(Value.return "add outer")
  ;;

  let starting_value =
    [ [ { a = 5; b = "hello" }; { a = 15; b = "world" } ]
    ; [ { a = 20; b = "foo" }; { a = 11; b = "bar" }; { a = 3; b = "baz" } ]
    ]
  ;;

  let component =
    let%sub outer_form = outer_form in
    let%sub outer_form =
      Form.Dynamic.with_default (Bonsai.Value.return starting_value) outer_form
    in
    return outer_form
  ;;
end

let component =
  let%sub simple_list = Simple_list.component in
  let%sub advanced_list = Advanced_list.component in
  let%arr simple_list = simple_list
  and advanced_list = advanced_list in
  let simple_output =
    Vdom.Node.sexp_for_debugging
      [%sexp (Form.value simple_list : Simple_list.t list Or_error.t)]
  in
  let advanced_output =
    Vdom.Node.sexp_for_debugging
      [%sexp (Form.value advanced_list : Advanced_list.t list list Or_error.t)]
  in
  Vdom.Node.div
    ~attr:(Vdom.Attr.class_ S.list_forms)
    [ Vdom.Node.h1 [ Vdom.Node.text "List Forms" ]
    ; Vdom.Node.h3 [ Vdom.Node.text "Simple form that accepts Pokemon starters" ]
    ; Form.View.to_vdom (Form.view simple_list)
    ; simple_output
    ; Vdom.Node.h3 [ Vdom.Node.text "Advanced form that builds a list of records" ]
    ; Form.View.to_vdom (Form.view advanced_list)
    ; advanced_output
    ]
;;
