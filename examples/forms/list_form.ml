open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_automatic_view
module E = Form.Elements

module S =
[%css
stylesheet
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
      (module T)
      ~equal:[%equal: T.t]
      ~extra_pill_container_attr:(Bonsai.return S.container)
      ~extra_pill_attr:(Bonsai.return S.pill)
  ;;
end

module Advanced_list = struct
  module Config = struct
    type t =
      { duration_hrs : int
      ; price : float
      }
    [@@deriving sexp_of, typed_fields]
  end

  module Per_symbol = struct
    type t =
      { symbol : string
      ; configs : Config.t list
      }
    [@@deriving sexp_of, typed_fields]
  end

  let form_for_config =
    Form.Typed.Record.make
      (module struct
        module Typed_field = Config.Typed_field

        let label_for_field = `Inferred

        let form_for_field : type a. a Typed_field.t -> Bonsai.graph -> a Form.t Bonsai.t =
          fun typed_field graph ->
          match typed_field with
          | Duration_hrs -> E.Textbox.int ~allow_updates_when_focused:`Never () graph
          | Price -> E.Textbox.float ~allow_updates_when_focused:`Never () graph
        ;;
      end)
  ;;

  let form_per_symbol graph =
    let symbol_form = E.Textbox.string ~allow_updates_when_focused:`Never () graph in
    Form.Typed.Record.make
      (module struct
        module Typed_field = Per_symbol.Typed_field

        let label_for_field = `Inferred

        let form_for_field : type a. a Typed_field.t -> Bonsai.graph -> a Form.t Bonsai.t =
          fun typed_field graph ->
          match typed_field with
          | Symbol -> symbol_form
          | Configs ->
            let add_element_text =
              let%arr symbol_form = symbol_form in
              let symbol = Form.value_or_default ~default:"" symbol_form in
              sprintf "add config for %s" symbol
            in
            Form.Elements.Multiple.list
              form_for_config
              ~button_placement:`Indented
              ~add_element_text
              graph
        ;;
      end)
      graph
  ;;

  let many_symbols =
    Form.Elements.Multiple.list
      form_per_symbol
      ~button_placement:`Indented
      ~add_element_text:(Bonsai.return "add new symbol")
  ;;

  let starting_value =
    [ { Per_symbol.symbol = "aapl"; configs = [ { duration_hrs = 5; price = 3.14 } ] }
    ; { Per_symbol.symbol = "tsla"
      ; configs =
          [ { duration_hrs = 6; price = 4.04 }; { duration_hrs = 16; price = 12.0 } ]
      }
    ; { Per_symbol.symbol = "msft"; configs = [] }
    ]
  ;;

  let component graph =
    let many_symbols = many_symbols graph in
    let many_symbols =
      Form.Dynamic.with_default (Bonsai.return starting_value) many_symbols graph
    in
    many_symbols
  ;;
end

let component graph =
  let simple_list = Simple_list.component graph in
  let advanced_list = Advanced_list.component graph in
  let%arr simple_list = simple_list
  and advanced_list = advanced_list in
  let simple_output =
    Vdom.Node.sexp_for_debugging
      [%sexp (Form.value simple_list : Simple_list.t list Or_error.t)]
  in
  let advanced_output =
    Vdom.Node.sexp_for_debugging
      [%sexp (Form.value advanced_list : Advanced_list.Per_symbol.t list Or_error.t)]
  in
  Vdom.Node.div
    ~attrs:[ S.list_forms ]
    [ Vdom.Node.h1 [ Vdom.Node.text "List Forms" ]
    ; Vdom.Node.h3 [ Vdom.Node.text "Simple form that accepts Pokemon starters" ]
    ; Form.View.to_vdom (Form.view simple_list)
    ; simple_output
    ; Vdom.Node.h3 [ Vdom.Node.text "Advanced form that builds a list of records" ]
    ; Form.View.to_vdom (Form.view advanced_list)
    ; advanced_output
    ]
;;
