open! Core
open! Bonsai_web
open! Vdom
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form
module Query_box = Bonsai_web_ui_query_box

module Css =
  [%css.raw
    {|
  .list_container {
    background: white;
    border: solid 1px black;
    padding: 5px;
    min-width: 150px;
  }
  .selected_item {
    background: yellow;
  }

  |}]

module Example_params = struct
  type t =
    { suggestion_list_kind : Query_box.Suggestion_list_kind.t
    ; expand_direction : Query_box.Expand_direction.t
    ; max_visible_items : int
    }
  [@@deriving typed_fields]

  let default =
    { suggestion_list_kind = Transient_overlay
    ; expand_direction = Down
    ; max_visible_items = 10
    }
  ;;
end

let component =
  let fruits =
    String.Map.of_alist_exn
      (List.map
         ~f:(fun x -> x, ())
         [ "apple"
         ; "apricot"
         ; "avocado"
         ; "banana"
         ; "blackberry"
         ; "blueberry"
         ; "breadfruit"
         ; "cantaloupe"
         ; "clementine"
         ; "fig"
         ; "grapefruit"
         ; "orange"
         ; "raspberry"
         ; "strawberry"
         ; "tangerine"
         ; "watermelon"
         ])
  in
  let%sub selected_items, add_item =
    Bonsai.state_machine0
      [%here]
      (module struct
        type t = string list [@@deriving sexp, equal]
      end)
      (module String)
      ~default_model:[]
      ~apply_action:(fun ~inject:_ ~schedule_event:_ items item -> item :: items)
  in
  let%sub form =
    Form.Typed.Record.make
      (module struct
        module Typed_field = Example_params.Typed_field

        let form_for_field : type a. a Typed_field.t -> a Form.t Computation.t = function
          | Suggestion_list_kind ->
            Form.Elements.Dropdown.enumerable
              [%here]
              (module Query_box.Suggestion_list_kind)
          | Expand_direction ->
            Form.Elements.Dropdown.enumerable [%here] (module Query_box.Expand_direction)
          | Max_visible_items -> Form.Elements.Number.int [%here] ~default:10 ~step:1 ()
        ;;
      end)
  in
  let%sub { Example_params.suggestion_list_kind; expand_direction; max_visible_items } =
    return (form >>| Form.value_or_default ~default:Example_params.default)
  in
  let%sub query_box =
    Query_box.create
      (module String)
      ~suggestion_list_kind
      ~expand_direction
      ~max_visible_items
      ~selected_item_attr:(Value.return (Attr.class_ Css.selected_item))
      ~extra_list_container_attr:(Value.return (Attr.class_ Css.list_container))
      ~extra_input_attr:(Value.return (Attr.placeholder "Filter Fruits"))
      ~on_select:add_item
      ~f:(fun query ->
        let%arr query = query in
        Map.filter_mapi fruits ~f:(fun ~key:fruit ~data:() ->
          if Fuzzy_match.is_match ~char_equal:Char.Caseless.equal fruit ~pattern:query
          then Some (Node.div [ Node.text fruit ])
          else None))
      ()
  in
  let%arr selected_items = selected_items
  and query_box = query_box
  and form = form >>| Form.view_as_vdom in
  Node.div
    [ form
    ; query_box
    ; Node.ul (List.map selected_items ~f:(fun item -> Node.li [ Node.text item ]))
    ]
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
