open! Core
open! Bonsai_web
open! Vdom
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form
module Query_box = Bonsai_web_ui_query_box

module Css =
  [%css
    stylesheet
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

module Input_source = struct
  type t =
    | Small_and_static_list_of_fruits
    | Large_and_rapidly_changing_filepaths
  [@@deriving enumerate, sexp, equal, compare]
end

module Example_params = struct
  type t =
    { suggestion_list_kind : Query_box.Suggestion_list_kind.t
    ; expand_direction : Query_box.Expand_direction.t
    ; max_visible_items : int
    ; input_source : Input_source.t
    ; filter_strategy : Query_box.Filter_strategy.t
    }
  [@@deriving typed_fields]

  let default =
    { suggestion_list_kind = Transient_overlay
    ; expand_direction = Down
    ; max_visible_items = 10
    ; input_source = Small_and_static_list_of_fruits
    ; filter_strategy = Fuzzy_search_and_score
    }
  ;;
end

let items =
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
  ]
;;

let component =
  let%sub selected_items, add_item =
    Bonsai.state_machine0
      ()
      ~sexp_of_model:[%sexp_of: string list]
      ~equal:[%equal: string list]
      ~sexp_of_action:[%sexp_of: String.t]
      ~default_model:[]
      ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) items item ->
        item :: items)
  in
  let%sub form =
    Form.Typed.Record.make
      (module struct
        module Typed_field = Example_params.Typed_field

        let label_for_field = `Inferred

        let form_for_field : type a. a Typed_field.t -> a Form.t Computation.t = function
          | Suggestion_list_kind ->
            Form.Elements.Dropdown.enumerable (module Query_box.Suggestion_list_kind)
          | Expand_direction ->
            Form.Elements.Dropdown.enumerable (module Query_box.Expand_direction)
          | Max_visible_items -> Form.Elements.Number.int ~default:10 ~step:1 ()
          | Input_source -> Form.Elements.Dropdown.enumerable (module Input_source)
          | Filter_strategy ->
            Form.Elements.Dropdown.enumerable (module Query_box.Filter_strategy)
        ;;
      end)
  in
  let%sub { suggestion_list_kind
          ; expand_direction
          ; max_visible_items
          ; input_source
          ; filter_strategy
          }
    =
    return (form >>| Form.value_or_default ~default:Example_params.default)
  in
  let%sub data =
    match%sub input_source with
    | Small_and_static_list_of_fruits ->
      Bonsai.const (String.Map.of_alist_exn (List.map ~f:(fun x -> x, x) items))
    | Large_and_rapidly_changing_filepaths ->
      let module Action = struct
        let quickcheck_generator_string =
          Quickcheck.Generator.map
            (List.quickcheck_generator (Quickcheck.Generator.of_list items))
            ~f:(String.concat ~sep:"/")
        ;;

        type t =
          | Add of string
          | Remove of string [@quickcheck.weight 1. /. 5.]
        [@@deriving sexp_of, quickcheck]
      end
      in
      let%sub map, inject =
        Bonsai.state_machine0
          ~default_model:String.Map.empty
          ~apply_action:(fun _context model action ->
            match action with
            | Action.Add key -> Map.set model ~key ~data:key
            | Remove key -> Map.remove model key)
          ()
      in
      let%sub add_random_item =
        let%arr inject = inject in
        let%bind.Effect item =
          Effect.of_sync_fun
            (fun () ->
               Quickcheck.Generator.generate
                 Action.quickcheck_generator
                 ~size:6
                 ~random:(Splittable_random.State.create Random.State.default))
            ()
        in
        inject item
      in
      let%sub () =
        Bonsai.Clock.every
          ~when_to_start_next_effect:`Wait_period_after_previous_effect_starts_blocking
          (Time_ns.Span.of_sec 0.2)
          add_random_item
      in
      return map
  in
  let%sub query_box =
    (* [filter_strategy] is not a dynamic parameter to [Query_box.stringable],
       so we have to add introduce dynamism ourselves with [match%sub]. *)
    match%sub filter_strategy with
    | Fuzzy_match ->
      Query_box.stringable
        (module String)
        ~suggestion_list_kind
        ~expand_direction
        ~max_visible_items
        ~selected_item_attr:(Value.return Css.selected_item)
        ~extra_list_container_attr:(Value.return Css.list_container)
        ~extra_input_attr:(Value.return (Attr.placeholder "Filter Fruits"))
        ~filter_strategy:Fuzzy_search_and_score
        ~on_select:add_item
        data
    | Fuzzy_search_and_score ->
      Query_box.stringable
        (module String)
        ~suggestion_list_kind
        ~expand_direction
        ~max_visible_items
        ~selected_item_attr:(Value.return Css.selected_item)
        ~extra_list_container_attr:(Value.return Css.list_container)
        ~extra_input_attr:(Value.return (Attr.placeholder "Filter Fruits"))
        ~filter_strategy:Fuzzy_match
        ~on_select:add_item
        data
  in
  let%arr selected_items = selected_items
  and query_box = query_box
  and form = form >>| Form.view_as_vdom in
  Node.div
    [ form
    ; Query_box.view query_box
    ; Node.ul (List.map selected_items ~f:(fun item -> Node.li [ Node.text item ]))
    ; Node.button
        ~attrs:[ Attr.on_click (fun _ -> Query_box.focus_input query_box) ]
        [ Node.text "Focus the query_box" ]
    ]
;;

let () = Bonsai_web.Start.start component
