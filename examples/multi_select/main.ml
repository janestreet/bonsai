open Core
open Bonsai_web
open Bonsai.Let_syntax

module Attribute = struct
  module T = struct
    type t =
      | Name
      | Department
      | Office
    [@@deriving sexp, compare, enumerate]
  end

  include T
  include Sexpable.To_stringable (T)
  include Comparable.Make (T)

  let name_singular = "attribute"
  let name_plural   = "attributes"
end

module Widget = Bonsai_web_ui_multi_select.Multi_factor.Make (String) (Attribute)

let subwidgets =
  Attribute.all
  |> List.map ~f:(fun attr ->
    let all_items =
      String.Set.of_list
        (match attr with
         | Name       -> [ "Henry VIII"; "Bill Gates"; "Alan Turing"; "Ada Lovelace" ]
         | Department -> [ "Tech"; "The Tudor Court" ]
         | Office     -> [ "LDN"; "NYC"; "HKG" ])
    in
    attr, { Widget.default_selection_status = Selected; all_items })
  |> Attribute.Map.of_alist_exn
  |> Value.return
;;

let id_prefix = Value.return "multi-select-widget-example"

let bonsai =
  let%sub widget_result =
    Widget.bonsai ~all_keys:(Attribute.Set.of_list Attribute.all) ~id_prefix subwidgets
  in
  return
  @@ let%map widget_result = widget_result in
  let open Virtual_dom.Vdom in
  Node.div
    [ Node.h2 [ Node.text "Selection demo" ]
    ; Widget.Result.view_with_keydown_handler widget_result
    ; Node.text
        (sprintf
           "You have selected %d items"
           (Map.data widget_result.selection |> String.Set.union_list |> Set.length))
    ]
;;

let (_handle : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" bonsai
;;
