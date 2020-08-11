open Core_kernel
module Bonsai = Bonsai_web.Bonsai

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

module Widget = Multi_select_widget.Multi_factor.Make (String) (Attribute)

let input =
  let subwidgets =
    List.map Attribute.all ~f:(fun attr ->
      let all_items =
        String.Set.of_list
          (match attr with
           | Name       -> [ "Henry VIII"; "Bill Gates"; "Alan Turing"; "Ada Lovelace" ]
           | Department -> [ "Tech"; "The Tudor Court" ]
           | Office     -> [ "LDN"; "NYC"; "HKG" ])
      in
      attr, { Widget.Input.default_selection_status = Selected; all_items })
    |> Attribute.Map.of_alist_exn
  in
  { Widget.Input.subwidgets; id_prefix = "multi-select-widget-example" }
;;

let bonsai =
  let%map.Bonsai.Arrow widget_result =
    Widget.bonsai ~all_keys:(Attribute.Set.of_list Attribute.all) ()
  in
  let open Virtual_dom.Vdom in
  Node.div
    []
    [ Node.h2 [] [ Node.text "Selection demo" ]
    ; Widget.Result.view_with_keydown_handler widget_result
    ; Node.text
        (sprintf
           "You have selected %d items"
           (Map.data widget_result.selection |> String.Set.union_list |> Set.length))
    ]
;;

let (_handle : _ Bonsai_web.Arrow.Start.Handle.t) =
  Bonsai_web.Arrow.Start.start_standalone
    ~initial_input:input
    ~bind_to_element_with_id:"app"
    bonsai
;;
