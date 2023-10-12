open! Core
module Vdom = Virtual_dom.Vdom
open Vdom
module Position_tracker = Bonsai_web_ui_element_size_hooks.Position_tracker
module Bulk_size_tracker = Bonsai_web_ui_element_size_hooks.Bulk_size_tracker
module Position = Position_tracker.Position

module Make (Name : Types.Name) = struct
  module Bindgen = Bindgen.Make (Name)
  open Bindgen

  module Trackers : sig
    type t
    type get_attr := Name.t -> Vdom.Attr.t

    val create : get_position_tracker_attr:get_attr -> get_size_tracker_attr:get_attr -> t
    val get_attr : t -> id:Name.t -> Vdom.Attr.t
    val track_resizing : t -> curr_id:Name.Count.t -> Vdom.Attr.t * Name.Count.t
  end = struct
    type get_attr = Name.t -> Vdom.Attr.t

    type t =
      { get_position_tracker_attr : get_attr
      ; get_size_tracker_attr : get_attr
      }

    let create ~get_position_tracker_attr ~get_size_tracker_attr =
      { get_position_tracker_attr; get_size_tracker_attr }
    ;;

    let track_resizing { get_size_tracker_attr; _ } ~curr_id =
      let id, curr_id = Name.next curr_id in
      get_size_tracker_attr id, curr_id
    ;;

    let get_attr { get_position_tracker_attr; get_size_tracker_attr } ~id =
      let position_tracker = get_position_tracker_attr id in
      let size_tracker = get_size_tracker_attr id in
      Vdom.Attr.many [ position_tracker; size_tracker ]
    ;;
  end

  module Style =
  [%css
  stylesheet
    {|
.map {
  position:relative;
  backface-visibility: hidden;
}

.map * {
}

.map>div {
  position: relative;
  opacity:1.0;
}

.map>svg {
  position:absolute;
  width:100%;
  height:100%;
  inset:0;
  pointer-events:none;
  z-index:2;
}


.redirect {
  width: 0px;
  height: 0px;
}

.testcase {
  border-radius: 0.25em;
  align-items: center;
  contain: paint;
  border-radius: 8px;
}

.computation {
  display: block;
  line-height: 1em;
  user-select: none;
  width:fit-content;
  height:fit-content;
}

.computation {
  border: 1px solid black;
  padding: 0.5em;
  border-radius:4px;
}


.computation::before {
  content: attr(data-kind);
  position: absolute;
  display: block;
  display: none;
  padding: 0;
  font-size: 0.5em;
  line-height: 0.5em;
  width: fit-content;
  transform: translate(0, -2em);
  color: inherit;
  background: inherit;
  padding: 0.5em;
  margin-bottom: -1em;
  border-radius: 3px;
  z-index:3;
  border:1px solid currentcolor;
}

.computation::before {
  position: relative;
  left: unset;
  margin-left:auto;
  margin-right:auto;
  background:white;
}

.sub {
  border: 1px dashed red;
  padding: 0.25em;
  border-radius:0.25em;
}

.wrap {
  border: 1px dashed green;
  padding: 0.25em;
  border-radius:0.25em;
}

.vbox, .hbox {
  display: flex;
  justify-content: space-around;
  align-items: center;
  align-self: stretch;
}

.vbox {
  flex-direction: column;
  gap: 2em;
}

.hbox {
  flex-direction: row;
  gap: 2em;
}

.hidden {
  display: none;
}

.ontop:hover + .hidden {
  display: block;
  color: red;
}
                         |}]

  let box t = function
    | [] -> Node.none
    | [ child ] -> child
    | children -> Node.div ~attrs:[ t ] children
  ;;

  let hbox, vbox = box Style.hbox, box Style.vbox

  let vbox_hbox = function
    | `Left_to_right -> vbox, hbox
    | `Top_to_bottom -> hbox, vbox
  ;;

  let conn kind name =
    match kind with
    | `Provide -> Attr.create "src-name" (Name.to_string name)
    | `Consume -> Attr.class_ ("dest-class-" ^ Name.to_string name)
  ;;

  module Edge = struct
    module T = struct
      type t =
        { from : Name.t
        ; to_ : Name.t
        }
      [@@deriving sexp_of, compare]
    end

    include T
    include Comparable.Make_plain (T)

    let all_roots (al_graph : Name.Set.t Name.Map.t) =
      let all = Core.Map.key_set al_graph in
      Core.Map.fold ~init:all al_graph ~f:(fun ~key:_ ~data acc -> Core.Set.diff acc data)
    ;;

    let topological_sort ~init (al_graph : Name.Set.t Name.Map.t)
      : Name.Set.t Int.Map.t Or_error.t
      =
      let rec loop ~al_graph ~acc =
        match Core.Map.is_empty al_graph with
        | true -> Or_error.return acc
        | false ->
          let all_roots = all_roots al_graph in
          (match Core.Set.is_empty all_roots with
           | true -> Or_error.error_string "cycle!"
           | false ->
             let al_graph =
               Core.Map.filteri al_graph ~f:(fun ~key ~data:_ ->
                 not (Core.Set.mem all_roots key))
             in
             let acc = Core.Map.set acc ~key:(Core.Map.length acc) ~data:all_roots in
             loop ~al_graph ~acc)
      in
      loop ~al_graph ~acc:init
    ;;
  end

  module Connections_state = struct
    type t =
      { id_to_source : Name.Set.t Name.Map.t
      ; dest_to_id : Name.Set.t Name.Map.t
      }
    [@@deriving sexp_of]

    let empty = { id_to_source = Name.Map.empty; dest_to_id = Name.Map.empty }

    let add_provider ~id ~source t =
      let id_to_source =
        let prev_value =
          Option.value (Map.find t.id_to_source id) ~default:Name.Set.empty
        in
        let new_value = Set.add prev_value source in
        Map.set t.id_to_source ~key:id ~data:new_value
      in
      { t with id_to_source }
    ;;

    let add_consumer ~id ~dest t =
      let dest_to_id =
        let prev_value =
          Option.value (Map.find t.dest_to_id dest) ~default:Name.Set.empty
        in
        let new_value = Set.add prev_value id in
        Map.set t.dest_to_id ~key:dest ~data:new_value
      in
      { t with dest_to_id }
    ;;

    let merge ta tb =
      let merge_maps
        : Name.Set.t Name.Map.t -> Name.Set.t Name.Map.t -> Name.Set.t Name.Map.t
        =
        fun a b ->
        Map.merge a b ~f:(fun ~key:_ -> function
          | `Both (a, b) -> Some (Set.union a b)
          | `Left x | `Right x -> Some x)
      in
      { id_to_source = merge_maps ta.id_to_source tb.id_to_source
      ; dest_to_id = merge_maps ta.dest_to_id tb.dest_to_id
      }
    ;;

    let merge_list (ts : t list) = List.fold ts ~init:empty ~f:merge

    let to_edges (t : t) =
      Map.fold ~init:Edge.Set.empty t.id_to_source ~f:(fun ~key:from ~data:sources acc ->
        Set.fold sources ~init:acc ~f:(fun acc src ->
          match Map.find t.dest_to_id src with
          | None -> acc
          | Some to_'s ->
            Set.fold to_'s ~init:acc ~f:(fun acc to_ -> Set.add acc { Edge.from; to_ })))
    ;;
  end

  let rec value_to_html
    ~direction
    ~here:_
    ~connections_state
    ~id_to_vdom
    ~point_to
    ~trackers
    (me : Value.t)
    =
    let open Connections_state in
    let id = me.value_id in
    let id_attr = Vdom.Attr.create "outside-id" (Name.to_string id) in
    let tracker = lazy (Trackers.get_attr trackers ~id) in
    let node = Option.value (Map.find id_to_vdom point_to) ~default:(Vdom.Node.div []) in
    match me.value_kind with
    | Fake -> [], connections_state
    | Singleton ->
      let nodes =
        [ [ Node.div
              ~key:(Name.to_string id)
              ~attrs:
                [ Attr.create "data-kind" "singleton"
                ; conn `Provide point_to
                ; force tracker
                ; id_attr
                ]
              [ node ]
          ]
        ]
      in
      let connections_state = add_provider connections_state ~id ~source:point_to in
      nodes, connections_state
    | Redirect { name } ->
      let nodes =
        [ [ Node.div
              ~key:(Name.to_string id)
              ~attrs:
                [ Style.redirect
                ; Attr.create "data-kind" "redirect"
                ; conn `Provide point_to
                ; conn `Consume name
                ; force tracker
                ; id_attr
                ]
              [ node ]
          ]
        ]
      in
      let connections_state =
        add_provider connections_state ~id ~source:point_to |> add_consumer ~id ~dest:name
      in
      nodes, connections_state
    | Named name ->
      let nodes =
        [ [ Node.div
              ~key:(Name.to_string id)
              ~attrs:
                [ Attr.create "data-kind" "named"
                ; conn `Provide point_to
                ; conn `Consume name
                ; force tracker
                ; id_attr
                ]
              [ node ]
          ]
        ]
      in
      let connections_state =
        add_provider connections_state ~id ~source:point_to |> add_consumer ~id ~dest:name
      in
      nodes, connections_state
    | Mapn [] -> failwith "mapn with an empty list?"
    | Mapn children ->
      let me = me.value_id in
      let children, connections_state =
        List.fold
          children
          ~init:([], connections_state)
          ~f:(fun (children, connections_state) child ->
          match child.value_kind with
          | Named name ->
            children, Connections_state.add_consumer connections_state ~id ~dest:name
          | _ -> child :: children, connections_state)
      in
      let children, connections_state =
        children
        |> List.map ~f:(fun v ->
             value_to_html
               ~direction
               ~connections_state
               ~id_to_vdom
               ~trackers
               ~here:v.value_here
               ~point_to:me
               v)
        |> List.reduce_balanced ~f:(fun (a_nodes, state_a) (b_nodes, state_b) ->
             let abs, rest = List.zip_with_remainder a_nodes b_nodes in
             let abs : Node.t list list = List.map abs ~f:(fun (a, b) -> a @ b) in
             let nodes =
               match rest with
               | None -> abs
               | Some (First a) -> abs @ a
               | Some (Second b) -> abs @ b
             in
             nodes, Connections_state.merge state_a state_b)
        |> Option.value ~default:([], connections_state)
      in
      let nodes =
        [ Node.div
            ~key:(Name.to_string id)
            ~attrs:
              [ Attr.create "kind" "mapn"
              ; conn `Consume me
              ; conn `Provide point_to
              ; id_attr
              ; force tracker
              ; Vdom.Attr.create "my-id" (Name.to_string id)
              ]
            [ node ]
        ]
        :: children
      in
      let connections_state =
        Connections_state.add_consumer ~id ~dest:me connections_state
        |> Connections_state.add_provider ~id ~source:point_to
      in
      nodes, connections_state
  ;;

  let value_to_html
    ~direction
    ~here
    ~point_to
    ~id_to_vdom
    ~trackers
    ~connections_state
    value
    =
    let vbox, hbox = vbox_hbox direction in
    let children, connections_state =
      value_to_html
        ~direction
        ~connections_state
        ~id_to_vdom
        ~trackers
        ~here
        ~point_to
        value
    in
    List.rev_map ~f:vbox children |> hbox, connections_state
  ;;

  let comp_pos ~here ~there =
    match there with
    | Some x -> Some x
    | None -> here
  ;;

  let rec computation_to_html
    ~direction
    ~here
    ~(curr_id : Name.Count.t)
    ~id_to_vdom
    ~point_to
    ~connections_state
    ~trackers
    =
    let vbox, hbox = vbox_hbox direction in
    function
    | { Computation.kind = Value v; free_variables = _; here = there } ->
      let here = comp_pos ~here ~there in
      ( value_to_html ~here ~direction ~point_to ~id_to_vdom ~trackers ~connections_state v
      , curr_id )
    | { kind = Bindings { bindings = []; last_body }; free_variables = _; here = there }
      ->
      let here = comp_pos ~here ~there in
      computation_to_html
        ~direction
        ~here
        ~curr_id
        ~point_to
        ~id_to_vdom
        ~connections_state
        ~trackers
        last_body
    | { kind = Bindings { bindings; last_body }; free_variables = _; here = there } ->
      let here = comp_pos ~here ~there in
      let curr_id, rows_and_state =
        let organized, curr_id =
          Transform.organize_bindings bindings ~curr_id ~last_body ~point_to
        in
        List.fold_map organized ~init:curr_id ~f:(fun curr_id row ->
          let curr_id, column_and_states =
            List.fold_map
              row
              ~init:curr_id
              ~f:(fun curr_id { Types.Binding.as_; bound } ->
              let here =
                match bound.here with
                | Some x -> Some x
                | None -> here
              in
              Tuple2.swap
              @@ computation_to_html
                   ~direction
                   ~connections_state
                   ~curr_id
                   ~id_to_vdom
                   ~trackers
                   ~here
                   ~point_to:as_
                   bound)
          in
          ( curr_id
          , ( vbox (List.map column_and_states ~f:Tuple2.get1)
            , Connections_state.merge_list (List.map column_and_states ~f:Tuple2.get2) ) ))
      in
      ( ( hbox (List.map rows_and_state ~f:Tuple2.get1)
        , Connections_state.merge_list (List.map rows_and_state ~f:Tuple2.get2) )
      , curr_id )
    | { kind = Wrapping { name; introduces; bodies }; free_variables = _; here = there }
      ->
      let here = comp_pos ~here ~there in
      let introduces_row, introduces_state =
        List.unzip
          (List.map introduces ~f:(fun introduced ->
             value_to_html
               ~direction
               ~connections_state
               ~id_to_vdom
               ~trackers
               ~here
               ~point_to
               { value_kind = Value.Named introduced
               ; value_here = here
               ; value_id = introduced
               }))
      in
      let (body_row, body_state), curr_id =
        let curr_id, list_to_partition =
          List.fold_map bodies ~init:curr_id ~f:(fun curr_id body ->
            Tuple2.swap
            @@ computation_to_html
                 ~direction
                 ~connections_state
                 ~curr_id
                 ~id_to_vdom
                 ~trackers
                 ~here
                 ~point_to
                 body)
        in
        List.unzip list_to_partition, curr_id
      in
      ( ( Node.div
            ~attrs:[ Style.wrap ]
            [ Node.text ("Wrapping " ^ name); vbox introduces_row; vbox body_row ]
        , Connections_state.(merge (merge_list introduces_state) (merge_list body_state))
        )
      , curr_id )
  ;;

  let computation_to_html
    c
    ~direction
    ~(curr_id : Name.Count.t)
    ~id_to_vdom
    ~trackers
    ~here
    =
    let out, curr_id = Name.next curr_id in
    computation_to_html c ~direction ~curr_id ~id_to_vdom ~point_to:out ~here ~trackers
  ;;

  let to_vdom
    ~(curr_id : Name.Count.t Bonsai.Value.t)
    ~(direction : [ `Left_to_right | `Top_to_bottom ])
    ~(node_to_vdom : Name.t Bonsai.Value.t -> Vdom.Node.t Bonsai.Computation.t)
    ~(edge_to_svg :
        edge:Edge.t Bonsai.Value.t
        -> from:Position.t Bonsai.Value.t
        -> to_:Position.t Bonsai.Value.t
        -> Vdom.Node.t Bonsai.Computation.t)
    (computation : Computation.t Bonsai_web.Value.t)
    : (Vdom.Node.t * Name.Count.t) Bonsai_web.Computation.t
    =
    let open Bonsai.Let_syntax in
    let%sub { positions
            ; get_attr = get_position_tracker_attr
            ; update = update_position_tracker
            }
      =
      Position_tracker.component (module Name)
    in
    let%sub sizes, get_size_tracker_attr =
      Bulk_size_tracker.component (module Name) Ignore_stale
    in
    let%sub () =
      let%sub callback =
        let%arr update_position_tracker = update_position_tracker in
        Fn.const update_position_tracker
      in
      Bonsai.Edge.on_change
        ~sexp_of_model:[%sexp_of: Bulk_size_tracker.Dimensions.t Name.Map.t]
        ~equal:[%equal: Bulk_size_tracker.Dimensions.t Name.Map.t]
        sizes
        ~callback
    in
    let%sub trackers =
      let%arr get_position_tracker_attr = get_position_tracker_attr
      and get_size_tracker_attr = get_size_tracker_attr in
      Trackers.create ~get_position_tracker_attr ~get_size_tracker_attr
    in
    let%sub trackers =
      let%arr trackers = trackers in
      trackers
    in
    let%sub all_ids =
      let%arr computation = computation in
      let finder_of_all_ids =
        object
          inherit ['acc] Bindgen.Types.fold as super
          method! name name acc = super#name name (Set.add acc name)
        end
      in
      finder_of_all_ids#computation computation Name.Set.empty
    in
    let%sub all_ids =
      let%arr all_ids = all_ids in
      all_ids
    in
    let%sub id_to_vdom =
      let%sub unit_map =
        let%arr all_ids = all_ids in
        Core.Set.to_map all_ids ~f:(Fn.const ())
      in
      Bonsai.assoc (module Name) unit_map ~f:(fun key _ -> node_to_vdom key)
    in
    let%sub id_to_vdom =
      let%arr id_to_vdom = id_to_vdom in
      id_to_vdom
    in
    let%sub (computation_as_html, connections_state), curr_id =
      let%arr computation = computation
      and id_to_vdom = id_to_vdom
      and trackers = trackers
      and curr_id = curr_id in
      computation_to_html
        ~direction
        ~here:None
        ~curr_id
        ~trackers
        ~id_to_vdom
        computation
        ~connections_state:Connections_state.empty
    in
    let%sub computation_as_html =
      let%arr computation_as_html = computation_as_html in
      let padding =
        Vdom.Node.div
          ~attrs:[ Vdom.Attr.style Css_gen.(height (`Px 12) @> width (`Px 12)) ]
          []
      in
      hbox [ padding; vbox [ padding; computation_as_html; padding ]; padding ]
    in
    let%sub edges =
      let%arr connections_state = connections_state in
      Connections_state.to_edges connections_state
    in
    let%sub edges = Bonsai.read (Bonsai.Value.cutoff edges ~equal:Edge.Set.equal) in
    let%sub edge_map =
      let%arr edges = edges in
      Set.to_map edges ~f:(Fn.const ())
    in
    let%sub edge_map =
      Bonsai.assoc
        (module Edge)
        edge_map
        ~f:(fun key _ ->
          let%sub from_to =
            let%arr positions = positions
            and { from; to_ } = key in
            let%bind.Option from_position = Map.find positions from in
            let%map.Option to_position = Map.find positions to_ in
            from_position, to_position
          in
          match%sub from_to with
          | None -> Bonsai.const None
          | Some (from_end, to_end) ->
            let%sub html = edge_to_svg ~edge:key ~from:from_end ~to_:to_end in
            let%arr html = html in
            Some html)
    in
    let%sub svgs =
      let%arr edge_map = edge_map in
      Map.data edge_map |> List.filter_opt
    in
    let%arr computation_as_html = computation_as_html
    and trackers = trackers
    and svgs = svgs
    and curr_id = curr_id in
    let tracker, curr_id = Trackers.track_resizing trackers ~curr_id in
    ( Node.div
        ~attrs:[ Style.map; Style.testcase; tracker ]
        [ Virtual_dom_svg.Node.svg svgs; computation_as_html ]
    , curr_id )
  ;;

  type 'a t =
    { nodes : 'a Name.Map.t
    ; edges : Edge.Set.t
    }
  [@@deriving sexp_of, equal, compare]

  open Bonsai_web
  open Bonsai.Let_syntax

  module Dagviz_ir = struct
    module Node = struct
      type t =
        { these : t list
        ; into : Name.t
        }
      [@@deriving sexp_of]
    end

    module Graph = struct
      type t =
        { nodes : Node.t list
        ; edges : Edge.t list
        }
      [@@deriving sexp_of]
    end
  end

  let of_dagviz_ir (t : Dagviz_ir.Graph.t) : Bindgen.Computation.t =
    let open Dagviz_ir in
    let { Graph.nodes; edges } = t in
    let rec loop nodes ~finally =
      List.fold_right nodes ~init:finally ~f:(fun { Node.these; into } body ->
        let name_depends_on =
          List.filter_map edges ~f:(fun { from; to_ } ->
            if Name.equal to_ into then Some (Bindgen.Value.named from) else None)
        in
        let bound =
          match name_depends_on with
          | [] -> Bindgen.Value.singleton ()
          | other -> Bindgen.Value.mapn other
        in
        match these with
        | [] ->
          Bindgen.Computation.sub
            ()
            ~bound:(Bindgen.Computation.return bound)
            ~as_:into
            ~for_:body
        | these ->
          Bindgen.Computation.sub
            ()
            ~bound:(loop these ~finally:(Bindgen.Computation.return bound))
            ~as_:into
            ~for_:body)
    in
    loop nodes ~finally:(Bindgen.Computation.return Bindgen.Value.fake)
  ;;

  let adjacency_list_representation ~all_nodes ~(edges : Edge.Set.t) =
    let init = Set.to_map all_nodes ~f:(Fn.const Name.Set.empty) in
    Set.fold edges ~init ~f:(fun acc { Edge.from; to_ } ->
      let curr =
        Map.update acc from ~f:(fun curr ->
          Option.value_map curr ~default:(Name.Set.singleton to_) ~f:(fun s ->
            Set.add s to_))
      in
      Map.update curr to_ ~f:(fun curr -> Option.value curr ~default:Name.Set.empty))
  ;;

  let create_nodes ~(al_graph : Name.Set.t Name.Map.t) =
    let%map.Or_error topo_sort = Edge.topological_sort ~init:Int.Map.empty al_graph in
    let linear_nodes =
      Map.data topo_sort
      |> List.concat_map ~f:Set.to_list
      |> List.map ~f:(fun id -> { Dagviz_ir.Node.into = id; these = [] })
    in
    linear_nodes
  ;;

  let to_bindgen (type a) (input : a t Bonsai.Value.t) =
    let%sub { nodes; edges } = return input in
    let%sub edges = return (Value.cutoff ~equal:Set.equal edges) in
    let%sub al_graph =
      let%arr edges = edges
      and nodes = nodes in
      adjacency_list_representation ~edges ~all_nodes:(Map.key_set nodes)
    in
    let%sub al_graph =
      Bonsai.read (Bonsai.Value.cutoff al_graph ~equal:(Name.Map.equal Name.Set.equal))
    in
    let%arr al_graph = al_graph
    and edges = edges in
    let nodes = create_nodes ~al_graph in
    let edges = Set.to_list edges in
    let%map.Or_error nodes = nodes in
    of_dagviz_ir { Dagviz_ir.Graph.nodes; edges }
  ;;

  let create
    ~(curr_id : Name.Count.t Bonsai.Value.t)
    ~(direction : [ `Left_to_right | `Top_to_bottom ])
    ~(node_to_vdom :
        Name.t Bonsai.Value.t -> 'a Value.t -> Vdom.Node.t Bonsai.Computation.t)
    ~(edge_to_svg :
        edge:Edge.t Bonsai.Value.t
        -> from:Position.t Bonsai.Value.t
        -> to_:Position.t Bonsai.Value.t
        -> Vdom.Node.t Bonsai.Computation.t)
    (dag : 'a t Bonsai_web.Value.t)
    =
    let open Bonsai.Let_syntax in
    let%sub bindgen = to_bindgen dag in
    match%sub bindgen with
    | Ok bindgen ->
      let out =
        let%sub vdom, curr_id =
          to_vdom
            ~direction
            ~curr_id
            ~node_to_vdom:(fun key ->
              let%sub data =
                let%arr key = key
                and dag = dag in
                Map.find dag.nodes key
              in
              match%sub data with
              | None -> Bonsai.const (Vdom.Node.div [])
              | Some data -> node_to_vdom key data)
            ~edge_to_svg
            bindgen
        in
        let%arr vdom = vdom
        and curr_id = curr_id in
        Or_error.return vdom, curr_id
      in
      out
    | Error error ->
      let%arr error = error
      and curr_id = curr_id in
      Error error, curr_id
  ;;
end
