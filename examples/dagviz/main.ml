open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Position = Bonsai_web_ui_element_size_hooks.Position_tracker.Position

module Progress = struct
  type t =
    { complete : int
    ; remaining : int
    }
  [@@deriving sexp, compare, quickcheck]

  let to_message { complete; remaining } =
    [%string "%{complete#Int}/%{(complete+remaining)#Int}"]
  ;;
end

module Styles =
  [%css
    stylesheet
      {|
.paper {
  box-shadow: 0 0 8px rgba(0,0,0,0.2);
  padding: 12px;
  border-radius: 4px;
  margin: 8px;
  max-width: fit-content;
  text-align: center;
}

  .header {
     display: flex;
     direction: column;
     align-items: center;
     justify-content: center;
  }

  @keyframes linear-progress-bar-stripes{0%{background-position:0 0}to{background-position:30px 0}}

  .spinning_progress_bar {
    animation-duration: 0.3s;
    animation-timing-function: linear;
    animation-delay: 0s;
    animation-iteration-count: infinite;
    animation-direction: reverse;
    animation-fill-mode: none;
    animation-play-state: running;
    animation-name: linear-progress-bar-stripes;
  }


  .progress_bar_meter {
    background: linear-gradient(-45deg,hsla(0,0%,100%,.2) 25%,transparent 0,transparent 50%,hsla(0,0%,100%,.2) 0,hsla(0,0%,100%,.2) 75%,transparent 0);
    background-size: 30px 30px;
    height: 6px;
    position: absolute;
    transition: width .2s cubic-bezier(.4,1,.75,.9);
  }

  .progress_bar {
    background: rgba(95,107,124,.2);
    display: block;
    height: 6px;
    overflow: hidden;
    position: relative;
    width: 100%;
  }

  .node {
    border-style: solid;
    border-color: #D3D3D3;
    border-width: 1px;
    display: flex;
    flex-direction: column;
    align-items: center;
    width: fit-content;
    transition: width 2s cubic-bezier(.4, 1, .75, .9);
  }

  .node_contents {
    display: flex;
    flex-direction: column;
    align-items: center;
    width: fit-content;
    padding: 4px;
  }

  .status_message {
    color: #636363;
  }

  .person {
    transition: all 0.5s;
  }

  .cursor {
      cursor: pointer;
  }
                          |}]

module Status = struct
  type t =
    | Starting
    | Running of Progress.t
    | Failed of Progress.t
    | Done of { total_atoms : int }
  [@@deriving sexp, compare, quickcheck]

  let to_color = function
    | Starting -> Tailwind_colors.yellow500
    | Running _ -> Tailwind_colors.blue500
    | Failed _ -> Tailwind_colors.red500
    | Done _ -> Tailwind_colors.green500
  ;;

  let to_percent_complete = function
    | Done _ -> Percent.one_hundred_percent
    | Starting -> Percent.one_hundred_percent
    | Running p | Failed p ->
      let total = p.remaining + p.complete in
      Percent.of_percentage (100. *. (Int.to_float p.complete /. Int.to_float total))
  ;;

  let is_not_finished = function
    | Starting | Running _ -> true
    | Failed _ | Done _ -> false
  ;;

  let to_status_message = function
    | Starting -> "Starting"
    | Failed p -> [%string "Failed (%{Progress.to_message p} atoms)"]
    | Running p -> [%string "Running (%{Progress.to_message p} atoms)"]
    | Done { total_atoms } -> [%string "Done (%{total_atoms#Int} atoms)"]
  ;;

  let to_vdom t =
    let color = to_color t in
    let width = to_percent_complete t in
    let maybe_spin =
      match is_not_finished t with
      | true -> [ Styles.spinning_progress_bar ]
      | false -> []
    in
    Vdom.Node.div
      ~attr:Styles.progress_bar
      [ Vdom.Node.div
          ~attr:
            (Vdom.Attr.many
               [ Vdom.Attr.many ([ Styles.progress_bar_meter ] @ maybe_spin)
               ; Vdom.Attr.style
                   (Css_gen.combine
                      (Css_gen.background_color color)
                      (Css_gen.width (`Percent width)))
               ])
          []
      ]
  ;;
end

module Id = Bonsai_experimental_dagviz.Default_id

module Node_data = struct
  module Person = struct
    type t = unit [@@deriving sexp, compare, equal, quickcheck]

    let to_vdom _ =
      Vdom.Node.div
        ~attr:(Vdom.Attr.many [ Styles.node; Styles.person ])
        [ Vdom.Node.text "I am a human BEEP BOOP!" ]
    ;;
  end

  type t =
    { status : Status.t
    ; name : string
    ; people : Person.t list
    }
  [@@deriving sexp, compare, quickcheck, fields]

  let to_vdom (id : Id.t Value.t) (t : t Value.t) : Vdom.Node.t Computation.t =
    let%sub collapsed, collapse = Bonsai.toggle ~default_model:true in
    let%arr collapsed = collapsed
    and id = id
    and t = t
    and collapse = collapse in
    let status = Status.to_vdom t.status in
    let status_message =
      Vdom.Node.div
        ~attr:Styles.status_message
        [ Vdom.Node.text @@ Status.to_status_message t.status ]
    in
    let name =
      Vdom.Node.div
        ~attr:(Vdom.Attr.style (Css_gen.flex_container ~direction:`Row ()))
        [ Vdom.Node.strong [ Vdom.Node.text t.name ] ]
    in
    let people =
      match collapsed with
      | true ->
        let number_of_people = List.length t.people in
        Vdom.Node.div [ Vdom.Node.text [%string "%{number_of_people#Int} people"] ]
      | false ->
        Vdom.Node.div
          ~attr:(Vdom.Attr.style (Css_gen.flex_container ~direction:`Row ()))
          (List.map t.people ~f:Person.to_vdom)
    in
    Vdom.Node.div
      ~attr:Styles.node
      [ status
      ; Vdom.Node.div
          ~attr:Styles.node_contents
          [ Vdom.Node.div
              ~attr:(Vdom.Attr.style (Css_gen.flex_container ~direction:`Row ()))
              [ name
              ; Feather_icon.svg
                  ~extra_attrs:[ Styles.cursor; Vdom.Attr.on_click @@ Fn.const collapse ]
                  (if collapsed
                   then Feather_icon.Chevron_down
                   else Feather_icon.Chevron_up)
              ]
          ; status_message
          ]
      ; people
      ; Vdom.Node.sexp_for_debugging [%message (id : Id.t)]
      ]
  ;;
end

module To_vdom = Bonsai_experimental_dagviz.To_vdom.Make (Id)

let people n = List.init n ~f:(Fn.const ())
let capybara_finder_id = Id.create ()

let capybara_finder =
  Node_data.Fields.create
    ~status:(Done { total_atoms = 126 })
    ~name:"Capybara finder"
    ~people:(people 2)
;;

let capybara_feeder_id = Id.create ()

let capybara_feeder =
  Node_data.Fields.create ~status:Starting ~name:"Capybara feeder" ~people:(people 3)
;;

let capybara_cleaner_id = Id.create ()

let capybara_cleaner =
  Node_data.Fields.create ~status:Starting ~name:"Capybara cleaner" ~people:(people 4)
;;

let capybara_petter_id = Id.create ()

let capybara_petter =
  Node_data.Fields.create
    ~status:(Running { complete = 50; remaining = 76 })
    ~name:"Capybara petter"
    ~people:(people 5)
;;

let capybara_herder_id = Id.create ()

let capybara_herder =
  Node_data.Fields.create
    ~status:(Running { complete = 20; remaining = 96 })
    ~name:"Capybara herder"
    ~people:[ () ]
;;

let capybara_photographer_id = Id.create ()

let capybara_photographer =
  Node_data.Fields.create
    ~status:(Running { complete = 10; remaining = 116 })
    ~name:"Capybara photographer"
    ~people:[ () ]
;;

let capybara_fan_id = Id.create ()

let capybara_fan =
  Node_data.Fields.create
    ~status:(Running { complete = 30; remaining = 116 })
    ~name:"Capybara fan"
    ~people:[ (); () ]
;;

let nodes =
  [ capybara_finder_id, capybara_finder
  ; capybara_feeder_id, capybara_feeder
  ; capybara_cleaner_id, capybara_cleaner
  ; capybara_petter_id, capybara_petter
  ; capybara_herder_id, capybara_herder
  ; capybara_photographer_id, capybara_photographer
  ; capybara_fan_id, capybara_fan
  ]
  |> Id.Map.of_alist_exn
;;

open To_vdom

let edges =
  [ { Edge.from = capybara_finder_id; to_ = capybara_feeder_id }
  ; { Edge.from = capybara_finder_id; to_ = capybara_petter_id }
  ; { Edge.from = capybara_finder_id; to_ = capybara_herder_id }
  ; { Edge.from = capybara_finder_id; to_ = capybara_photographer_id }
  ; { Edge.from = capybara_feeder_id; to_ = capybara_cleaner_id }
  ; { Edge.from = capybara_cleaner_id; to_ = capybara_herder_id }
  ; { Edge.from = capybara_petter_id; to_ = capybara_herder_id }
  ; { Edge.from = capybara_herder_id; to_ = capybara_photographer_id }
  ; { Edge.from = capybara_fan_id; to_ = capybara_photographer_id }
  ]
  |> To_vdom.Edge.Set.of_list
;;

module Point = struct
  type t =
    { x : int
    ; y : int
    }
end

module Line = struct
  type t =
    { from : Point.t
    ; to_ : Point.t
    }
end

let face_point : Position.t -> [ `Top | `Left | `Bottom | `Right ] -> Point.t =
  fun position direction ->
  let { Position.top; left; width; height } = position in
  let x =
    match direction with
    | `Top | `Bottom -> left + (width / 2)
    | `Right -> left
    | `Left -> left + width
  in
  let y =
    match direction with
    | `Right | `Left -> top + (height / 2)
    | `Top -> top + height
    | `Bottom -> top
  in
  { Point.x; y }
;;

let edge_to_svg
      ~(direction : [ `Top_down | `Left_to_right ])
      ~(edge : Edge.t Value.t)
      ~(from : Position.t Value.t)
      ~(to_ : Position.t Value.t)
  =
  let%arr edge = edge
  and from = from
  and to_ = to_ in
  let href =
    let from = edge.from |> Id.to_string in
    let to_ = edge.to_ |> Id.to_string in
    [%string "bee_path%{from}_%{to_}"]
  in
  let get_from_direction = function
    | `Top_down -> `Top
    | `Left_to_right -> `Left
  in
  let get_to_direction = function
    | `Top_down -> `Bottom
    | `Left_to_right -> `Right
  in
  let { Line.from = { x = x1; y = y1 }; to_ = { x = x2; y = y2 } } =
    let from = face_point from (get_from_direction direction) in
    let to_ = face_point to_ (get_to_direction direction) in
    { Line.from; to_ }
  in
  let midx, midy = Int.to_float ((x1 + x2) / 2), Int.to_float ((y1 + y2) / 2) in
  let attr =
    match direction with
    | `Top_down ->
      Virtual_dom_svg.Attr.d
        [ Virtual_dom_svg.Attr.Move_to_abs { x = Int.to_float x1; y = Int.to_float y1 }
        ; Virtual_dom_svg.Attr.Cubic_smooth_abs
            { x = midx; y = midy; x2 = Int.to_float x1; y2 = midy }
        ; Virtual_dom_svg.Attr.Cubic_smooth_abs
            { x = Int.to_float x2; y = Int.to_float y2; x2 = Int.to_float x2; y2 = midy }
        ]
    | `Left_to_right ->
      Virtual_dom_svg.Attr.d
        [ Virtual_dom_svg.Attr.Move_to_abs { x = Int.to_float x1; y = Int.to_float y1 }
        ; Virtual_dom_svg.Attr.Cubic_smooth_abs
            { x = midx; y = midy; x2 = midx; y2 = Int.to_float y1 }
        ; Virtual_dom_svg.Attr.Cubic_smooth_abs
            { x = Int.to_float x2; y = Int.to_float y2; x2 = midx; y2 = Int.to_float y2 }
        ]
  in
  let attr =
    Vdom.Attr.many
      [ attr
      ; Vdom.Attr.create "fill-opacity" "0.0"
      ; Virtual_dom_svg.Attr.stroke_width 3.0
      ; Virtual_dom_svg.Attr.stroke (`Hex "#939393")
      ; Vdom.Attr.id href
      ]
  in
  let bee_href = href ^ "actual_bee" in
  Virtual_dom_svg.Node.g
    [ Virtual_dom_svg.Node.path ~attr []
    ; Virtual_dom_svg.Node.text
        ~attr:(Vdom.Attr.create "rotate" "180")
        [ Virtual_dom_svg.Node.text_path
            ~attr:
              (Vdom.Attr.many
                 [ Vdom.Attr.id bee_href; Virtual_dom_svg.Attr.href ("#" ^ href) ])
            [ Vdom.Node.text "🐝" ]
        ]
    ; Vdom.Node.create_svg
        "animate"
        ~attr:
          (Vdom.Attr.many
             [ Vdom.Attr.href ("#" ^ bee_href)
             ; Vdom.Attr.create "attributeName" "startOffset"
             ; Vdom.Attr.create "from" "0%"
             ; Vdom.Attr.create "to" "100%"
             ; Vdom.Attr.create "dur" "2s"
             ; Vdom.Attr.create "repeatCount" "indefinite"
             ])
        []
    ]
;;

let component =
  let curr_id = Value.return Id.Count.zero in
  let%sub dag_data = return (Value.return { To_vdom.nodes; edges }) in
  let%sub dag, _curr_id =
    To_vdom.create
      ~curr_id
      ~direction:`Top_to_bottom
      ~node_to_vdom:Node_data.to_vdom
      ~edge_to_svg:(edge_to_svg ~direction:`Top_down)
      dag_data
  in
  let%sub dag =
    match%sub dag with
    | Ok dag -> return dag
    | Error error ->
      let%arr error = error in
      Vdom.Node.sexp_for_debugging [%message "" ~_:(error : Error.t)]
  in
  let%arr dag = dag in
  Vdom.Node.div
    ~attr:Styles.header
    [ Vdom.Node.div
        ~attr:Styles.paper
        [ Vdom.Node.strong [ Vdom.Node.text "Capybara Petting Zoo" ]; dag ]
    ]
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
