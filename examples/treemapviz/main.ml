open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax

let percent_range_generator = Base_quickcheck.Generator.float_inclusive (-2.8) 2.8

let generate_range ~from ~to_ =
  let generator = Base_quickcheck.Generator.float_inclusive from to_ in
  let random = Splittable_random.create Random.State.default in
  Quickcheck.Generator.generate generator ~size:1 ~random
;;

module Tree = struct
  type t =
    { name : (string[@quickcheck.generator Base_quickcheck.Generator.string_non_empty])
    ; weight :
        (float[@quickcheck.generator Base_quickcheck.Generator.float_inclusive 1.0 200.0])
    ; color :
        ((* NOTE: [color] color corresponds to the percent move. Roughly 3.0 means green -3
            means red and everything in between is a gradient.*)
         float
        [@quickcheck.generator percent_range_generator])
    ; children : t list
    }
  [@@deriving quickcheck]

  let weight t = t.weight
  let children t = t.children
end

let m name children =
  { Tree.name
  ; color = generate_range ~from:(-3.0) ~to_:3.0
  ; weight = generate_range ~from:3.0 ~to_:20.0
  ; children
  }
;;

let life_elements =
  Nonempty_list.
    [ m
        "plant"
        [ m "fruit" [ m "tomato" []; m "grapes" []; m "banana" []; m "apple" [] ]
        ; m "vegetable" [ m "spinach" []; m "lettuce" []; m "potato" [] ]
        ; m
            "tree"
            [ m "oak" []
            ; m "acacia" []
            ; m "spruce" []
            ; m "dark oak" []
            ; m "jungle" []
            ; m "birch" []
            ]
        ; m "flytrap" []
        ]
    ; m
        "animal"
        [ m
            "rodent"
            [ m "capybara" []
            ; m "agouti" []
            ; m "mouse" []
            ; m "fancy mouse" []
            ; m "groundhog" []
            ; m "prarie dog" []
            ; m "mara" []
            ]
        ; m
            "felidae"
            [ m "tiger" []; m "cat" []; m "lion" []; m "cheetah" []; m "wildcat" [] ]
        ; m
            "bird"
            [ m "owl" []
            ; m "parrot" []
            ; m "chicken" []
            ; m "penguin" []
            ; m "hummingbird" []
            ]
        ; m "tardigrade" []
        ]
    ; m
        "fungus"
        [ m
            "mushrooms"
            [ m "button" []
            ; m "portobello" []
            ; m "oyster" []
            ; m "snow puff" []
            ; m "chanterelle" []
            ]
        ]
    ]
;;

let stress_elements =
  let random = Splittable_random.create Random.State.default in
  Nonempty_list.init 100 ~f:(fun _ ->
    Quickcheck.Generator.generate Tree.quickcheck_generator ~size:100 ~random)
;;

module Style =
[%css
stylesheet
  {|
* {
  box-sizing: border-box;
}

html, body {
  margin: 0px;
  font-family: 'Open Sans', sans-serif;
  height: 100vh;
}

.top {
  height: 100vh;
  display: grid;
  grid-template-columns: 100%;
  grid-template-rows: 2rem calc(100vh - 2rem);
}

.full-width {
  height: 100%;
  width: 100%;
  overflow: hidden;
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  color: white;
  border: 2px solid black;
}

.full-width:hover {
  border: 2px solid #FACC15;
}

.red-gradient {
  background: linear-gradient(90deg, rgba(79,90,91,1) 0%, rgba(226,62,62,1) 100%);
  background-size: 1000% 100%;
}

.green-gradient {
  background: linear-gradient(90deg, rgba(79,90,91,1) 0%, rgba(16,187,109,1) 100%);
  background-size: 100000% 100%;
}

.tiny-margin {
  padding: 0.2rem;
}

.box-title {
  background-color: black;
  width: 100%;
  display: flex;
  justify-content: center;
  align-items: center;
  font-weight: 0.2rem;
}

.unset_text {
  color: black;
}

.redacted {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  background-image: linear-gradient( 135deg, #FFAA85 10%, #B3315F 100%);
  color: white;
  height: 100%;
  width: 100%;
  border: 2px solid black;
  overflow: hidden;
}


.redacted:hover {
  border: 2px solid #FACC15;
}

.stress-button {
  background-color : black;
  color: white;
  font-weight: 3.0;
  width: 100%;
  height: 100%;
  display: flex;
  flex-direction: row;
  cursor: pointer;
  align-items: center;
  border: 1px solid white;
  padding-left: 1rem;
  transition: 0.2s;
}

.stress-button:hover, .stress-button:focus {
  border-color: #FACC15;
  color: #FACC15;
}|}]

let create_color percent =
  let base =
    if Float.O.(percent > 0.) then Style.green_gradient else Style.red_gradient
  in
  let percent = Float.min (Float.abs percent) 3.0 /. 3.0 *. 100. in
  let percent = Float.min (Float.max 0.0 percent) 100.0 in
  let percent = if Float.O.(percent < 0.01) then 0.0 else percent in
  let offset =
    let value = [%string "%{Float.to_string percent}% 50%"] in
    Vdom.Attr.style (Css_gen.create ~field:"background-position" ~value)
  in
  Vdom.Attr.many [ base; offset ]
;;

module Dimensions = struct
  type t =
    { width : float
    ; height : float
    }
  [@@deriving sexp, equal]
end

let create_treemap ~elements graph =
  let dimensions, set_dimensions =
    Bonsai.state_opt
      graph
      ~sexp_of_model:[%sexp_of: Dimensions.t]
      ~equal:[%equal: Dimensions.t]
  in
  let tracker =
    let%arr set_dimensions = set_dimensions in
    Bonsai_web_ui_element_size_hooks.Size_tracker.on_change (fun ~width ~height ->
      set_dimensions (Some { Dimensions.height; width }))
  in
  let treemap =
    match%sub dimensions with
    | None -> Bonsai.return Vdom.Node.none
    | Some dimensions ->
      let dimensions =
        let%arr dimensions = dimensions in
        { Dimensions.height = Float.round_nearest_half_to_even dimensions.height
        ; width = Float.round_up dimensions.width
        }
      in
      let dimensions = Bonsai.cutoff dimensions ~equal:[%equal: Dimensions.t] in
      let treemap =
        let%arr { width; height } = dimensions in
        Bonsai_experimental_treemapviz.create
          ~width
          ~height
          ~min_percentage:(Percent.of_mult 0.95)
          ~weight:Tree.weight
          ~children:Tree.children
          elements
      in
      let%arr treemap = treemap in
      Bonsai_experimental_treemapviz.render treemap ~f:(function
        | No_more_space elements ->
          let length = Nonempty_list.length elements in
          Vdom.Node.div ~attrs:[ Style.redacted ] [ View.textf "+%d" length ]
        | Leaf { element = tree; children } ->
          (match children with
           | None ->
             View.vbox
               ~attrs:
                 [ Style.tiny_margin; create_color tree.Tree.color; Style.full_width ]
               [ View.text tree.name
               ; (let sign = if Float.O.(tree.color > 0.0) then "+" else "" in
                  View.text
                    [%string "%{sign}%{Float.to_string_hum ~decimals:2 tree.color}%"])
               ]
           | Some children ->
             View.vbox
               ~attrs:[ Style.full_width ]
               [ View.hbox ~attrs:[ Style.box_title ] [ View.text tree.name ]; children ]))
  in
  let%arr tracker = tracker
  and treemap = treemap in
  Vdom.Node.div ~attrs:[ tracker ] [ treemap ]
;;

let component graph =
  let stress_state, toggle_stress = Bonsai.toggle ~default_model:false graph in
  let content =
    match%sub stress_state with
    | false -> create_treemap ~elements:life_elements graph
    | true -> create_treemap ~elements:stress_elements graph
  in
  let button =
    let%arr stress_state = stress_state
    and toggle_stress = toggle_stress in
    let text =
      match stress_state with
      | false -> "Show stress test (10k nodes (100 top-nodes with ~100 nodes each))"
      | true -> "Show capybara treemap of life"
    in
    Vdom.Node.div
      ~attrs:
        [ Style.stress_button
        ; Vdom.Attr.on_click (fun _ -> toggle_stress)
        ; Vdom.Attr.role "button"
        ; Vdom.Attr.tabindex 0
        ]
      [ Feather_icon.svg Chevrons_right ~size:(`Rem 1.5) ~stroke:(`Hex "#FACC15")
      ; View.text text
      ]
  in
  let%arr button = button
  and content = content in
  Vdom.Node.div ~attrs:[ Style.top ] [ button; content ]
;;

let () = Bonsai_web.Start.start component
