open Core
open Js_of_ocaml
open Bonsai_web

(** x, x^2, x^3 *)
let data : Dygraph.Data.t =
  Array.init 10 ~f:(fun i ->
    let x = float i in
    [| x; x ** 2.; x ** 3. |])
  |> Dygraph.Data.create
;;

let x_label = "x"

let options =
  let cubed_series_options =
    Dygraph.Options.Series_options.create () ~axis:`y2 ~color:(`Name "purple")
  in
  let squared_series_otions =
    Dygraph.Options.Series_options.create () ~axis:`y1 ~color:(`Hex "#70ba70")
  in
  let axes =
    let y2_axis_options = Dygraph.Options.Axis_options.create () ~independentTicks:true in
    Dygraph.Options.Axes.create () ~y2:y2_axis_options
  in
  let series =
    Dygraph.Options.Series.create
      [ "x^2", squared_series_otions; "x^3", cubed_series_options ]
  in
  Dygraph.Options.create
    ()
    ~drawPoints:true
    ~strokeWidth:0.
    ~series
    ~title:"Simple example"
    ~xlabel:x_label
    ~ylabel:"x^2"
    ~y2label:"x^3"
    ~axes
;;

let app =
  Dygraph.With_bonsai.create
    ()
    ~key:("graph" |> Value.return)
    ~x_label:("x" |> Value.return)
    ~per_series_info:
      ([ "x^2"; "x^3" ] |> Dygraph.Per_series_info.create_all_visible |> Value.return)
    ~options:(options |> Value.return)
    ~data:(data |> Value.return)
    ~with_graph:(fun graph -> Js.Unsafe.set Dom_html.window "g" graph)
;;
