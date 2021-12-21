open Core
open Js_of_ocaml
open Bonsai_web

let side_lengths = Array.init 12 ~f:(fun i -> float i)

let data : Dygraph.Data.t =
  Array.map side_lengths ~f:(fun side_length ->
    let radius = side_length /. 2. in
    [| side_length; Float.pi *. (radius **. 2.); side_length ** 2. |])
  |> Dygraph.Data.create
;;

let draw_square
      ~(context : Dom_html.canvasRenderingContext2D Js.t)
      ~stroke_or_fill
      ~cx
      ~cy
      ~side_length
  =
  let top_left_x_coord = cx -. (side_length /. 2.) in
  let top_left_y_coord = cy -. (side_length /. 2.) in
  context##beginPath;
  match stroke_or_fill with
  | `Stroke ->
    context##strokeRect top_left_x_coord top_left_y_coord side_length side_length
  | `Fill -> context##fillRect top_left_x_coord top_left_y_coord side_length side_length
;;

let draw_circle ~(context : Dom_html.canvasRenderingContext2D Js.t) ~cx ~cy ~radius =
  context##beginPath;
  context##arc cx cy radius 0. (2. *. Float.pi) (Js.bool false);
  context##stroke
;;

let options =
  let circle_series_options =
    Dygraph.Options.Series_options.create
      ~drawHighlightPointCallback:
        (fun ~graph:_ ~seriesName:_ ~context ~cx ~cy ~color:_ ~pointSize:_ ~idx ->
           let radius = side_lengths.(idx) /. 2. in
           draw_circle ~context ~cx ~cy ~radius;
           context##fill)
      ~drawPointCallback:
        (fun ~graph:_ ~seriesName:_ ~context ~cx ~cy ~color:_ ~pointSize:_ ~idx ->
           let radius = side_lengths.(idx) /. 2. in
           draw_circle ~context ~cx ~cy ~radius)
      ()
  in
  let square_series_options =
    Dygraph.Options.Series_options.create
      ~drawHighlightPointCallback:
        (fun ~graph:_ ~seriesName:_ ~context ~cx ~cy ~color:_ ~pointSize:_ ~idx ->
           let side_length = side_lengths.(idx) in
           draw_square ~context ~stroke_or_fill:`Fill ~cx ~cy ~side_length)
      ~drawPointCallback:
        (fun ~graph:_ ~seriesName:_ ~context ~cx ~cy ~color:_ ~pointSize:_ ~idx ->
           let side_length = side_lengths.(idx) in
           draw_square ~context ~stroke_or_fill:`Stroke ~cx ~cy ~side_length)
      ()
  in
  let series =
    Dygraph.Options.Series.create
      [ "circles", circle_series_options; "squares", square_series_options ]
  in
  Dygraph.Options.create
    ()
    ~drawPoints:true
    ~strokeWidth:0.
    ~series
    ~title:"Custom Drawn Points Example"
;;

let app =
  Dygraph.With_bonsai.create
    ()
    ~key:("custom-drawn-points-graph" |> Value.return)
    ~x_label:("diameter/side length" |> Value.return)
    ~per_series_info:
      ([ "circles"; "squares" ]
       |> Dygraph.Per_series_info.create_all_visible
       |> Value.return)
    ~options:(options |> Value.return)
    ~data:(data |> Value.return)
;;
