open! Core_kernel
open Bonsai_web
open Virtual_dom_svg
open! Math

let circle ~r1 ~r2 ~gap1 ~gap2 ~rounding =
  let outer_start, outer_end =
    let gap_theta = gap1 /. r1 in
    ( Circle.at_center ~radius:r1 |> Circle.point_on_surface ~theta:(gap_theta /. -2.0)
    , Circle.at_center ~radius:r1 |> Circle.point_on_surface ~theta:(gap_theta /. 2.0) )
  in
  let inner_start, inner_end =
    let gap_theta = gap2 /. r2 in
    ( Circle.at_center ~radius:r2 |> Circle.point_on_surface ~theta:(gap_theta /. -2.0)
    , Circle.at_center ~radius:r2 |> Circle.point_on_surface ~theta:(gap_theta /. 2.0) )
  in
  let start_line = Line.create ~p1:outer_start ~p2:inner_start in
  let end_line = Line.create ~p1:outer_end ~p2:inner_end in
  let inner_start, outer_start =
    let _, start_line = Line.offsets_by start_line ~how_much:rounding in
    match
      ( line_circle_intersection (Circle.at_center ~radius:(r2 +. rounding)) start_line
      , line_circle_intersection (Circle.at_center ~radius:(r1 -. rounding)) start_line )
    with
    | (_, Some a), (_, Some b) -> a, b
    | _ -> Point.origin, Point.origin
  in
  let inner_end, outer_end =
    let end_line, _ = Line.offsets_by end_line ~how_much:rounding in
    match
      ( line_circle_intersection (Circle.at_center ~radius:(r2 +. rounding)) end_line
      , line_circle_intersection (Circle.at_center ~radius:(r1 -. rounding)) end_line )
    with
    | (_, Some a), (_, Some b) -> a, b
    | _ -> Point.origin, Point.origin
  in
  let a =
    Line.create ~p1:inner_start ~p2:Point.origin
    |> Line.to_vec
    |> Vec.normal
    |> Vec.scale ~how_much:rounding
    |> Vec.add_to_point inner_start
  in
  let b =
    Line.create ~p1:Point.origin ~p2:outer_start
    |> Line.to_vec
    |> Vec.normal
    |> Vec.scale ~how_much:rounding
    |> Vec.add_to_point outer_start
  in
  let a' =
    Line.create ~p1:inner_end ~p2:Point.origin
    |> Line.to_vec
    |> Vec.normal
    |> Vec.scale ~how_much:rounding
    |> Vec.add_to_point inner_end
  in
  let b' =
    Line.create ~p1:Point.origin ~p2:outer_end
    |> Line.to_vec
    |> Vec.normal
    |> Vec.scale ~how_much:rounding
    |> Vec.add_to_point outer_end
  in
  let c, d =
    Line.create ~p1:outer_start ~p2:inner_start
    |> Line.to_vec
    |> Vec.normal
    |> Vec.perp1
    |> Vec.scale ~how_much:rounding
    |> fun v -> Vec.add_to_point inner_start v, Vec.add_to_point outer_start v
  in
  let c', d' =
    Line.create ~p1:outer_end ~p2:inner_end
    |> Line.to_vec
    |> Vec.normal
    |> Vec.perp2
    |> Vec.scale ~how_much:rounding
    |> fun v -> Vec.add_to_point inner_end v, Vec.add_to_point outer_end v
  in
  let path =
    [ Attr.Move_to_abs { x = a.x; y = a.y }
    ; Attr.Arc_to_abs
        { x = c.x
        ; y = c.y
        ; rx = rounding
        ; ry = rounding
        ; x_axis_rotation = 0.0
        ; large_arc = false
        ; sweep = false
        }
    ; Attr.Line_to_abs { x = d.x; y = d.y }
    ; Attr.Arc_to_abs
        { x = b.x
        ; y = b.y
        ; rx = rounding
        ; ry = rounding
        ; x_axis_rotation = 0.0
        ; large_arc = false
        ; sweep = false
        }
    ; Attr.Arc_to_abs
        { x = b'.x
        ; y = b'.y
        ; rx = r1
        ; ry = r1
        ; x_axis_rotation = 0.0
        ; large_arc = true
        ; sweep = false
        }
    ; Attr.Arc_to_abs
        { x = d'.x
        ; y = d'.y
        ; rx = rounding
        ; ry = rounding
        ; x_axis_rotation = 0.0
        ; large_arc = false
        ; sweep = false
        }
    ; Attr.Line_to_abs { x = c'.x; y = c'.y }
    ; Attr.Arc_to_abs
        { x = a'.x
        ; y = a'.y
        ; rx = rounding
        ; ry = rounding
        ; x_axis_rotation = 0.0
        ; large_arc = false
        ; sweep = false
        }
    ; Attr.Arc_to_abs
        { x = a.x
        ; y = a.y
        ; rx = r2
        ; ry = r2
        ; x_axis_rotation = 0.0
        ; large_arc = true
        ; sweep = true
        }
    ]
  in
  ( Node.g
      []
      [ Node.path [ Attr.fill (`Hex "#FFFF00AA"); Attr.d path ] []
      ; Node.circle
          [ Vdom.Attr.class_ "c_"
          ; Attr.fill (`Hex "#FF00FFAA")
          ; Attr.cx c'.x
          ; Attr.cy c'.y
          ; Attr.r 3.0
          ]
          []
      ; Node.circle
          [ Vdom.Attr.class_ "d_"
          ; Attr.fill (`Hex "#FF00FFAA")
          ; Attr.cx d'.x
          ; Attr.cy d'.y
          ; Attr.r 3.0
          ]
          []
      ; Node.circle
          [ Vdom.Attr.class_ "c"
          ; Attr.fill (`Hex "#FF00FFAA")
          ; Attr.cx c.x
          ; Attr.cy c.y
          ; Attr.r 3.0
          ]
          []
      ; Node.circle
          [ Vdom.Attr.class_ "d"
          ; Attr.fill (`Hex "#FF00FFAA")
          ; Attr.cx d.x
          ; Attr.cy d.y
          ; Attr.r 3.0
          ]
          []
      ; Node.circle
          [ Vdom.Attr.class_ "a"
          ; Attr.fill (`Hex "#FF00FFAA")
          ; Attr.cx a.x
          ; Attr.cy a.y
          ; Attr.r 3.0
          ]
          []
      ; Node.circle
          [ Vdom.Attr.class_ "b"
          ; Attr.fill (`Hex "#FF00FFAA")
          ; Attr.cx b.x
          ; Attr.cy b.y
          ; Attr.r 3.0
          ]
          []
      ; Node.circle
          [ Vdom.Attr.class_ "a_"
          ; Attr.fill (`Hex "#FF00FFAA")
          ; Attr.cx a'.x
          ; Attr.cy a'.y
          ; Attr.r 3.0
          ]
          []
      ; Node.circle
          [ Vdom.Attr.class_ "b_"
          ; Attr.fill (`Hex "#FF00FFAA")
          ; Attr.cx b'.x
          ; Attr.cy b'.y
          ; Attr.r 3.0
          ]
          []
      ; Node.circle
          [ Attr.fill (`Hex "#FF0000AA")
          ; Attr.cx inner_start.x
          ; Attr.cy inner_start.y
          ; Attr.r rounding
          ]
          []
      ; Node.circle
          [ Attr.fill (`Hex "#FF0000AA")
          ; Attr.cx outer_start.x
          ; Attr.cy outer_start.y
          ; Attr.r rounding
          ]
          []
      ; Node.circle
          [ Attr.fill (`Hex "#FF0000AA")
          ; Attr.cx outer_end.x
          ; Attr.cy outer_end.y
          ; Attr.r rounding
          ]
          []
      ; Node.circle
          [ Attr.fill (`Hex "#FF0000AA")
          ; Attr.cx inner_end.x
          ; Attr.cy inner_end.y
          ; Attr.r rounding
          ]
          []
      ; Node.g [] [ Node.path [ Attr.fill (`Hex "#FFFF00AA"); Attr.d path ] [] ]
      ]
  , path )
;;
