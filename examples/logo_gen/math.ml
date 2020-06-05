open! Core_kernel

module Point = struct
  type t =
    { x : float
    ; y : float
    }
  [@@deriving fields]

  let create = Fields.create
  let origin = { x = 0.0; y = 0.0 }
end

module Vec = struct
  type t =
    { dx : float
    ; dy : float
    }
  [@@deriving fields]

  let create = Fields.create
  let magnitude { dx; dy } = Float.sqrt ((dx *. dx) +. (dy *. dy))

  let normal { dx; dy } =
    let len = magnitude { dx; dy } in
    { dx = dx /. len; dy = dy /. len }
  ;;

  let perp1 { dx; dy } = { dx = dy; dy = -.dx }
  let perp2 { dx; dy } = { dx = -.dy; dy = dx }
  let add_to_point { Point.x; y } { dx; dy } = { Point.x = x +. dx; y = y +. dy }
  let scale { dx; dy } ~how_much = { dx = dx *. how_much; dy = dy *. how_much }
end

module Line = struct
  type t =
    { p1 : Point.t
    ; p2 : Point.t
    }
  [@@deriving fields]

  let create = Fields.create

  let to_vec { p1 = { x = x1; y = y1 }; p2 = { x = x2; y = y2 } } =
    let dx = x2 -. x1 in
    let dy = y2 -. y1 in
    Vec.create ~dx ~dy
  ;;

  let offsets_by ({ p1; p2 } as line) ~how_much =
    let perp1 = line |> to_vec |> Vec.perp1 in
    let perp2 = line |> to_vec |> Vec.perp2 in
    let perp1 = perp1 |> Vec.normal |> Vec.scale ~how_much in
    let perp2 = perp2 |> Vec.normal |> Vec.scale ~how_much in
    let line1 = { p1 = Vec.add_to_point p1 perp1; p2 = Vec.add_to_point p2 perp1 } in
    let line2 = { p1 = Vec.add_to_point p1 perp2; p2 = Vec.add_to_point p2 perp2 } in
    line1, line2
  ;;
end

module Circle = struct
  type t =
    { point : Point.t
    ; radius : float
    }
  [@@deriving fields]

  let create = Fields.create
  let at_center ~radius = { point = { x = 0.0; y = 0.0 }; radius }

  let point_on_surface { point = { x; y }; radius } ~theta =
    { Point.x = x +. (radius *. Float.cos theta)
    ; Point.y = y +. (radius *. Float.sin theta)
    }
  ;;
end

(* Calculates the intersections between a circle 
 * and the line defined by two points on the line. *)
let line_circle_intersection
      { Circle.point = { x = cx; y = cy }; radius }
      { Line.p1 = point1; p2 = point2 }
  =
  let open Float in
  let dx = point2.x - point1.x in
  let dy = point2.y - point1.y in
  let a = (dx * dx) + (dy * dy) in
  let b = 2.0 * ((dx * (point1.x - cx)) + (dy * (point1.y - cy))) in
  let c =
    ((point1.x - cx) * (point1.x - cx))
    + ((point1.y - cy) * (point1.y - cy))
    - (radius * radius)
  in
  let det = (b * b) - (4.0 * a * c) in
  if a <= 0.0000001 || det < 0.0
  then None, None
  else if Float.equal det 0.0
  then (
    let t = -b / (2.0 * a) in
    let intersection1 = Point.{ x = point1.x + (t * dx); y = point1.y + (t * dy) } in
    Some intersection1, None)
  else (
    let t = (-b + sqrt det) / (2.0 * a) in
    let intersection1 = Point.{ x = point1.x + (t * dx); y = point1.y + (t * dy) } in
    let t = (-b - sqrt det) / (2.0 * a) in
    let intersection2 = Point.{ x = point1.x + (t * dx); y = point1.y + (t * dy) } in
    Some intersection1, Some intersection2)
;;
