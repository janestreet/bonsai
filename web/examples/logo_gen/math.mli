open! Core_kernel

module Point : sig
  type t =
    { x : float
    ; y : float
    }
  [@@deriving fields]

  val create : x:float -> y:float -> t
  val origin : t
end

module Vec : sig
  type t =
    { dx : float
    ; dy : float
    }
  [@@deriving fields]

  val create : dx:float -> dy:float -> t
  val normal : t -> t
  val scale : t -> how_much:float -> t
  val add_to_point : Point.t -> t -> Point.t
  val perp1 : t -> t
  val perp2 : t -> t
end

module Line : sig
  type t =
    { p1 : Point.t
    ; p2 : Point.t
    }
  [@@deriving fields]

  val create : p1:Point.t -> p2:Point.t -> t
  val to_vec : t -> Vec.t
  val offsets_by : t -> how_much:float -> t * t
end

module Circle : sig
  type t =
    { point : Point.t
    ; radius : float
    }
  [@@deriving fields]

  val create : point:Point.t -> radius:float -> t
  val at_center : radius:float -> t
  val point_on_surface : t -> theta:float -> Point.t
end

val line_circle_intersection : Circle.t -> Line.t -> Point.t option * Point.t option
