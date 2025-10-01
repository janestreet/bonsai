open! Core

module Node = struct
  module T = struct
    type t =
      { here : Source_code_position_with_quickcheck.t
      ; kind : string
      }
    [@@sexp.allow_extra_fields] [@@deriving sexp, compare, quickcheck]
  end

  include T
  include Comparable.Make (T)
end

module Event = struct
  module Stable = struct
    module V1 = struct
      type t = Node_created of { node : Node.t }
      [@@unboxed] [@@deriving sexp, quickcheck]
    end

    type t = V1 of V1.t [@@deriving sexp, quickcheck]

    let of_latest v1 = V1 v1

    let to_latest = function
      | V1 v1 -> v1
    ;;
  end

  type t = Stable.V1.t = Node_created of { node : Node.t }
  [@@unboxed] [@@deriving sexp_of]
end

module State = struct
  type t = int Node.Map.t [@@deriving sexp]

  let empty = Node.Map.empty

  let apply_event : t -> Event.t -> t =
    fun map event ->
    match event with
    | Node_created { node } ->
      Map.update map node ~f:(function
        | None -> 1
        | Some prev -> prev + 1)
  ;;
end
