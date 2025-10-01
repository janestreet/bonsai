open! Core

module Node : sig
  type t =
    { here : Source_code_position.t
    ; kind : string
    }
  [@@deriving sexp_of, compare]

  include Comparable.S with type t := t
end

module Event : sig
  type t = Node_created of { node : Node.t } [@@unboxed] [@@deriving sexp_of]

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving sexp]
    end

    type t [@@deriving sexp, quickcheck]

    val of_latest : V1.t -> t
    val to_latest : t -> V1.t
  end
end

module State : sig
  type t = int Node.Map.t [@@deriving sexp_of]

  val empty : t
  val apply_event : t -> Event.t -> t
end
