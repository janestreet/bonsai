open! Core
open! Import

module Node_info : sig
  type t =
    { node_type : string
    ; here : Source_code_position.t option
    }
  [@@deriving sexp, bin_io, equal]

  val of_computation : _ Computation.t -> t
  val of_value : _ Value.t -> t
end

(** There are a few models for understanding a [Computation.t].

    - As the tree of data that gets manipulated at eval-time. This tree understanding is
      quite close to the lexical structure of the program that produced the tree, so the
      tree model is useful for understanding where in your program each node came from.
    - As the directed-acyclic-graph (DAG) which exposes the sharing of computation in the
      graph. This model is useful for checking that computations are not being needlessly
      computed twice (i.e. they are being shared). Note that the computation inside an
      [Assoc] has only one copy in this model, even though the computation must run for
      every single entry in the input map.
    - As the incremental graph produced by [eval]. We do not currently generate this
      version of the graph, since it cannot be determined by static analysis of the Bonsai
      graph.

    We expose the first two of these models as the [tree] and [dag] properties,
    represented as a map from dependency(s) to dependent. In addition, we also expose a
    mapping of nodes to node metadata. *)
type t =
  { tree : Node_path.t Node_path.Map.t
  ; dag : Node_path.t list Node_path.Map.t
  ; info : Node_info.t Node_path.Map.t
  }
[@@deriving bin_io, sexp, equal]

val empty : t

(** Iterate through a computation to build up a [t] with information about the graph. The
    [Lazy] portions of the graph are traversed lazily; thus, rather than putting the
    resulting [t] in the return value of this function, we require a callback for
    receiving all the updates, including the immediately traversed ones and the ones that
    are delayed by [Lazy]. *)
val iter_graph_updates
  :  'result Computation.t
  -> on_update:(t -> unit)
  -> 'result Computation.t

val pull_source_locations_from_nearest_parent : t -> Node_info.t Node_path.Map.t

module Stable : sig
  module Node_info : sig
    module V1 : sig
      type t = Node_info.t [@@deriving sexp, bin_io, compare]
    end

    module V2 : sig
      type t =
        { node_type : string
        ; here : Source_code_position.t option
        ; id : int
        }
      [@@deriving sexp, bin_io, compare]

      val to_v1 : t -> V1.t
      val of_v1 : V1.t -> t
    end

    module V3 : sig
      type t = V1.t [@@deriving sexp, bin_io, compare]

      val to_v2 : t -> V2.t
      val of_v2 : V2.t -> t
    end
  end

  module V1 : sig
    type t =
      { tree : Node_path.Stable.V1.t Node_path.Stable.V1.Map.t
      ; dag : Node_path.Stable.V1.t list Node_path.Stable.V1.Map.t
      ; info : Node_info.V1.t Node_path.Stable.V1.Map.t
      }
    [@@deriving sexp, bin_io]
  end

  module V2 : sig
    type t =
      { tree : Node_path.t Node_path.Map.t
      ; dag : Node_path.t list Node_path.Map.t
      ; info : Node_info.V2.t Node_path.Map.t
      }
    [@@deriving bin_io, sexp]

    val to_v1 : t -> V1.t
    val of_v1 : V1.t -> t
  end

  module V3 : sig
    type nonrec t = t =
      { tree : Node_path.t Node_path.Map.t
      ; dag : Node_path.t list Node_path.Map.t
      ; info : Node_info.V3.t Node_path.Map.t
      }
    [@@deriving bin_io, sexp]

    val to_v2 : t -> V2.t
    val of_v2 : V2.t -> t
  end
end
