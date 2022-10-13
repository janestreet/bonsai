open! Core
open Bonsai_web
module Vdom = Virtual_dom.Vdom
module Position := Bonsai_web_ui_element_size_hooks.Position_tracker.Position

(** [Make] will create a module for visualizing a Directed Acyclic Graph (DAG) as a
    Bonsai Vdom.Node.t Computation.t whose vertices are identified by [Name]*)

module Make (Name : Types.Name) : sig
  module Edge : sig
    (** Edge that starts at [from] and ends at [to_]. *)
    type t =
      { from : Name.t
      ; to_ : Name.t
      }
    [@@deriving sexp, equal, compare]

    include Comparable.S with type t := t
  end

  type 'a t =
    { nodes : 'a Name.Map.t
    ; edges : Edge.Set.t
    }
  [@@deriving sexp, equal, compare]

  (* Creates a DAG visualization. Since new ids could be minted/created while
     introducing redirect nodes, in order to avoid side-effects a state-monad-like pattern
     where every time that a new id is minted, the [curr_id] our count is incremented. If this
     is the first place where new ids can be minted, it is safe to pass in Name.Count.zero. If
     there are multiple DAGs, please give the first one a curr id of zero, and subsequent ones
     and the [curr_id] returned by the previous call. e.g.:

     {[
       let%sub dag1, curr_id = create ~curr_id:Name.Count.zero ... in
       let%sub dag2, curr_id = create ~curr_id ... in
       ...
     ]}

     The graph will fail if the given graph is not a DAG (i.e. it has a cycle.)
  *)
  val create
    :  curr_id:Name.Count.t Value.t
    -> direction:[ `Left_to_right | `Top_to_bottom ]
    -> node_to_vdom:(Name.t Value.t -> 'a Value.t -> Vdom.Node.t Computation.t)
    -> edge_to_svg:
         (edge:Edge.t Value.t
          -> from:Position.t Value.t
          -> to_:Position.t Value.t
          -> Vdom.Node.t Computation.t)
    -> 'a t Value.t
    -> (Vdom.Node.t Or_error.t * Name.Count.t) Computation.t
end
