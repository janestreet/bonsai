open! Core
open  Import

(** The recommended way to create dygraphs (with bonsai). *)

module Legend_model : sig
  type t = { visibility : bool list } [@@deriving equal, fields]
end

(** This is the legend that [create] will when nothing is passed to [custom_legend].

    Even if you intend to pass this into [create], it may be useful to create this
    yourself to gain access to the Legend_model.t.
*)
val create_default_legend
  :  x_label:string Bonsai.Value.t
  -> per_series_info:Per_series_info.t list Bonsai.Value.t
  -> (Legend_model.t * Vdom.Node.t * (Legend_data.t -> unit Ui_effect.t))
       Bonsai.Computation.t

type t =
  { graph_view   : Vdom.Node.t
  ; modify_graph : (Graph.t -> unit) -> unit Effect.t
  }
[@@deriving fields]

val create
  :  key:string Bonsai.Value.t
  (** [key] is a virtualdom concept that allows it to identify which items in a list have
      changed.  For more information, see
      https://reactjs.org/docs/lists-and-keys.html#keys.

      Every graph in your document should have a unique [key].

      For a given graph, [key] should be constant.
  *)
  -> x_label:string Bonsai.Value.t
  -> per_series_info:Per_series_info.t list Bonsai.Value.t
  -> ?custom_legend:
       (Legend_model.t * Vdom.Node.t * (Legend_data.t -> unit Ui_effect.t)) Bonsai.Value.t
  (** [custom_legend] defaults to Default_legend.  If you don't want that legend, you're
      free to pass in your own bonsai computation. *)
  -> ?options:Options.t Bonsai.Value.t
  -> ?with_graph:(Graph.t -> unit)
  (** This hook may be useful if you want to, for example, bind the graph to some global
      variable on the window.  That way you can poke at the graph in the console. *)
  -> ?on_zoom:(is_zoomed:bool -> unit Vdom.Effect.t) Bonsai.Value.t
  -> ?extra_attr:Vdom.Attr.t Bonsai.Value.t
  -> data:Data.t Bonsai.Value.t
  -> unit
  -> t Bonsai.Computation.t
