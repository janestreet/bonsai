open! Core
open! Import

(** This is the default legend that [With_bonsai.create] will use if you don't pass
    [custom_legend].

    This is nicer than the default dygraph legend in that it's in a fixed location (to the
    right of the graph), allows toggling visibility, and underlines/bolds the highlighted
    series.  The look of this legend was largely copied from dwu's htmlplot dygraph
    legend. *)

module Model : sig
  module Series : sig
    type t =
      { label          : string
      ; value          : Raw_html.t option
      ; dash           : Raw_html.t option
      ; color          : string option
      ; is_visible     : bool
      ; is_highlighted : bool
      }
    [@@deriving equal, fields, sexp]
  end

  type t =
    { x_label     : string
    ; x_value     : Raw_html.t option
    ; series      : Series.t list
    ; past_series : Series.t Map.M(String).t
    (** [past_series] remembers all the series (by series label) that we've ever seen.
        This means that if someone makes a change to a particular series (e.g. toggles
        visibility), moves to a graph without that series, and then moves back to the
        original graph, the information will not be lost.

        This may sound like a memory leak, and it kind of is, but the hope is that the
        total number of unique series labels that one sees over the lifetime of a graph is
        very small.  *)
    }
  [@@deriving equal, sexp]
end

module Action : sig
  type t =
    | From_graph        of Legend_data.t
    | Toggle_visibility of string
    | Select_none
    | Select_all
  [@@deriving equal, sexp]
end

val create
  :  x_label:string Bonsai.Value.t
  -> per_series_info:Per_series_info.t list Bonsai.Value.t
  -> (Model.t * Vdom.Node.t * (Action.t -> unit Ui_effect.t)) Bonsai.Computation.t
