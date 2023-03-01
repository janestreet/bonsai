open! Core
open! Import

(** A [Multi_factor.t] is a collection of [Single_factor.t]s (referred to as a "subwidget"
    in this module), indexed by the [Arg.Key] type. This widget provides focus tracking
    among the different subwidgets.
*)

module type Key = sig
  type t [@@deriving equal, sexp]

  include Comparable.S with type t := t

  val to_string : t -> string

  (** The human-language name of your type. E.g. if [t] represents some attributes of a
      trade, you could use something like "trade property" here. *)
  val name_singular : string

  (** As above, but in the plural. *)
  val name_plural : string
end

module type S = sig
  open! Core
  open! Import
  module Item : Single_factor.Item
  module Key  : Key
  module Single_factor : module type of Single_factor.Make (Item)

  module Action : sig
    type t =
      | Cycle_focused_subwidget  of [ `Next | `Prev ]
      | Set_focused_subwidget    of Key.t
      | Subwidget_action         of Key.t * Single_factor.Action.t
      | Select_on_all_subwidgets of [ `All | `None ]
  end

  module Result : sig
    type t =
      { view             : Vdom.Node.t
      ; view_for_testing : string Lazy.t
      ; key_handler      : Vdom_keyboard.Keyboard_event_handler.t
      ; inject           : Action.t -> unit Vdom.Effect.t
      (** [selection] is the set of all selected items, by key. *)
      ; selection        : Item.Set.t Key.Map.t
      }
    [@@deriving fields]

    (** A DOM node very similar to [t.view], but with a keydown handler attached to deal
        with keyboard events. We provide both this and [view] / [key_handler] separately,
        as the latter is required when embedding this component into an incr_dom app --
        since incr_dom likes to always have the root node be in focus, it is required that
        one constructs one big overall Keyboard_event_handler.t for the entire page.
    *)
    val view_with_keydown_handler : t -> Vdom.Node.t
  end

  type per_subwidget =
    { default_selection_status : Selection_status.t
    ; all_items                : Item.Set.t
    }

  val bonsai
    :  ?initial_model_settings:Single_factor.Initial_model_settings.t Key.Map.t
    -> all_keys:Key.Set.t
    -> id_prefix:string Value.t
    -> per_subwidget Key.Map.t Value.t
    -> Result.t Computation.t
end

module type Multi_factor = sig
  module type S   = S
  module type Key = Key

  module Make (Item : Single_factor.Item) (Key : Key) :
    S with module Item := Item and module Key := Key
end
