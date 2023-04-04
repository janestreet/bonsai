[@@@alert
  private_bonsai_view_library
    {|
This library is private, for use by Bonsai developers in [lib/bonsai/web_ui/view] and
[lib/bonsai/web_ui/form]. If that doesn't apply to you, then you should use the public
[Bonsai_web_ui_form] library, which re-exports this functionality as part of its [View]
module.
|}]

open! Core
open! Import

type editable =
  [ `Yes_always
  | `Currently_yes
  | `Currently_no
  ]

type button_location =
  [ `Before
  | `After
  ]

type submission_options =
  { on_submit : unit Ui_effect.t option
  ; handle_enter : bool
  ; button_text : string option
  ; button_attr : Vdom.Attr.t
  ; button_location : button_location
  }

(* For creating *)
type context =
  { tooltip : Vdom.Node.t option
  ; error : Error.t option
  ; label : Vdom.Node.t option
  }
[@@deriving sexp_of]

type append_item =
  | Append_info of
      { append : unit Effect.t
      ; text : string option
      }
  | Append_view of Vdom.Node.t
[@@deriving sexp_of]

type remove_item =
  | Remove_info of
      { remove : unit Effect.t
      ; element_label : (delete_button:Vdom.Node.t -> int -> Vdom.Node.t) option
      }
  | Remove_view of Vdom.Node.t
[@@deriving sexp_of]

type t =
  { context : context
  ; view : view
  }

and view =
  | Empty
  | Collapsible of collapsible
  | Raw of raw
  | Record of field list
  | Variant of variant
  | List of list_view
  | Tuple of t list
  | Option of option_view

and raw =
  { id : string
  ; raw_view : context -> editable:editable -> Vdom.Node.t
  }

and collapsible =
  { collapse_label : Vdom.Node.t
  ; state : collapsed_state
  }

and collapsed_state =
  | Collapsed of t option
  | Expanded of t

and option_view =
  { toggle : Vdom.Node.t
  ; status : option_status
  }

and option_status =
  | Currently_some of t
  | Currently_none of t option

and list_view =
  { list_items : list_item list
  ; append_item : append_item
  ; legacy_button_position : [ `Inline | `Indented ]
  }

and list_item =
  { item_view : t
  ; remove_item : remove_item
  }

and variant =
  { clause_selector : Vdom.Node.t
  ; selected_clause : clause option
  }

and clause =
  { clause_name : string
  ; clause_view : t
  }

and field =
  { field_name : string
  ; field_view : t
  }
[@@deriving sexp_of]

(** [of_vdom] creates a Form's view from a raw [Vdom.Node.t]. [id] is used to disambiguate
    different input fields during [Vdom] diffing and patching, as well as linking a label
    to the input. That is, if you attach the supplied [id] to a text input, then users who
    click on the label will focus the textbox.

    Because ids are required to be unique, you should ~always use [Bonsai.path_id] to
    generate the [id] that you pass. *)
val of_vdom : id:string -> Vdom.Node.t -> t

(** [of_vdom'] is like [of_vdom], but allows access to some extra metadata that is stored
    in the form's view. *)
val of_vdom' : id:string -> (context -> editable:editable -> Vdom.Node.t) -> t

(** An [empty] form view. *)
val empty : t

(** [tuple] combines a list of [t] into a single one. *)
val tuple : t list -> t

(** [record] combines a list of fields (field names and their corresponding views) into a
    view representing a record with those fields. *)
val record : field list -> t

(** [variant] takes a selector for selecting which variant case, as well as an optional
    [selected_clause] which contains the name of the currently selected clause and the
    [t] representing that clause's arguments. *)
val variant : clause_selector:Vdom.Node.t -> selected_clause:clause option -> t

(** [option] takes a [toggle] for switching between [None] and [Some _], as well as a
    status for the form. [status] can be:
    - [Currently_some view] if toggle is set to [Some _] and [view] represents the form
      for the type in the option.
    - [Currently_none None] if the toggle is set to [None] and we shouldn't show any view
      for the [None] case
    - [Currently_none (Some view)] if the toggle is set to [None] and we want to display
      [view] when the toggle is set to [None] (e.g. displaying a greyed out textbox) *)
val option : toggle:Vdom.Node.t -> status:option_status -> t

(** [collapsible] represents a collapsible subform. The supplied [label] can be used to
    toggle open/closed the collapsible [t].

    [collapsible_state] can be:
    - [Collapsed None] if the view is collapsed and we shouldn't render any subform
    - [Collapsed (Some view)] if the view is collapsed but we want to render a subform
      in this case (e.g. displaying [...] in a code editor when folded)
    - [Expanded view] if the view is expanded and [view] is the subform to show *)
val collapsible : label:Vdom.Node.t -> state:collapsed_state -> t

(** [list_item] represents a single form in an extendable list.

    [remove_item] can either be:
    - [Remove_info { remove; element_label }] in which case the rendering function will
      decide how to render a remove button for this item with [remove] being scheduled on
      click and [element_label] being called to generate the label for this element.
    - [Remove_view view] in which case [view] will be used as the remove button *)
val list_item : view:t -> remove_item:remove_item -> list_item

(** [list] represents a form for an extendable list.

    [append_item] can either be:
    - [Append_info { append; text }] in which case the rendering function will decide how
      to render an append button with [append] being scheduled on click and [text] being the
      text for the button
    - [Append_view view] in which case [view] will be used as the append button *)
val list
  :  append_item:append_item
  -> legacy_button_position:[ `Inline | `Indented ]
  -> list_item list
  -> t

val suggest_error : Error.t -> t -> t
val suggest_label' : Vdom.Node.t -> t -> t
val suggest_label : string -> t -> t
val set_label : Vdom.Node.t -> t -> t
val set_tooltip : Vdom.Node.t -> t -> t

(* For rendering *)

val with_fieldset : currently_editable:bool -> Vdom.Node.t -> Vdom.Node.t
