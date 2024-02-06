open! Core
open! Import

(** {1 Overview}
    The [Vdom.Node] and [Vdom.Attr] modules contain primitives for constructing
    virtual-dom nodes which mirror the browsers DOM API. This is great for users who know
    exactly what they want the document to contain, but all-things-considered, is a pretty
    low-level interface.

    [Bonsai_web.View] is - at its core - a higher-level API for building [Vdom.*.t], for people
    who want a button element, and would like to avoid boilerplate for common elements
    like buttons:

    {[
      Vdom.Node.button
        ~attr:(Vdom.Attr.on_click (fun _ -> do_thing))
        [ Vdom.Node.text "click me" ]

      ;; (* vs *)

      View.button theme ~on_click:do_thing "click me"
    ]}

    or basic layout primitives:

    {[
      Vdom.Node.div
        ~attr:(Vdom.Attr.style (Css_gen.flex_container ~direction:`Row ~wrap:`Wrap ()))
        [a; b; c]

      ;; (* vs *)

      View.hbox ~wrap:Wrap [a; b; c]
    ]}


    {1 Theming}

    Another big focus of the View module is an API for building composable, extendable,
    and customizable themes.  All of the [View.*] functions which take a [View.Theme.t] as
    the first argument are handing over basic "look and feel" decisions to the provided
    theme.  However, themes are extendable by the user of that theme, allowing minor changes
    like color choices, but also the power to completely override rendering of specific
    components if necessary.

*)

module Theme : sig
  type t

  (** Returns the name of the theme *)
  val name : t -> string

  (** Fetches the theme currently installed into Bonsai's scope. *)
  val current : t Bonsai.Computation.t

  (** Sets the given theme as 'current' for the provided computation, and registers
      the returned Vdom node as the "application-node" giving it the power to do things
      like set the window's background color and font-family. *)
  val set_for_app
    :  t Bonsai.Value.t
    -> Vdom.Node.t Bonsai.Computation.t
    -> Vdom.Node.t Bonsai.Computation.t

  (** Same as [set_for_app] but permits the app to return an arbitrary value in addition
      to the top-level app component *)
  val set_for_app'
    :  t Bonsai.Value.t
    -> ('a * Vdom.Node.t) Bonsai.Computation.t
    -> ('a * Vdom.Node.t) Bonsai.Computation.t

  (** This function allows the caller to build a new theme based on the current theme's
      constants, installing that new theme for all users inside the given computation. *)
  val override_constants_for_computation
    :  f:(Constants.t -> Constants.t)
    -> 'a Bonsai.Computation.t
    -> 'a Bonsai.Computation.t

  (** [set_temporarily] will install a new theme for all theme-users inside the given
      computation. *)
  val set_for_computation
    :  t Bonsai.Value.t
    -> 'a Bonsai.Computation.t
    -> 'a Bonsai.Computation.t
end

module type Enum = sig
  type t [@@deriving enumerate, equal, sexp_of]
end

(** {1 Constants}

    Themes have a set of associated constants, which can be accessed by the functions
    in this section.  These constants may be overritten by calling
    [Theme.override_constants_temporarily]. *)

module Constants = Constants

(** A getter for the constants in a theme. *)
val constants : Theme.t -> Constants.t

module Color := Css_gen.Color

module Fg_bg : sig
  type t =
    { foreground : Color.t
    ; background : Color.t
    }
end

module Intent : sig
  (** An [Intent.t] is an indicator to the component that it should render
      the component differently.  Usually this means incorporating the
      intent-colors as defined by the theme. *)

  type t =
    | Info
    | Success
    | Warning
    | Error
  [@@deriving sexp, equal, compare, enumerate]
end

(** A getter for the primary colors in a theme. *)
val primary_colors : Theme.t -> Fg_bg.t

(** A getter for the "extreme" colors.  Extreme colors mimic the primary colors,
    but have a higher contrast ratio. *)
val extreme_colors : Theme.t -> Fg_bg.t

(* A color that is used for the border between a [primary] and [extreme] color set *)
val extreme_primary_border_color : Theme.t -> Color.t

(** A getter for the colors associated with a particular intent. *)
val intent_colors : Theme.t -> Intent.t -> Fg_bg.t

type 'a format := ('a, unit, string, Vdom.Node.t) format4

(** {1 Text}
    The text functions render a <span> of text with optional attributes.
    [textf] can be used with format syntax. *)

val text : ?attrs:Vdom.Attr.t list -> string -> Vdom.Node.t
val textf : ?attrs:Vdom.Attr.t list -> 'a format -> 'a

module Font_style : sig
  type t =
    | Regular
    | Bold
    | Italic
    | Underlined
  [@@deriving sexp, equal, compare, enumerate]
end

module Font_size : sig
  type t =
    | Small
    | Regular
    | Large
  [@@deriving sexp, equal, compare, enumerate]
end

(* [themed_text] and [themed_textf] can be given an intent, as well as specify the font
   style and size. *)
val themed_text
  :  Theme.t
  -> ?attrs:Vdom.Attr.t list
  -> ?intent:Intent.t
  -> ?style:Font_style.t
  -> ?size:Font_size.t
  -> string
  -> Vdom.Node.t

val themed_textf
  :  Theme.t
  -> ?attrs:Vdom.Attr.t list
  -> ?intent:Intent.t
  -> ?style:Font_style.t
  -> ?size:Font_size.t
  -> 'a format
  -> 'a

(** {1 Layout}

    Layout helper functions are used to wrap other vdom nodes inside a container.
    [hbox] and [vbox] both use css "flexbox", for more, read this documentation:

    https://css-tricks.com/snippets/css/a-guide-to-flexbox/

    The only terminology difference between the css properties and our function
    arguments is that we refer to "justify-content" as "main_axis_alignment"
    and "align-items" as "cross_axis_alignment".

    Layout helpers do not take a Theme.t as input because themes should not
    be able to influence a layout explicitly requested by the user. *)

module Flex = Layout.Flex

(** Builds a flexbox container for elements and lays them out horizontally. *)
val hbox
  :  ?attrs:Vdom.Attr.t list
  -> ?gap:Css_gen.Length.t
  -> ?main_axis_alignment:Flex.Main_axis_alignment.t
  -> ?cross_axis_alignment:Flex.Cross_axis_alignment.t
  -> ?direction:Flex.Horizontal_dir.t
  -> Vdom.Node.t list
  -> Vdom.Node.t

(** Builds a flexbox container for elements and lays them out vertically. *)
val vbox
  :  ?attrs:Vdom.Attr.t list
  -> ?gap:Css_gen.Length.t
  -> ?main_axis_alignment:Flex.Main_axis_alignment.t
  -> ?cross_axis_alignment:Flex.Cross_axis_alignment.t
  -> ?direction:Flex.Vertical_dir.t
  -> Vdom.Node.t list
  -> Vdom.Node.t

(** A horizontal flexbox container whose content will wrap
    onto multiple rows if necessary *)
val hbox_wrap
  :  ?attrs:Vdom.Attr.t list
  -> ?row_gap:Css_gen.Length.t
  -> ?column_gap:Css_gen.Length.t
  -> ?main_axis_alignment:Flex.Main_axis_alignment.t
  -> ?cross_axis_alignment:Flex.Cross_axis_alignment.t
  -> ?direction:Flex.Horizontal_dir.t
  -> ?align_content:Flex.Align_content.t
  -> Vdom.Node.t list
  -> Vdom.Node.t

(** A vertical flexbox container whose content will wrap
    onto multiple columns if necessary *)
val vbox_wrap
  :  ?attrs:Vdom.Attr.t list
  -> ?row_gap:Css_gen.Length.t
  -> ?column_gap:Css_gen.Length.t
  -> ?main_axis_alignment:Flex.Main_axis_alignment.t
  -> ?cross_axis_alignment:Flex.Cross_axis_alignment.t
  -> ?direction:Flex.Vertical_dir.t
  -> ?align_content:Flex.Align_content.t
  -> Vdom.Node.t list
  -> Vdom.Node.t

(** {1 Interactive Elements}

    These node constructors are for elements that the user interacts with. *)

(** Builds a basic button with text for a label.

    Optional and named arguments:

    - [attr], if provided, will be attached to the topmost button element.
    - [disabled] defaults to false.  If provided (and set to true), the button
      will be marked as disabled and will be unclickable.
    - [intent] is used by the theme to color and style the button to indicate
      the intent of the button.
    - [tooltip] is used to display some text near the button when hovered.
    - [on_click] contains an effect to schedule when the button is clicked

    The unnamed string parameter is used as the label for the button *)
val button
  :  Theme.t
  -> ?attrs:Vdom.Attr.t list
  -> ?disabled:bool
  -> ?intent:Intent.t
  -> ?tooltip:string
  -> on_click:unit Effect.t
  -> string
  -> Vdom.Node.t

(** Same as [button] but the contents of the button are specified as a list
    of vdom nodes instead of as a string *)
val button'
  :  Theme.t
  -> ?attrs:Vdom.Attr.t list
  -> ?disabled:bool
  -> ?intent:Intent.t
  -> ?tooltip:string
  -> on_click:unit Effect.t
  -> Vdom.Node.t list
  -> Vdom.Node.t

module Tooltip_direction : sig
  type t =
    | Top
    | Right
    | Bottom
    | Left
end

(** Tooltips can be used to provide more information to a user when they
    hover over an element. *)
val tooltip
  :  Theme.t
  -> ?container_attrs:Vdom.Attr.t list
  -> ?tooltip_attrs:Vdom.Attr.t list
  -> ?direction:Tooltip_direction.t
  -> tooltip:string
  -> string
  -> Vdom.Node.t

(** [tooltip'] is just like [tooltip] except that it allows both the tooltip
    and the wrapped element to be arbitrary vdom nodes instead of just [string] *)
val tooltip'
  :  Theme.t
  -> ?container_attrs:Vdom.Attr.t list
  -> ?tooltip_attrs:Vdom.Attr.t list
  -> ?direction:Tooltip_direction.t
  -> tooltip:Vdom.Node.t
  -> Vdom.Node.t
  -> Vdom.Node.t

(** Builds a horizontally-aligned grouping of tabs, keyed on an item representing
    the tab, with one of those items serving to indicate the currently active tab. *)
val tabs
  :  Theme.t
  -> ?attrs:Vdom.Attr.t list
  -> ?per_tab_attrs:('a -> is_active:bool -> Vdom.Attr.t list)
  -> equal:('a -> 'a -> bool)
  -> on_change:(from:'a -> to_:'a -> unit Effect.t)
  -> active:'a
  -> ('a * Vdom.Node.t) list
  -> Vdom.Node.t

(** A helper function for tabs where the items representing the tabs are enumerable *)
val tabs_enum
  :  Theme.t
  -> ?attrs:Vdom.Attr.t list
  -> ?per_tab_attrs:('a -> is_active:bool -> Vdom.Attr.t list)
  -> ?tab_to_vdom:('a -> Vdom.Node.t)
  -> (module Enum with type t = 'a)
  -> on_change:(from:'a -> to_:'a -> unit Effect.t)
  -> active:'a
  -> Vdom.Node.t

(** A devbar is the attention-catching bar across the top of an app to indicate that
    the user isn't on a production instance of the application. *)
val devbar
  :  Theme.t
  -> ?attrs:Vdom.Attr.t list
  -> ?count:int
  -> ?intent:Intent.t
  -> string
  -> Vdom.Node.t

module Card_title_kind : sig
  type t =
    | Prominent (** Rendered in an easier to see bar. Use to make your title stand out. *)
    | Discreet
        (** Title is rendered alongside the top of the border of the card in a more discrete way. Use to give structure to your content. *)
end

(** A "card" is a way of highlighting important messages, and to also bring some
    structure/sense of hierarchy to your app. This component is the conceptual equivalent
    of "paper" in other UI component frameworks/toolkits. *)
val card
  :  Theme.t
  -> ?container_attrs:Vdom.Attr.t list
  -> ?title_attrs:Vdom.Attr.t list
  -> ?content_attrs:Vdom.Attr.t list
  -> ?intent:Intent.t
  -> ?title:string
  -> ?title_kind:Constants.Card_title_kind.t
  -> ?on_click:unit Effect.t
  -> string
  -> Vdom.Node.t

(* Like [card], but allows for arbitrary vdom nodes in the title and in the content. *)
val card'
  :  Theme.t
  -> ?container_attrs:Vdom.Attr.t list
  -> ?title_attrs:Vdom.Attr.t list
  -> ?content_attrs:Vdom.Attr.t list
  -> ?intent:Intent.t
  -> ?title:Vdom.Node.t list
  -> ?title_kind:Constants.Card_title_kind.t
  -> ?on_click:unit Effect.t
  -> Vdom.Node.t list
  -> Vdom.Node.t

(** A module for building tables *)
module Table : sig
  module Col : sig
    (** A ['a Col.t] represents a column (or set of columns) that defines a way
        to render cells from a row in the table that has type ['a]. *)
    type 'a t

    (** [make] builds a column for a row ['a] by providing a getter ['a -> 'b] and
        a renderer for the ['b].  The first parameter is a string that is used for
        the label of the column.

        [header_attr] is an optional attribute attached to the <th> element containing
        the label.

        [cell_attr] is for building attribute that will be attached to the <td> element
        containing the rendered content of the cell. *)
    val make
      :  ?cell_attrs:('b -> Vdom.Attr.t list)
      -> ?header_attrs:Vdom.Attr.t list
      -> string
      -> get:('a -> 'b)
      -> render:(Theme.t -> 'b -> Vdom.Node.t)
      -> 'a t

    (** [make_opt] is the same as [make] except that the return value from [get] can
        be an option *)
    val make_opt
      :  ?cell_attrs:('b -> Vdom.Attr.t list)
      -> ?header_attrs:Vdom.Attr.t list
      -> string
      -> get:('a -> 'b option)
      -> render:(Theme.t -> 'b -> Vdom.Node.t)
      -> 'a t

    (** [group] produces a column group over the provided list of columns with the
        string parameter being used as the label for the group.

        [header_attr] behaves the same as it does for the [make] function *)
    val group : ?header_attrs:Vdom.Attr.t list -> string -> 'a t list -> 'a t

    (* [lift] is used to move a column group from one type to another *)
    val lift : 'a t -> f:('b -> 'a) -> 'b t

    (** The remaining "prime" functions in this module are identical to their
        "non-prime" versions except that the "label" argument is an arbitrary Vdom node.*)

    val make'
      :  ?cell_attrs:('b -> Vdom.Attr.t list)
      -> ?header_attrs:Vdom.Attr.t list
      -> Vdom.Node.t
      -> get:('a -> 'b)
      -> render:(Theme.t -> 'b -> Vdom.Node.t)
      -> 'a t

    val group' : ?header_attrs:Vdom.Attr.t list -> Vdom.Node.t -> 'a t list -> 'a t

    val make_opt'
      :  ?cell_attrs:('b -> Vdom.Attr.t list)
      -> ?header_attrs:Vdom.Attr.t list
      -> Vdom.Node.t
      -> get:('a -> 'b option)
      -> render:(Theme.t -> 'b -> Vdom.Node.t)
      -> 'a t
  end

  val render
    :  Theme.t
    -> ?table_attrs:Vdom.Attr.t list
    -> ?row_attrs:('a -> Vdom.Attr.t list)
    -> 'a Col.t list
    -> 'a list
    -> Vdom.Node.t
end

(** Hooks for building controlled form inputs. You may want to consider
    [Bonsai_web_ui_form] instead of using these directly, as it provides combinators for
    building large forms. These methods underlie [Bonsai_web_ui_form]'s analagous
    elements. *)
module Form_inputs : sig
  val textbox
    :  Theme.t
    -> ?attrs:Vdom.Attr.t list
    -> ?placeholder:string
    -> ?key:string
    -> allow_updates_when_focused:[ `Always | `Never ]
    -> disabled:bool
    -> value:string
    -> set_value:(string -> unit Effect.t)
    -> unit
    -> Vdom.Node.t

  val password
    :  Theme.t
    -> ?attrs:Vdom.Attr.t list
    -> ?placeholder:string
    -> ?key:string
    -> allow_updates_when_focused:[ `Always | `Never ]
    -> disabled:bool
    -> value:string
    -> set_value:(string -> unit Effect.t)
    -> unit
    -> Vdom.Node.t

  val textarea
    :  Theme.t
    -> ?attrs:Vdom.Attr.t list
    -> ?placeholder:string
    -> ?key:string
    -> allow_updates_when_focused:[ `Always | `Never ]
    -> disabled:bool
    -> value:string
    -> set_value:(string -> unit Effect.t)
    -> unit
    -> Vdom.Node.t

  val number
    :  Theme.t
    -> ?attrs:Vdom.Attr.t list
    -> ?placeholder:string
    -> ?min:float
    -> ?max:float
    -> ?key:string
    -> allow_updates_when_focused:[ `Always | `Never ]
    -> disabled:bool
    -> step:float
    -> value:float option
    -> set_value:(float option -> unit Effect.t)
    -> unit
    -> Vdom.Node.t

  val range
    :  Theme.t
    -> ?attrs:Vdom.Attr.t list
    -> ?min:float
    -> ?max:float
    -> ?key:string
    -> allow_updates_when_focused:[ `Always | `Never ]
    -> disabled:bool
    -> step:float
    -> value:float
    -> set_value:(float -> unit Effect.t)
    -> unit
    -> Vdom.Node.t
end

module For_components : sig
  module Codemirror : sig
    val theme : Theme.t -> For_codemirror.Theme.t option
  end

  module Forms : sig
    val to_vdom
      :  Theme.t
      -> ?on_submit:Form_view.submission_options
      -> ?editable:Form_view.editable
      -> Form_view.t
      -> Vdom.Node.t

    val to_vdom_plain
      :  Theme.t
      -> ?editable:Form_view.editable
      -> Form_view.t
      -> Vdom.Node.t list

    val view_error : Theme.t -> Error.t -> Vdom.Node.t list

    val remove_item
      :  Theme.t
      -> ?editable:Form_view.editable
      -> Form_view.remove_item
      -> index:int
      -> Vdom.Node.t

    val append_item
      :  Theme.t
      -> ?editable:Form_view.editable
      -> Form_view.append_item
      -> Vdom.Node.t
  end

  module Prt : sig
    val styling : Theme.t -> For_prt.t
  end
end

module App : sig
  val top_attr : Theme.t -> Vdom.Attr.t Lazy.t
end

module Expert : sig
  open Underlying_intf

  val default_theme : Theme.t
  val override_theme : Theme.t -> f:((module S) -> (module S)) -> Theme.t

  (** This function allows the caller to build a new theme based on a theme. *)
  val override_constants : Theme.t -> f:(Constants.t -> Constants.t) -> Theme.t

  (** [override_current_theme_temporarily] allows the full extension of the current theme,
      installing this theme for the given computation. *)
  val override_theme_for_computation
    :  f:((module S) -> (module S))
    -> 'a Bonsai.Computation.t
    -> 'a Bonsai.Computation.t

  module For_codemirror = For_codemirror
  module Form_context = Form_context
end

(** The [Raw] module contains helper functions for building vdom nodes that have styling
    pulled from a theme, but rely on the user to properly structure and organize the nodes
    manually.  You probably shouldn't use these APIs if the standard functions are sufficient *)
module Raw : Raw_intf.S with module Theme := Theme
