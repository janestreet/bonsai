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
  type t = Theme.t

  (** Returns the name of the theme *)
  val name : t -> string

  (** Fetches the theme currently installed into Bonsai's scope. *)
  val current : t Bonsai.Computation.t

  (** This function allows the caller to build a new theme based on the current theme's
      constants, installing that new theme for all users inside the given computation. *)
  val override_constants_temporarily
    :  f:(Constants.t -> Constants.t)
    -> inside:'a Bonsai.Computation.t
    -> 'a Bonsai.Computation.t

  (** [set_temporarily] will install a new theme for all theme-users inside the given
      computation. *)
  val set_temporarily
    :  t Bonsai.Value.t
    -> inside:'a Bonsai.Computation.t
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

(** A getter for the colors associated with a particular intent. *)
val intent_colors : Theme.t -> Intent.t -> Fg_bg.t

(** {1 Text}
    The text functions render a span of text.  If an [Intent.t] is passed, it'll
    render with the foreground and background color for the current theme's intent. *)

val text : Theme.t -> ?attr:Vdom.Attr.t -> ?intent:Intent.t -> string -> Vdom.Node.t

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
  :  ?attr:Vdom.Attr.t
  -> ?gap:Css_gen.Length.t
  -> ?main_axis_alignment:Flex.Main_axis_alignment.t
  -> ?cross_axis_alignment:Flex.Cross_axis_alignment.t
  -> ?direction:Flex.Horizontal_dir.t
  -> Vdom.Node.t list
  -> Vdom.Node.t

(** Builds a flexbox container for elements and lays them out vertically. *)
val vbox
  :  ?attr:Vdom.Attr.t
  -> ?gap:Css_gen.Length.t
  -> ?main_axis_alignment:Flex.Main_axis_alignment.t
  -> ?cross_axis_alignment:Flex.Cross_axis_alignment.t
  -> ?direction:Flex.Vertical_dir.t
  -> Vdom.Node.t list
  -> Vdom.Node.t

(** A horizontal flexbox container whose content will wrap
    onto multiple rows if necessary *)
val hbox_wrap
  :  ?attr:Vdom.Attr.t
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
  :  ?attr:Vdom.Attr.t
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

(** Builds a basic button with text for a label. *)
val button
  :  Theme.t
  -> ?attr:Vdom.Attr.t
  -> ?disabled:bool
  -> ?intent:Intent.t
  -> on_click:unit Effect.t
  -> string
  -> Vdom.Node.t

(** Builds a button with a group of vdom nodes inside it. *)
val button'
  :  Theme.t
  -> ?attr:Vdom.Attr.t
  -> ?disabled:bool
  -> ?intent:Intent.t
  -> on_click:unit Effect.t
  -> Vdom.Node.t list
  -> Vdom.Node.t

(** Builds a horizontally-aligned grouping of tabs, keyed on an item representing
    the tab, with one of those items serving to indicate the currently active tab. *)
val tabs
  :  Theme.t
  -> ?attr:Vdom.Attr.t
  -> ?per_tab_attr:('a -> is_active:bool -> Vdom.Attr.t)
  -> equal:('a -> 'a -> bool)
  -> on_change:(from:'a -> to_:'a -> unit Effect.t)
  -> active:'a
  -> ('a * Vdom.Node.t) list
  -> Vdom.Node.t

(** A helper function for tabs where the items representing the tabs are enumerable *)
val tabs_enum
  :  Theme.t
  -> ?attr:Vdom.Attr.t
  -> ?per_tab_attr:('a -> is_active:bool -> Vdom.Attr.t)
  -> ?tab_to_vdom:('a -> Vdom.Node.t)
  -> (module Enum with type t = 'a)
  -> on_change:(from:'a -> to_:'a -> unit Effect.t)
  -> active:'a
  -> Vdom.Node.t

(** A devbar is the attention-catching bar across the top of an app to indicate that
    the user isn't on a production instance of the application. *)
val devbar
  :  Theme.t
  -> ?attr:Vdom.Attr.t
  -> ?count:int
  -> ?intent:Intent.t
  -> string
  -> Vdom.Node.t

module Expert : sig
  include module type of struct
    include Expert
  end

  (** [override_current_theme_temporarily] allows the full extension of the current theme,
      installing this theme for the given computation. *)
  val override_current_theme_temporarily
    :  f:((module S) -> (module S))
    -> inside:'a Bonsai.Computation.t
    -> 'a Bonsai.Computation.t
end
