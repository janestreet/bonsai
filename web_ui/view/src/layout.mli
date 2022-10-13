open! Core
open! Import

module Flex : sig
  module Horizontal_dir : sig
    type t =
      | Left_to_right
      | Right_to_left
    [@@deriving compare, equal, sexp, enumerate]
  end

  module Vertical_dir : sig
    type t =
      | Top_to_bottom
      | Bottom_to_top
    [@@deriving compare, equal, sexp, enumerate]
  end

  module Wrap : sig
    type t =
      | Wrap
      | Wrap_reverse
      | No_wrap
    [@@deriving compare, equal, sexp, enumerate]
  end

  module Align_content : sig
    type t =
      | Start
      | End
      | Normal
      | Center
      | Space_between
      | Space_around
      | Stretch
    [@@deriving compare, equal, sexp, enumerate]
  end

  module Main_axis_alignment : sig
    type t =
      | Start
      | End
      | Center
      | Space_between
      | Space_around
      | Space_evenly
    [@@deriving compare, equal, sexp, enumerate]
  end

  module Cross_axis_alignment : sig
    type t =
      | Start
      | End
      | Center
      | Stretch
      | Baseline
    [@@deriving compare, equal, sexp, enumerate]
  end
end

val hbox
  :  ?attr:Vdom.Attr.t
  -> ?gap:Css_gen.Length.t
  -> ?main_axis_alignment:Flex.Main_axis_alignment.t
  -> ?cross_axis_alignment:Flex.Cross_axis_alignment.t
  -> ?direction:Flex.Horizontal_dir.t
  -> Vdom.Node.t list
  -> Vdom.Node.t

val vbox
  :  ?attr:Vdom.Attr.t
  -> ?gap:Css_gen.Length.t
  -> ?main_axis_alignment:Flex.Main_axis_alignment.t
  -> ?cross_axis_alignment:Flex.Cross_axis_alignment.t
  -> ?direction:Flex.Vertical_dir.t
  -> Vdom.Node.t list
  -> Vdom.Node.t

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
