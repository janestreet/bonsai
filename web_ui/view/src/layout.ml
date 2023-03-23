open! Core
open! Import

module Flex = struct
  module Horizontal_dir = struct
    type t =
      | Left_to_right
      | Right_to_left
    [@@deriving equal, compare, sexp, enumerate]
  end

  module Vertical_dir = struct
    type t =
      | Top_to_bottom
      | Bottom_to_top
    [@@deriving equal, compare, sexp, enumerate]
  end

  module Wrap = struct
    type t =
      | Wrap
      | Wrap_reverse
      | No_wrap
    [@@deriving equal, compare, sexp, enumerate]
  end

  module Align_content = struct
    type t =
      | Start
      | End
      | Normal
      | Center
      | Space_between
      | Space_around
      | Stretch
    [@@deriving equal, compare, sexp, enumerate]
  end

  module Main_axis_alignment = struct
    type t =
      | Start
      | End
      | Center
      | Space_between
      | Space_around
      | Space_evenly
    [@@deriving equal, compare, sexp, enumerate]
  end

  module Cross_axis_alignment = struct
    type t =
      | Start
      | End
      | Center
      | Stretch
      | Baseline
    [@@deriving equal, compare, sexp, enumerate]
  end
end

let to_css_gen_wrap : Flex.Wrap.t -> _ = function
  | No_wrap -> `Nowrap
  | Wrap -> `Wrap
  | Wrap_reverse -> `Wrap_reverse
;;

let to_css_gen_align_content : Flex.Align_content.t -> _ = function
  | Start -> `Flex_start
  | End -> `Flex_end
  | Normal -> `Normal
  | Center -> `Center
  | Space_between -> `Space_between
  | Space_around -> `Space_around
  | Stretch -> `Stretch
;;

let to_css_gen_justify_content : Flex.Main_axis_alignment.t -> _ = function
  | Start -> `Flex_start
  | End -> `Flex_end
  | Center -> `Center
  | Space_between -> `Space_between
  | Space_around -> `Space_around
  | Space_evenly -> `Space_evenly
;;

let to_css_gen_align_items : Flex.Cross_axis_alignment.t -> _ = function
  | Start -> `Flex_start
  | End -> `Flex_end
  | Center -> `Center
  | Stretch -> `Stretch
  | Baseline -> `Baseline
;;

let to_css_gen_horizontal_direction : Flex.Horizontal_dir.t -> _ = function
  | Left_to_right -> `Row
  | Right_to_left -> `Row_reverse
;;

let to_css_gen_vertical_direction : Flex.Vertical_dir.t -> _ = function
  | Top_to_bottom -> `Column
  | Bottom_to_top -> `Column_reverse
;;

let box
      ~(default_direction : 'direction option)
      ~(direction_to_css_gen_direction :
          'direction -> [ `Column | `Column_reverse | `Default | `Row | `Row_reverse ])
      ?(attrs = [])
      ?row_gap
      ?column_gap
      ?main_axis_alignment
      ?cross_axis_alignment
      ?direction
      ?wrap
      ?align_content
      children
  =
  let direction =
    match default_direction, direction with
    | _, Some direction -> direction_to_css_gen_direction direction
    | Some default_direction, None -> direction_to_css_gen_direction default_direction
    | _ -> `Default
  in
  let wrap = Option.value_map wrap ~f:to_css_gen_wrap ~default:`Default in
  let align_content = Option.map ~f:to_css_gen_align_content align_content in
  let justify_content = Option.map ~f:to_css_gen_justify_content main_axis_alignment in
  let align_items = Option.map ~f:to_css_gen_align_items cross_axis_alignment in
  let flex_options =
    Css_gen.flex_container
      ~direction
      ~wrap
      ?align_items
      ?align_content
      ?justify_content
      ?row_gap
      ?column_gap
      ()
  in
  Vdom.Node.div ~attrs:(Vdom.Attr.style flex_options :: attrs) children
;;

let hbox ?attrs ?gap =
  (* NOTE: Since display: flex defaults to horizontal layout, [`Default] is picked
     when a direction is not specified for hboxes. *)
  box
    ~default_direction:None
    ~direction_to_css_gen_direction:to_css_gen_horizontal_direction
    ?attrs
    ?column_gap:gap
    ?row_gap:None
    ?align_content:None
    ?wrap:None
;;

let vbox ?attrs ?gap =
  box
    ~default_direction:(Some Flex.Vertical_dir.Top_to_bottom)
    ~direction_to_css_gen_direction:to_css_gen_vertical_direction
    ?attrs
    ?row_gap:gap
    ?column_gap:None
    ?align_content:None
    ?wrap:None
;;

let hbox_wrap =
  (* NOTE: Since display: flex defaults to horizontal layout, [`Default] is picked
     when a direction is not specified for hboxes. *)
  box
    ~default_direction:None
    ~direction_to_css_gen_direction:to_css_gen_horizontal_direction
    ~wrap:Wrap
;;

let vbox_wrap =
  box
    ~default_direction:(Some Flex.Vertical_dir.Top_to_bottom)
    ~direction_to_css_gen_direction:to_css_gen_vertical_direction
    ~wrap:Wrap
;;
