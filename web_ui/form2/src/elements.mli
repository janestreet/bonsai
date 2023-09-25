open! Core
open! Bonsai_web

module type Model = sig
  type t [@@deriving sexp_of]
end

module type Stringable_model = sig
  type t

  include Model with type t := t
  include Stringable with type t := t
end

(** For checkboxes and radio buttons, you can choose between
    having their visual display be the default native rendering,
    or if you want them to look like actual buttons, then the
    input element is hidden, and [extra_attrs] will be passed to
    each <label> per element. *)
module Selectable_style : sig
  type t =
    | Native
    | Button_like of { extra_attrs : checked:bool -> Vdom.Attr.t list }

  val barebones_button_like : t
end

module Non_interactive : sig
  (** This form always contains the specified value. Setting the form has no
      effect. In addition, one must specify how the form should look to the user. *)
  val constant
    :  Vdom.Node.t Value.t
    -> 'a Or_error.t Value.t
    -> ('a, Vdom.Node.t) Form.t Computation.t
end

module Textbox : sig
  val string
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?placeholder:string
    -> unit
    -> (string, Vdom.Node.t) Form.t Computation.t

  val int
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?placeholder:string
    -> unit
    -> (int, Vdom.Node.t) Form.t Computation.t

  val float
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?placeholder:string
    -> unit
    -> (float, Vdom.Node.t) Form.t Computation.t

  val sexpable
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?placeholder:string
    -> (module Sexpable with type t = 'a)
    -> ('a, Vdom.Node.t) Form.t Computation.t

  val stringable
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?placeholder:string
    -> (module Stringable with type t = 'a)
    -> ('a, Vdom.Node.t) Form.t Computation.t
end

module Password : sig
  val string
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?placeholder:string
    -> unit
    -> (string, Vdom.Node.t) Form.t Computation.t

  val stringable
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?placeholder:string
    -> (module Stringable with type t = 'a)
    -> ('a, Vdom.Node.t) Form.t Computation.t
end

module Textarea : sig
  val string
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?placeholder:string
    -> unit
    -> (string, Vdom.Node.t) Form.t Computation.t

  val int
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?placeholder:string
    -> unit
    -> (int, Vdom.Node.t) Form.t Computation.t

  val float
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?placeholder:string
    -> unit
    -> (float, Vdom.Node.t) Form.t Computation.t

  val sexpable
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?placeholder:string
    -> (module Sexpable with type t = 'a)
    -> ('a, Vdom.Node.t) Form.t Computation.t

  val stringable
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?placeholder:string
    -> (module Stringable with type t = 'a)
    -> ('a, Vdom.Node.t) Form.t Computation.t
end

module Checkbox : sig
  val bool
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> default:bool
    -> unit
    -> (bool, Vdom.Node.t) Form.t Computation.t

  val set
    :  ?style:Selectable_style.t Value.t
    -> ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?to_string:('a -> string)
    -> ?layout:[ `Vertical | `Horizontal ]
    -> ('a, 'cmp) Bonsai.comparator
    -> 'a list Value.t
    -> (('a, 'cmp) Set.t, Vdom.Node.t) Form.t Computation.t

  module Private : sig
    val make_input
      :  extra_attrs:Vdom.Attr.t list
      -> state:bool
      -> set_state:(bool -> unit Ui_effect.t)
      -> Vdom.Node.t
  end
end

module Toggle : sig
  (** Very similar to [Checkbox.bool], but with a different stylization.  Looks similar to
      the rounded variant here: https://www.w3schools.com/howto/howto_css_switch.asp *)
  val bool
    :  ?extra_attr:Vdom.Attr.t Value.t
    -> default:bool
    -> unit
    -> (bool, Vdom.Node.t) Form.t Computation.t
end

module Dropdown : sig
  val list
    :  ?init:[ `Empty | `First_item | `This of 'a Value.t ]
    -> ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?extra_option_attrs:('a -> Vdom.Attr.t list) Value.t
    -> ?to_string:('a -> string)
    -> (module Model with type t = 'a)
    -> equal:('a -> 'a -> bool)
    -> 'a list Value.t
    -> ('a, Vdom.Node.t) Form.t Computation.t

  val list_opt
    :  ?init:[ `Empty | `First_item | `This of 'a Value.t ]
    -> ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?extra_option_attrs:('a -> Vdom.Attr.t list) Value.t
    -> ?to_string:('a -> string)
    -> (module Model with type t = 'a)
    -> equal:('a -> 'a -> bool)
    -> 'a list Value.t
    -> ('a option, Vdom.Node.t) Form.t Computation.t

  val enumerable
    :  ?init:[ `Empty | `First_item | `This of 'a Value.t ]
    -> ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?extra_option_attrs:('a -> Vdom.Attr.t list) Value.t
    -> ?to_string:('a -> string)
    -> (module Bonsai.Enum with type t = 'a)
    -> ('a, Vdom.Node.t) Form.t Computation.t

  val enumerable_opt
    :  ?init:[ `Empty | `First_item | `This of 'a Value.t ]
    -> ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?extra_option_attrs:('a -> Vdom.Attr.t list) Value.t
    -> ?to_string:('a -> string)
    -> (module Bonsai.Enum with type t = 'a)
    -> ('a option, Vdom.Node.t) Form.t Computation.t

  module Private : sig
    module Opt : sig
      type 'a t =
        | Uninitialized
        | Explicitly_none
        | Set of 'a
      [@@deriving sexp, equal]

      val to_option : 'a t -> 'a option
    end

    val make_input
      :  ?to_string:('a -> string)
      -> (module Model with type t = 'a)
      -> equal:('a -> 'a -> bool)
      -> include_empty:bool
      -> default_value:'a option
      -> state:'a Opt.t
      -> set_state:('a Opt.t -> unit Ui_effect.t)
      -> extra_attrs:Vdom.Attr.t list
      -> extra_option_attrs:('a -> Vdom.Attr.t list)
      -> all:'a list
      -> Vdom.Node.t
  end
end

module Typeahead : sig
  val single
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?placeholder:string
    -> ?to_string:('a -> string) Value.t
    -> ?to_option_description:('a -> string) Value.t
    -> ?handle_unknown_option:(string -> 'a option) Value.t
    -> (module Bonsai.Model with type t = 'a)
    -> equal:('a -> 'a -> bool)
    -> all_options:'a list Value.t
    -> ('a, Vdom.Node.t) Form.t Computation.t

  val single_opt
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?placeholder:string
    -> ?to_string:('a -> string) Value.t
    -> ?to_option_description:('a -> string) Value.t
    -> ?handle_unknown_option:(string -> 'a option) Value.t
    -> (module Bonsai.Model with type t = 'a)
    -> equal:('a -> 'a -> bool)
    -> all_options:'a list Value.t
    -> ('a option, Vdom.Node.t) Form.t Computation.t

  val set
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?placeholder:string
    -> ?to_string:('a -> string) Value.t
    -> ?to_option_description:('a -> string) Value.t
    -> ?split:(string -> string list)
    -> ('a, 'cmp) Bonsai.comparator
    -> all_options:'a list Value.t
    -> (('a, 'cmp) Set.t, Vdom.Node.t) Form.t Computation.t

  val list
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?placeholder:string
    -> ?to_string:('a -> string) Value.t
    -> ?to_option_description:('a -> string) Value.t
    -> ?split:(string -> string list)
    -> ('a, _) Bonsai.comparator
    -> all_options:'a list Value.t
    -> ('a list, Vdom.Node.t) Form.t Computation.t
end

module Date_time : sig
  module Span_unit : sig
    type t =
      | Milliseconds
      | Seconds
      | Minutes
      | Hours
  end

  val date
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> unit
    -> (Date.t, Vdom.Node.t) Form.t Computation.t

  val date_opt
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> unit
    -> (Date.t option, Vdom.Node.t) Form.t Computation.t

  val time
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> unit
    -> (Time_ns.Ofday.t, Vdom.Node.t) Form.t Computation.t

  val time_opt
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> unit
    -> (Time_ns.Ofday.t option, Vdom.Node.t) Form.t Computation.t

  val time_span
    :  ?extra_unit_attrs:Vdom.Attr.t list Value.t
    -> ?extra_amount_attrs:Vdom.Attr.t list Value.t
    -> ?default_unit:Span_unit.t
    -> ?default:Time_ns.Span.t
    -> unit
    -> (Time_ns.Span.t, Vdom.Node.t) Form.t Computation.t

  val time_span_opt
    :  ?extra_unit_attrs:Vdom.Attr.t list Value.t
    -> ?extra_amount_attrs:Vdom.Attr.t list Value.t
    -> ?default_unit:Span_unit.t
    -> ?default:Time_ns.Span.t option
    -> unit
    -> (Time_ns.Span.t option, Vdom.Node.t) Form.t Computation.t

  val datetime_local
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> unit
    -> (Time_ns.t, Vdom.Node.t) Form.t Computation.t

  val datetime_local_opt
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> unit
    -> (Time_ns.t option, Vdom.Node.t) Form.t Computation.t

  module Range : sig
    val date
      :  ?extra_attr:Vdom.Attr.t Value.t
      -> ?allow_equal:bool
      -> unit
      -> (Date.t * Date.t, Vdom.Node.t) Form.t Computation.t

    val date_opt
      :  ?extra_attr:Vdom.Attr.t Value.t
      -> ?allow_equal:bool
      -> unit
      -> (Date.t option * Date.t option, Vdom.Node.t) Form.t Computation.t

    val time
      :  ?extra_attr:Vdom.Attr.t Value.t
      -> ?allow_equal:bool
      -> unit
      -> (Time_ns.Ofday.t * Time_ns.Ofday.t, Vdom.Node.t) Form.t Computation.t

    val time_opt
      :  ?extra_attr:Vdom.Attr.t Value.t
      -> ?allow_equal:bool
      -> unit
      -> (Time_ns.Ofday.t option * Time_ns.Ofday.t option, Vdom.Node.t) Form.t
         Computation.t

    val datetime_local
      :  ?extra_attr:Vdom.Attr.t Value.t
      -> ?allow_equal:bool
      -> unit
      -> (Time_ns.t * Time_ns.t, Vdom.Node.t) Form.t Computation.t

    val datetime_local_opt
      :  ?extra_attr:Vdom.Attr.t Value.t
      -> ?allow_equal:bool
      -> unit
      -> (Time_ns.t option * Time_ns.t option, Vdom.Node.t) Form.t Computation.t
  end
end

module Multiselect : sig
  val set
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?to_string:('a -> string)
    -> ?default_selection_status:Bonsai_web_ui_multi_select.Selection_status.t Value.t
    -> ('a, 'cmp) Bonsai.comparator
    -> 'a list Value.t
    -> (('a, 'cmp) Set.t, Vdom.Node.t) Form.t Computation.t

  val list
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?to_string:('a -> string)
    -> ?default_selection_status:Bonsai_web_ui_multi_select.Selection_status.t Value.t
    -> ('a, _) Bonsai.comparator
    -> 'a list Value.t
    -> ('a list, Vdom.Node.t) Form.t Computation.t
end

module Number : sig
  val int
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?min:int
    -> ?max:int
    -> ?default:int
    -> step:int
    -> unit
    -> (int, Vdom.Node.t) Form.t Computation.t

  val float
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?min:float
    -> ?max:float
    -> ?default:float
    -> step:float
    -> unit
    -> (float, Vdom.Node.t) Form.t Computation.t
end

module Range : sig
  val int
    :  ?extra_attrs:Vdom.Attr.t list Bonsai_web.Value.t
    -> ?min:int
    -> ?max:int
    -> ?left_label:Vdom.Node.t
    -> ?right_label:Vdom.Node.t
    -> ?default:int
    -> step:int
    -> unit
    -> (int, Vdom.Node.t) Form.t Computation.t

  val float
    :  ?extra_attrs:Vdom.Attr.t list Bonsai_web.Value.t
    -> ?min:float
    -> ?max:float
    -> ?left_label:Vdom.Node.t
    -> ?right_label:Vdom.Node.t
    -> ?default:float
    -> step:float
    -> unit
    -> (float, Vdom.Node.t) Form.t Computation.t
end

module Radio_buttons : sig
  val list
    :  ?style:Selectable_style.t Value.t
    -> ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?init:'a
    -> ?to_string:('a -> string)
    -> (module Model with type t = 'a)
    -> equal:('a -> 'a -> bool)
    -> layout:[ `Vertical | `Horizontal ]
    -> 'a list Value.t
    -> ('a, Vdom.Node.t) Form.t Computation.t

  val enumerable
    :  ?style:Selectable_style.t Value.t
    -> ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?init:'a
    -> ?to_string:('a -> string)
    -> (module Bonsai.Enum with type t = 'a)
    -> layout:[ `Vertical | `Horizontal ]
    -> ('a, Vdom.Node.t) Form.t Computation.t
end

module Color_picker : sig
  val hex
    :  ?extra_attr:Vdom.Attr.t Value.t
    -> unit
    -> ([ `Hex of string ], Vdom.Node.t) Form.t Computation.t
end

module Multiple : sig
  (* [stringable_list] creates a form with a single textbox which calls [of_string] on the
     contents of the textbox and adds it to the list, whenever enter is pressed. This is
     preferred to [list] when the string representation of a type is easy to write. *)
  val stringable_list
    :  ?extra_input_attr:Vdom.Attr.t Value.t
    -> ?extra_pill_container_attr:Vdom.Attr.t Value.t
    -> ?extra_pill_attr:Vdom.Attr.t Value.t
    -> ?placeholder:string
    -> (module Stringable_model with type t = 'a)
    -> equal:('a -> 'a -> bool)
    -> ('a list, Vdom.Node.t) Form.t Computation.t

  type 'view t =
    { items : ('view * unit Effect.t) list
    ; add_element : unit Effect.t
    }

  val list : ('a, 'view) Form.t Computation.t -> ('a list, 'view t) Form.t Computation.t

  val set
    :  ('a, 'cmp) Bonsai.comparator
    -> ('a, 'view) Form.t Computation.t
    -> (('a, 'cmp) Set.t, 'view t) Form.t Computation.t

  val map
    :  ('k, 'cmp) Bonsai.comparator
    -> key:('k, 'key_view) Form.t Computation.t
    -> data:('v, 'data_view) Form.t Computation.t
    -> (('k, 'v, 'cmp) Map.t, ('key_view * 'data_view) t) Form.t Computation.t
end

module File_select : sig
  (** A form element that allows the user to select a file from their local disk.

      NOTE: these widgets are not safe for use in Tangle as internally they require a
      model which cannot be [of_sexp]'d. *)

  val single_opt
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?accept:[ `Extension of string | `Mimetype of string ] list
    -> unit
    -> (Bonsai_web_ui_file.t option, Vdom.Node.t) Form.t Computation.t

  (** A form where picking a file is mandatory. The form will be in an error state until a
      file is picked. *)
  val single
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?accept:[ `Extension of string | `Mimetype of string ] list
    -> unit
    -> (Bonsai_web_ui_file.t, Vdom.Node.t) Form.t Computation.t

  val multiple
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?accept:[ `Extension of string | `Mimetype of string ] list
    -> unit
    -> (Bonsai_web_ui_file.t Filename.Map.t, Vdom.Node.t) Form.t Computation.t
end

module Freeform_multiselect : sig
  val list
    :  ?extra_attr:Vdom.Attr.t Value.t
    -> ?placeholder:string
    -> ?split:(string -> string list)
    -> unit
    -> (string list, Vdom.Node.t) Form.t Computation.t

  val set
    :  ?extra_attr:Vdom.Attr.t Value.t
    -> ?placeholder:string
    -> ?split:(string -> string list)
    -> unit
    -> (String.Set.t, Vdom.Node.t) Form.t Computation.t
end

module Rank : sig
  val list
    :  ('a, 'b) Bonsai_web_ui_reorderable_list.comparator
    -> ?enable_debug_overlay:bool
    -> ?extra_item_attrs:Vdom.Attr.t Value.t
    -> ?left:Css_gen.Length.t
    -> ?right:Css_gen.Length.t
    -> ?empty_list_placeholder:(item_is_hovered:bool Value.t -> Vdom.Node.t Computation.t)
    -> ?default_item_height:int
    -> (source:Vdom.Attr.t Value.t -> 'a Value.t -> Vdom.Node.t Computation.t)
    -> ('a list, Vdom.Node.t) Form.t Computation.t
end

module Query_box : sig
  val create_opt
    :  (module Bonsai.Comparator with type comparator_witness = 'cmp and type t = 'k)
    -> ?initial_query:string
    -> ?max_visible_items:int Value.t
    -> ?suggestion_list_kind:Bonsai_web_ui_query_box.Suggestion_list_kind.t Value.t
    -> ?selected_item_attr:Vdom.Attr.t Value.t
    -> ?extra_list_container_attr:Vdom.Attr.t Value.t
    -> ?extra_input_attr:Vdom.Attr.t Value.t
    -> ?extra_attr:Vdom.Attr.t Value.t
    -> selection_to_string:('k -> string) Value.t
    -> f:(string Value.t -> ('k, Vdom.Node.t, 'cmp) Map.t Computation.t)
    -> unit
    -> ('k option, Vdom.Node.t) Form.t Computation.t

  val create
    :  (module Bonsai.Comparator with type comparator_witness = 'cmp and type t = 'k)
    -> ?initial_query:string
    -> ?max_visible_items:int Value.t
    -> ?suggestion_list_kind:Bonsai_web_ui_query_box.Suggestion_list_kind.t Value.t
    -> ?selected_item_attr:Vdom.Attr.t Value.t
    -> ?extra_list_container_attr:Vdom.Attr.t Value.t
    -> ?extra_input_attr:Vdom.Attr.t Value.t
    -> ?extra_attr:Vdom.Attr.t Value.t
    -> selection_to_string:('k -> string) Value.t
    -> f:(string Value.t -> ('k, Vdom.Node.t, 'cmp) Map.t Computation.t)
    -> unit
    -> ('k, Vdom.Node.t) Form.t Computation.t

  val single
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?to_string:('a -> string) Value.t
    -> ?to_option_description:('a -> string) Value.t
    -> ?selected_item_attr:Vdom.Attr.t Value.t
    -> ?extra_list_container_attr:Vdom.Attr.t Value.t
    -> ?handle_unknown_option:(string -> 'a option) Value.t
    -> (module Bonsai.Comparator with type t = 'a and type comparator_witness = 'cmp)
       (* If there are duplicate items in [all_options] (according to the comparator),
       the last of the duplicates will be the only one that show up in the list
       of suggestions. *)
    -> all_options:'a list Value.t
    -> ('a, Vdom.Node.t) Form.t Computation.t

  val single_opt
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?to_string:('a -> string) Value.t
    -> ?to_option_description:('a -> string) Value.t
    -> ?selected_item_attr:Vdom.Attr.t Value.t
    -> ?extra_list_container_attr:Vdom.Attr.t Value.t
    -> ?handle_unknown_option:(string -> 'a option) Value.t
    -> (module Bonsai.Comparator with type t = 'a and type comparator_witness = 'cmp)
       (* If there are duplicate items in [all_options] (according to the comparator),
       the last of the duplicates will be the only one that show up in the list
       of suggestions. *)
    -> all_options:'a list Value.t
    -> ('a option, Vdom.Node.t) Form.t Computation.t
end

module Optional : sig
  (* [dropdown] takes an existing form, and adds a dropdown with [some_label] and
     [none_label] options. If the user selects the [some_label] option, the provided form
     is used for the inner value of the option. *)
  val dropdown
    :  ?some_label:string (** default ["Some"] *)
    -> ?none_label:string (** default ["None"] *)
    -> ('a, 'view) Form.t Computation.t
       (** shown when the [some_label] option is selected *)
    -> ('a option, Vdom.Node.t * 'view option) Form.t Computation.t
end

module Private : sig
  val sexp_to_pretty_string : ('a -> Sexp.t) -> 'a -> string
end
