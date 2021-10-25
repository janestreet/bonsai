open! Core
open! Bonsai_web

module Textbox : sig
  val string
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?placeholder:string
    -> Source_code_position.t
    -> string Form.t Computation.t

  val int
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?placeholder:string
    -> Source_code_position.t
    -> int Form.t Computation.t

  val float
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?placeholder:string
    -> Source_code_position.t
    -> float Form.t Computation.t

  val sexpable
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?placeholder:string
    -> Source_code_position.t
    -> (module Sexpable with type t = 'a)
    -> 'a Form.t Computation.t

  val stringable
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?placeholder:string
    -> Source_code_position.t
    -> (module Stringable with type t = 'a)
    -> 'a Form.t Computation.t
end

module Textarea : sig
  val string
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> Source_code_position.t
    -> string Form.t Computation.t

  val int
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> Source_code_position.t
    -> int Form.t Computation.t

  val float
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> Source_code_position.t
    -> float Form.t Computation.t

  val sexpable
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> Source_code_position.t
    -> (module Sexpable with type t = 'a)
    -> 'a Form.t Computation.t

  val stringable
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> Source_code_position.t
    -> (module Stringable with type t = 'a)
    -> 'a Form.t Computation.t
end

module Checkbox : sig
  val bool
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> Source_code_position.t
    -> default:bool
    -> bool Form.t Computation.t

  val set
    :  Source_code_position.t
    -> ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?to_string:('a -> string)
    -> ('a, 'cmp) Bonsai.comparator
    -> 'a list Value.t
    -> ('a, 'cmp) Set.t Form.t Computation.t
end

module Dropdown : sig
  val list
    :  Source_code_position.t
    -> ?init:[ `Empty | `First_item | `This of 'a Value.t ]
    -> ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?to_string:('a -> string)
    -> (module Bonsai.Model with type t = 'a)
    -> 'a list Value.t
    -> 'a Form.t Computation.t

  val list_opt
    :  Source_code_position.t
    -> ?init:[ `Empty | `First_item | `This of 'a Value.t ]
    -> ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?to_string:('a -> string)
    -> (module Bonsai.Model with type t = 'a)
    -> 'a list Value.t
    -> 'a option Form.t Computation.t

  val enumerable
    :  Source_code_position.t
    -> ?init:[ `Empty | `First_item | `This of 'a Value.t ]
    -> ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?to_string:('a -> string)
    -> (module Bonsai.Enum with type t = 'a)
    -> 'a Form.t Computation.t

  val enumerable_opt
    :  Source_code_position.t
    -> ?init:[ `Empty | `First_item | `This of 'a Value.t ]
    -> ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?to_string:('a -> string)
    -> (module Bonsai.Enum with type t = 'a)
    -> 'a option Form.t Computation.t
end

module Typeahead : sig
  val single
    :  Source_code_position.t
    -> ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?placeholder:string
    -> ?to_string:('a -> string) Bonsai.Value.t
    -> (module Bonsai.Model with type t = 'a)
    -> all_options:'a list Value.t
    -> 'a Form.t Computation.t

  val single_opt
    :  Source_code_position.t
    -> ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?placeholder:string
    -> ?to_string:('a -> string) Bonsai.Value.t
    -> (module Bonsai.Model with type t = 'a)
    -> all_options:'a list Value.t
    -> 'a option Form.t Computation.t

  val set
    :  Source_code_position.t
    -> ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?placeholder:string
    -> ?to_string:('a -> string)
    -> ?split:(string -> string list)
    -> ('a, 'cmp) Bonsai.comparator
    -> all_options:'a list Value.t
    -> ('a, 'cmp) Set.t Form.t Computation.t

  val list
    :  Source_code_position.t
    -> ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?placeholder:string
    -> ?to_string:('a -> string)
    -> ?split:(string -> string list)
    -> ('a, _) Bonsai.comparator
    -> all_options:'a list Value.t
    -> 'a list Form.t Computation.t
end

module Date_time : sig
  val date
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> Source_code_position.t
    -> Date.t Form.t Computation.t

  val date_opt
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> Source_code_position.t
    -> Date.t option Form.t Computation.t

  val time
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> Source_code_position.t
    -> Time_ns.Ofday.t Form.t Computation.t

  val time_opt
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> Source_code_position.t
    -> Time_ns.Ofday.t option Form.t Computation.t

  val datetime_local
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> Source_code_position.t
    -> Time_ns.t Form.t Computation.t

  val datetime_local_opt
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> Source_code_position.t
    -> Time_ns.t option Form.t Computation.t
end

module Multiselect : sig
  val set
    :  Source_code_position.t
    -> ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?to_string:('a -> string)
    -> ('a, 'cmp) Bonsai.comparator
    -> 'a list Value.t
    -> ('a, 'cmp) Set.t Form.t Computation.t

  val list
    :  Source_code_position.t
    -> ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?to_string:('a -> string)
    -> ('a, _) Bonsai.comparator
    -> 'a list Value.t
    -> 'a list Form.t Computation.t
end

module Number : sig
  val int
    :  Source_code_position.t
    -> ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?min:int
    -> ?max:int
    -> default:int
    -> step:int
    -> unit
    -> int Form.t Computation.t

  val float
    :  Source_code_position.t
    -> ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?min:float
    -> ?max:float
    -> default:float
    -> step:float
    -> unit
    -> float Form.t Computation.t
end

module Range : sig
  val int
    :  Source_code_position.t
    -> ?extra_attrs:Vdom.Attr.t list Bonsai_web.Value.t
    -> ?min:int
    -> ?max:int
    -> default:int
    -> step:int
    -> unit
    -> int Form.t Computation.t

  val float
    :  Source_code_position.t
    -> ?extra_attrs:Vdom.Attr.t list Bonsai_web.Value.t
    -> ?min:float
    -> ?max:float
    -> default:float
    -> step:float
    -> unit
    -> float Form.t Computation.t
end

module Radio_buttons : sig
  val list
    :  Source_code_position.t
    -> ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?to_string:('a -> string)
    -> (module Bonsai.Model with type t = 'a)
    -> layout:[ `Vertical | `Horizontal ]
    -> 'a list Value.t
    -> 'a Form.t Computation.t

  val enumerable
    :  Source_code_position.t
    -> ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?to_string:('a -> string)
    -> (module Bonsai.Enum with type t = 'a)
    -> layout:[ `Vertical | `Horizontal ]
    -> 'a Form.t Computation.t
end

module Multiple : sig
  val list
    :  Source_code_position.t
    -> ?element_group_label:
         (delete_button:Vdom.Node.t -> int -> 'a Or_error.t -> Vdom.Node.t)
    -> ?add_element_text:string
    -> 'a Form.t Computation.t
    -> 'a list Form.t Computation.t

  val set
    :  Source_code_position.t
    -> ?element_group_label:
         (delete_button:Vdom.Node.t -> int -> 'a Or_error.t -> Vdom.Node.t)
    -> ?add_element_text:string
    -> ('a, 'cmp) Bonsai.comparator
    -> 'a Form.t Computation.t
    -> ('a, 'cmp) Set.t Form.t Computation.t

  val map
    :  Source_code_position.t
    -> ?element_group_label:
         (delete_button:Vdom.Node.t -> int -> ('k * 'v) Or_error.t -> Vdom.Node.t)
    -> ?add_element_text:string
    -> ('k, 'cmp) Bonsai.comparator
    -> key:'k Form.t Computation.t
    -> data:'v Form.t Computation.t
    -> ('k, 'v, 'cmp) Map.t Form.t Computation.t
end

module File_select : sig
  (** A form element that allows the user to select a file from their local disk.

      NOTE: these widgets are not safe for use in Tangle as internally they require a
      model which cannot be [of_sexp]'d. *)


  val single_opt
    :  Source_code_position.t
    -> ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?accept:[ `Extension of string | `Mimetype of string ] list
    -> unit
    -> Bonsai_web_ui_file.t option Form.t Computation.t

  (** A form where picking a file is mandatory. The form will be in an error state until a
      file is picked. *)
  val single
    :  Source_code_position.t
    -> ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?accept:[ `Extension of string | `Mimetype of string ] list
    -> unit
    -> Bonsai_web_ui_file.t Form.t Computation.t

  val multiple
    :  Source_code_position.t
    -> ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?accept:[ `Extension of string | `Mimetype of string ] list
    -> unit
    -> Bonsai_web_ui_file.t Filename.Map.t Form.t Computation.t
end

module Freeform_multiselect : sig
  val list
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?placeholder:string
    -> ?split:(string -> string list)
    -> Source_code_position.t
    -> string list Form.t Computation.t

  val set
    :  ?extra_attrs:Vdom.Attr.t list Value.t
    -> ?placeholder:string
    -> ?split:(string -> string list)
    -> Source_code_position.t
    -> String.Set.t Form.t Computation.t
end

module Rank : sig
  val list
    :  ('a, 'b) Bonsai.comparator
    -> ?enable_debug_overlay:bool
    -> ?extra_item_attrs:Vdom.Attr.t Value.t
    -> ?left:Css_gen.Length.t
    -> ?right:Css_gen.Length.t
    -> ?empty_list_placeholder:Vdom.Node.t Value.t
    -> ?default_item_height:int
    -> (source:Vdom.Attr.t Value.t -> 'a Value.t -> Vdom.Node.t Computation.t)
    -> 'a list Form.t Computation.t

end
