open! Core
open! Import

type 'a render =
  eval_context:Form_context.t -> view_context:Form_view.context -> 'a -> Vdom.Node.t list

module C = struct
  class type t = object
    method theme_name : string
    method constants : Constants.t

    method button :
      attrs:Vdom.Attr.t list
      -> disabled:bool
      -> intent:Constants.Intent.t option
      -> tooltip:string option
      -> on_click:unit Effect.t
      -> Vdom.Node.t list
      -> Vdom.Node.t

    method badge :
      attrs:Vdom.Attr.t list
      -> intent:Constants.Intent.t option
      -> on_dismiss:unit Effect.t option
      -> Vdom.Node.t list
      -> Vdom.Node.t

    method tabs :
      attrs:Vdom.Attr.t list
      -> per_tab_attrs:('a -> is_active:bool -> Vdom.Attr.t list)
      -> on_change:(from:'a -> to_:'a -> unit Effect.t)
      -> equal:('a -> 'a -> bool)
      -> active:'a
      -> ('a * Vdom.Node.t) list
      -> Vdom.Node.t

    method devbar :
      attrs:Vdom.Attr.t list
      -> count:int
      -> intent:Constants.Intent.t option
      -> string
      -> Vdom.Node.t

    method humanize_sexp : Sexp.t -> string
    method use_intent_fg_or_bg_for_highlighting : [ `Fg | `Bg ]

    method themed_text :
      attrs:Vdom.Attr.t list
      -> intent:Constants.Intent.t option
      -> style:Constants.Font_style.t option
      -> size:Constants.Font_size.t option
      -> string
      -> Vdom.Node.t

    method tooltip :
      container_attrs:Vdom.Attr.t list
      -> tooltip_attrs:Vdom.Attr.t list
      -> direction:Tooltip.Direction.t
      -> tipped:Vdom.Node.t
      -> tooltip:Vdom.Node.t
      -> Vdom.Node.t

    method app_attr : Vdom.Attr.t Lazy.t
    method codemirror_theme : For_codemirror.Theme.t option
    method prt_styling : For_prt.t

    (* tables *)
    method table : Vdom.Attr.t
    method table_header : Vdom.Attr.t
    method table_header_row : Vdom.Attr.t
    method table_header_cell : Vdom.Attr.t
    method table_body : Vdom.Attr.t
    method table_body_row : Vdom.Attr.t
    method table_body_cell : Vdom.Attr.t
    method table_body_cell_empty : Vdom.Attr.t

    (* misc forms *)
    method form_view_error : Error.t -> Vdom.Node.t list
    method form_view_error_details : Error.t -> Vdom.Node.t
    method form_view_tooltip : Vdom.Node.t -> Vdom.Node.t

    method form_remove_item :
      eval_context:Form_context.t -> Form_view.remove_item -> index:int -> Vdom.Node.t

    method form_append_item :
      eval_context:Form_context.t -> Form_view.append_item -> Vdom.Node.t

    (* form constructors *)
    method form_empty :
      eval_context:Form_context.t
      -> view_context:Form_view.context
      -> unit
      -> Vdom.Node.t list

    method form_collapsible :
      eval_context:Form_context.t
      -> view_context:Form_view.context
      -> Form_view.collapsible
      -> Vdom.Node.t list

    method form_raw :
      eval_context:Form_context.t
      -> view_context:Form_view.context
      -> Form_view.raw
      -> Vdom.Node.t list

    method form_record :
      eval_context:Form_context.t
      -> view_context:Form_view.context
      -> Form_view.field list
      -> Vdom.Node.t list

    method form_variant :
      eval_context:Form_context.t
      -> view_context:Form_view.context
      -> Form_view.variant
      -> Vdom.Node.t list

    method form_tuple :
      eval_context:Form_context.t
      -> view_context:Form_view.context
      -> Form_view.t list
      -> Vdom.Node.t list

    method form_option :
      eval_context:Form_context.t
      -> view_context:Form_view.context
      -> Form_view.option_view
      -> Vdom.Node.t list

    method form_list :
      eval_context:Form_context.t
      -> view_context:Form_view.context
      -> Form_view.list_view
      -> Vdom.Node.t list

    method form_view : eval_context:Form_context.t -> Form_view.t -> Vdom.Node.t list
    method form_toplevel_combine : Vdom.Node.t list -> Vdom.Node.t

    (* forms *)
    method form_to_vdom :
      ?on_submit:Form_view.submission_options
      -> eval_context:Form_context.t
      -> Form_view.t
      -> Vdom.Node.t

    (* card component *)
    method card :
      container_attrs:Vdom.Attr.t list
      -> title_attrs:Vdom.Attr.t list
      -> content_attrs:Vdom.Attr.t list
      -> intent:Constants.Intent.t option
      -> on_click:unit Effect.t
      -> title:Vdom.Node.t list
      -> title_kind:Constants.Card_title_kind.t
      -> content:Vdom.Node.t list
      -> Vdom.Node.t

    (* input elements *)
    method textbox :
      ?attrs:Vdom.Attr.t list
      -> ?placeholder:string
      -> ?key:string
      -> allow_updates_when_focused:[ `Always | `Never ]
      -> disabled:bool
      -> value:string
      -> set_value:(string -> unit Effect.t)
      -> unit
      -> Vdom.Node.t

    method password :
      ?attrs:Vdom.Attr.t list
      -> ?placeholder:string
      -> ?key:string
      -> allow_updates_when_focused:[ `Always | `Never ]
      -> disabled:bool
      -> value:string
      -> set_value:(string -> unit Effect.t)
      -> unit
      -> Vdom.Node.t

    method textarea :
      ?attrs:Vdom.Attr.t list
      -> ?placeholder:string
      -> ?key:string
      -> allow_updates_when_focused:[ `Always | `Never ]
      -> disabled:bool
      -> value:string
      -> set_value:(string -> unit Effect.t)
      -> unit
      -> Vdom.Node.t

    method number :
      ?attrs:Vdom.Attr.t list
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

    method range :
      ?attrs:Vdom.Attr.t list
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
end

module type S = sig
  class c : C.t
end

type t = (module S)
