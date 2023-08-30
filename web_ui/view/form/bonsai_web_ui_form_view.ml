open! Core
open Import

module Style =
[%css
stylesheet
  {|
        .clear_fieldset_styles {
          border: 0;
          margin: 0;
          padding: 0;
        }
      |}]

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
  { tooltip : (Vdom.Node.t option[@sexp.opaque])
  ; error : Error.t option
  ; label : (Vdom.Node.t option[@sexp.opaque])
  }
[@@deriving sexp_of]

type append_item =
  | Append_info of
      { append : (unit Effect.t[@sexp.opaque])
      ; text : string option
      }
  | Append_view of (Vdom.Node.t[@sexp.opaque])
[@@deriving sexp_of]

type remove_item =
  | Remove_info of
      { remove : (unit Effect.t[@sexp.opaque])
      ; element_label :
          ((delete_button:Vdom.Node.t -> int -> Vdom.Node.t) option[@sexp.opaque])
      }
  | Remove_view of (Vdom.Node.t[@sexp.opaque])
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
  { unique_key : string
  ; raw_view : (context -> editable:editable -> Vdom.Node.t[@sexp.opaque])
  }

and collapsible =
  { collapse_label : (Vdom.Node.t[@sexp.opaque])
  ; state : collapsed_state
  }

and collapsed_state =
  | Collapsed of t option
  | Expanded of t

and field =
  { field_name : string
  ; field_view : t
  }

and variant =
  { clause_selector : (Vdom.Node.t[@sexp.opaque])
  ; selected_clause : clause option
  }

and clause =
  { clause_name : string
  ; clause_view : t
  }

and list_view =
  { list_items : list_item list
  ; append_item : append_item
  ; legacy_button_position : [ `Inline | `Indented ]
  }

and list_item =
  { item_view : t
  ; remove_item : remove_item
  }

and option_view =
  { toggle : (Vdom.Node.t[@sexp.opaque])
  ; status : option_status
  }

and option_status =
  | Currently_some of t
  | Currently_none of t option
[@@deriving sexp_of]

let empty_context = { tooltip = None; error = None; label = None }
let wrap_view view = { context = empty_context; view }
let of_vdom' ~unique_key raw_view = wrap_view (Raw { unique_key; raw_view })
let of_vdom ~unique_key vdom = of_vdom' ~unique_key (fun _context ~editable:_ -> vdom)
let empty = wrap_view Empty
let tuple ts = wrap_view (Tuple ts)
let record fields = wrap_view (Record fields)
let collapsible ~label ~state = wrap_view (Collapsible { collapse_label = label; state })

let variant ~clause_selector ~selected_clause =
  wrap_view (Variant { clause_selector; selected_clause })
;;

let list ~append_item ~legacy_button_position list_items =
  wrap_view (List { list_items; append_item; legacy_button_position })
;;

let option ~toggle ~status = wrap_view (Option { toggle; status })
let list_item ~view ~remove_item = { item_view = view; remove_item }

let suggest_error error t =
  match t.context.error with
  | None -> { t with context = { t.context with error = Some error } }
  | Some _ -> t
;;

let suggest_label' label t =
  match t.context.label with
  | None -> { t with context = { t.context with label = Some label } }
  | Some _ -> t
;;

let suggest_label label t = suggest_label' (Vdom.Node.text label) t
let set_label label t = { t with context = { t.context with label = Some label } }
let set_tooltip tooltip t = { t with context = { t.context with tooltip = Some tooltip } }

(* For rendering *)

let with_fieldset ~currently_editable view =
  let disabled = if currently_editable then Vdom.Attr.empty else Vdom.Attr.disabled in
  Vdom.Node.fieldset ~attrs:[ disabled; Style.clear_fieldset_styles ] [ view ]
;;
