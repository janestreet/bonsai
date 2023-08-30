open! Core
open! Import
module Constants = Constants
module Fg_bg = Constants.Fg_bg
module Intent = Constants.Intent
module Card_title_kind = Constants.Card_title_kind
module Font_style = Constants.Font_style
module Font_size = Constants.Font_size
module Table = Table
include Layout

let primary_colors ((module T) : Theme.t) = T.singleton#constants.primary
let extreme_colors ((module T) : Theme.t) = T.singleton#constants.extreme

let extreme_primary_border_color ((module T) : Theme.t) =
  T.singleton#constants.extreme_primary_border
;;

let intent_colors ((module T) : Theme.t) (intent : Intent.t) =
  let { Intent.info; success; warning; error } = T.singleton#constants.intent in
  match intent with
  | Info -> info
  | Success -> success
  | Warning -> warning
  | Error -> error
;;

let button
  ((module T) : Theme.t)
  ?(attrs = [])
  ?(disabled = false)
  ?intent
  ?tooltip
  ~on_click
  text
  =
  T.singleton#button ~attrs ~disabled ~intent ~tooltip ~on_click [ Vdom.Node.text text ]
;;

let button'
  ((module T) : Theme.t)
  ?(attrs = [])
  ?(disabled = false)
  ?intent
  ?tooltip
  ~on_click
  content
  =
  T.singleton#button ~attrs ~disabled ~intent ~tooltip ~on_click content
;;

let tabs
  ((module T) : Theme.t)
  ?(attrs = [])
  ?(per_tab_attrs = fun _ ~is_active:_ -> [])
  ~equal
  ~on_change
  ~active
  tabs
  =
  T.singleton#tabs ~attrs ~per_tab_attrs ~on_change ~equal ~active tabs
;;

module type Enum = sig
  type t [@@deriving enumerate, equal, sexp_of]
end

let tabs_enum
  (type a)
  ((module T) : Theme.t)
  ?(attrs = [])
  ?(per_tab_attrs = fun _ ~is_active:_ -> [])
  ?tab_to_vdom
  (module A : Enum with type t = a)
  ~on_change
  ~active
  =
  let tab_to_vdom =
    Option.value tab_to_vdom ~default:(fun tab ->
      Vdom.Node.text (T.singleton#humanize_sexp (A.sexp_of_t tab)))
  in
  let tabs = List.map A.all ~f:(fun tab -> tab, tab_to_vdom tab) in
  T.singleton#tabs ~attrs ~per_tab_attrs ~on_change ~equal:A.equal ~active tabs
;;

let devbar ((module T) : Theme.t) ?(attrs = []) ?(count = 100) ?intent text =
  T.singleton#devbar ~attrs ~count ~intent text
;;

let constants ((module T) : Theme.t) = T.singleton#constants
let text ?attrs s = Vdom.Node.span ?attrs [ Vdom.Node.text s ]
let textf ?attrs format = Printf.ksprintf (text ?attrs) format

let themed_text ((module T) : Theme.t) ?(attrs = []) ?intent ?style ?size text =
  T.singleton#themed_text ~attrs ~intent ~style ~size text
;;

let themed_textf theme ?attrs ?intent ?style ?size format =
  Printf.ksprintf (themed_text theme ?attrs ?intent ?style ?size) format
;;

module Tooltip_direction = Tooltip.Direction

let tooltip'
  ((module T) : Theme.t)
  ?(container_attrs = [])
  ?(tooltip_attrs = [])
  ?(direction = Tooltip.Direction.Top)
  ~tooltip
  tipped
  =
  T.singleton#tooltip ~container_attrs ~tooltip_attrs ~direction ~tipped ~tooltip
;;

let tooltip theme ?container_attrs ?tooltip_attrs ?direction ~tooltip tipped =
  let tipped = Vdom.Node.text tipped in
  let tooltip = Vdom.Node.text tooltip in
  tooltip' theme ?container_attrs ?tooltip_attrs ?direction ~tooltip tipped
;;

let card'
  ((module T) : Theme.t)
  ?(container_attrs = [])
  ?(title_attrs = [])
  ?(content_attrs = [])
  ?intent
  ?(title = [])
  ?(title_kind = Card_title_kind.Prominent)
  ?(on_click = Effect.Ignore)
  content
  =
  T.singleton#card
    ~container_attrs
    ~title_attrs
    ~content_attrs
    ~intent
    ~on_click
    ~title
    ~title_kind
    ~content
;;

let card
  theme
  ?container_attrs
  ?title_attrs
  ?content_attrs
  ?intent
  ?title
  ?title_kind
  ?on_click
  content
  =
  card'
    theme
    ?container_attrs
    ?title_attrs
    ?content_attrs
    ?intent
    ?title:(Option.map title ~f:(fun title -> [ Vdom.Node.text title ]))
    ?title_kind
    ?on_click
    [ Vdom.Node.text content ]
;;

module App = struct
  let top_attr ((module T) : Theme.t) = T.singleton#app_attr
end

let theme_dyn_var =
  Bonsai.Dynamic_scope.create ~name:"web-ui theme" ~fallback:Expert.default_theme ()
;;

let current_theme = Bonsai.Dynamic_scope.lookup theme_dyn_var

module For_components = struct
  module Codemirror = struct
    let theme ((module T) : Theme.t) = T.singleton#codemirror_theme
  end

  module Forms = struct
    let to_vdom ((module T) : Theme.t) ?on_submit ?(editable = `Yes_always) =
      T.singleton#form_to_vdom ?on_submit ~eval_context:(Form_context.default ~editable)
    ;;

    let to_vdom_plain ((module T) : Theme.t) ?(editable = `Yes_always) =
      Form.to_vdom_plain T.singleton ~eval_context:(Form_context.default ~editable)
    ;;

    let view_error ((module T) : Theme.t) = T.singleton#form_view_error

    let append_item ((module T) : Theme.t) ?(editable = `Yes_always) =
      T.singleton#form_append_item ~eval_context:(Form_context.default ~editable)
    ;;

    let remove_item ((module T) : Theme.t) ?(editable = `Yes_always) =
      T.singleton#form_remove_item ~eval_context:(Form_context.default ~editable)
    ;;
  end
end

module Expert = struct
  open Bonsai.Let_syntax
  include Expert

  let set_theme_for_computation theme inside =
    Bonsai.Dynamic_scope.set theme_dyn_var theme ~inside
  ;;

  let override_theme_for_computation ~f inside =
    let%sub current_theme = current_theme in
    let%sub new_theme =
      let%arr current_theme = current_theme in
      override_theme current_theme ~f
    in
    set_theme_for_computation new_theme inside
  ;;

  let override_constants = Theme.override_constants

  module For_codemirror = For_codemirror
  module Form_context = Form_context
end

module Theme = struct
  open Bonsai.Let_syntax

  type t = Theme.t

  let name = Theme.name
  let current = current_theme
  let set_for_computation theme inside = Expert.set_theme_for_computation theme inside

  let rec with_attr attrs (vdom : Vdom.Node.t) =
    match vdom with
    | None -> Vdom.Node.div ~attrs []
    | Fragment children -> Vdom.Node.div ~attrs children
    | Text _ -> Vdom.Node.span ~attrs [ vdom ]
    | Element e ->
      Element
        (Vdom.Node.Element.map_attrs e ~f:(fun xs -> Vdom.Attr.many (attrs @ [ xs ])))
    | Widget _ -> Vdom.Node.div ~attrs [ vdom ]
    | Lazy { key; t } -> Lazy { key; t = Lazy.map t ~f:(with_attr attrs) }
  ;;

  let set_for_app theme app =
    let%sub app_vdom = set_for_computation theme app in
    let%arr app_vdom = app_vdom
    and theme = theme in
    with_attr [ App.top_attr theme ] app_vdom
  ;;

  let set_for_app' theme app =
    let%sub result_and_vdom = set_for_computation theme app in
    let%arr result, app_vdom = result_and_vdom
    and theme = theme in
    result, with_attr [ App.top_attr theme ] app_vdom
  ;;

  let override_constants_for_computation ~f inside =
    let%sub current_theme = current_theme in
    let%sub new_theme =
      let%arr current_theme = current_theme in
      Theme.override_constants current_theme ~f
    in
    Expert.set_theme_for_computation new_theme inside
  ;;
end

module Raw = struct
  module Table = Table.Raw
end
