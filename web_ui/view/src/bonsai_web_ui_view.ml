open! Core
open! Import
module Constants = Constants
module Fg_bg = Constants.Fg_bg
module Intent = Constants.Intent
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
      ?(attr = Vdom.Attr.empty)
      ?(disabled = false)
      ?intent
      ?tooltip
      ~on_click
      text
  =
  T.singleton#button ~attr ~disabled ~intent ~tooltip ~on_click [ Vdom.Node.text text ]
;;

let button'
      ((module T) : Theme.t)
      ?(attr = Vdom.Attr.empty)
      ?(disabled = false)
      ?intent
      ?tooltip
      ~on_click
      content
  =
  T.singleton#button ~attr ~disabled ~intent ~tooltip ~on_click content
;;

let tabs
      ((module T) : Theme.t)
      ?(attr = Vdom.Attr.empty)
      ?(per_tab_attr = fun _ ~is_active:_ -> Vdom.Attr.empty)
      ~equal
      ~on_change
      ~active
      tabs
  =
  T.singleton#tabs ~attr ~per_tab_attr ~on_change ~equal ~active tabs
;;

module type Enum = sig
  type t [@@deriving enumerate, equal, sexp_of]
end

let tabs_enum
      (type a)
      ((module T) : Theme.t)
      ?(attr = Vdom.Attr.empty)
      ?(per_tab_attr = fun _ ~is_active:_ -> Vdom.Attr.empty)
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
  T.singleton#tabs ~attr ~per_tab_attr ~on_change ~equal:A.equal ~active tabs
;;

let devbar ((module T) : Theme.t) ?(attr = Vdom.Attr.empty) ?(count = 100) ?intent text =
  T.singleton#devbar ~attr ~count ~intent text
;;

let text ?attr s = Vdom.Node.span ?attr [ Vdom.Node.text s ]
let textf ?attr format = Printf.ksprintf (text ?attr) format

let themed_text ((module T) : Theme.t) ?(attr = Vdom.Attr.empty) ?intent text =
  T.singleton#themed_text ~attr ~intent text
;;

let themed_textf theme ?attr ?intent format =
  Printf.ksprintf (themed_text theme ?attr ?intent) format
;;

module Tooltip_direction = Tooltip.Direction

let tooltip'
      ((module T) : Theme.t)
      ?(container_attr = Vdom.Attr.empty)
      ?(tooltip_attr = Vdom.Attr.empty)
      ?(direction = Tooltip.Direction.Top)
      ~tooltip
      tipped
  =
  T.singleton#tooltip ~container_attr ~tooltip_attr ~direction ~tipped ~tooltip
;;

let tooltip theme ?container_attr ?tooltip_attr ?direction ~tooltip tipped =
  let tipped = Vdom.Node.text tipped in
  let tooltip = Vdom.Node.text tooltip in
  tooltip' theme ?container_attr ?tooltip_attr ?direction ~tooltip tipped
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

  module For_codemirror = For_codemirror
end

module Theme = struct
  open Bonsai.Let_syntax

  type t = Theme.t

  let name = Theme.name
  let current = current_theme
  let set_for_computation theme inside = Expert.set_theme_for_computation theme inside

  let rec with_attr attr (vdom : Vdom.Node.t) =
    match vdom with
    | None -> Vdom.Node.div ~attr []
    | Text _ -> Vdom.Node.span ~attr [ vdom ]
    | Element e ->
      Element (Vdom.Node.Element.map_attrs e ~f:(fun xs -> Vdom.Attr.many [ attr; xs ]))
    | Widget _ -> Vdom.Node.div ~attr [ vdom ]
    | Lazy { key; t } -> Lazy { key; t = Lazy.map t ~f:(with_attr attr) }
  ;;

  let set_for_app theme app =
    let%sub app_vdom = set_for_computation theme app in
    let%arr app_vdom = app_vdom
    and theme = theme in
    with_attr (App.top_attr theme) app_vdom
  ;;

  let set_for_app' theme app =
    let%sub result_and_vdom = set_for_computation theme app in
    let%arr result, app_vdom = result_and_vdom
    and theme = theme in
    result, with_attr (App.top_attr theme) app_vdom
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
