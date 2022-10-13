open! Core
open! Import
module Constants = Constants
module Fg_bg = Constants.Fg_bg
module Intent = Constants.Intent
include Layout

let primary_colors ((module T) : Theme.t) = T.singleton#constants.primary

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
      ~on_click
      text
  =
  T.singleton#button ~attr ~disabled ~intent ~on_click [ Vdom.Node.text text ]
;;

let button'
      ((module T) : Theme.t)
      ?(attr = Vdom.Attr.empty)
      ?(disabled = false)
      ?intent
      ~on_click
      content
  =
  T.singleton#button ~attr ~disabled ~intent ~on_click content
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

let devbar
      ((module T) : Theme.t)
      ?(attr = Vdom.Attr.empty)
      ?(count = 100)
      ?(intent = Intent.Error)
      text
  =
  T.singleton#devbar ~attr ~count ~intent text
;;

let text ((module T) : Theme.t) ?(attr = Vdom.Attr.empty) ?intent text =
  T.singleton#text ~attr ~intent text
;;

let theme_dyn_var =
  Bonsai.Dynamic_scope.create ~name:"web-ui theme" ~fallback:Expert.default_theme ()
;;

let current_theme = Bonsai.Dynamic_scope.lookup theme_dyn_var

module Expert = struct
  open Bonsai.Let_syntax
  include Expert

  let set_theme_temporarily theme ~inside =
    Bonsai.Dynamic_scope.set theme_dyn_var theme ~inside
  ;;

  let override_current_theme_temporarily ~f ~inside =
    let%sub current_theme = current_theme in
    let%sub new_theme =
      let%arr current_theme = current_theme in
      override_theme current_theme ~f
    in
    set_theme_temporarily new_theme ~inside
  ;;
end

module Theme = struct
  open Bonsai.Let_syntax

  type t = Theme.t

  let name = Theme.name
  let current = current_theme
  let set_temporarily = Expert.set_theme_temporarily

  let override_constants_temporarily ~f ~inside =
    let%sub current_theme = current_theme in
    let%sub new_theme =
      let%arr current_theme = current_theme in
      Theme.override_constants current_theme ~f
    in
    Expert.set_theme_temporarily new_theme ~inside
  ;;
end
