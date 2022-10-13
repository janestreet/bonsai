open! Core
open! Async_kernel
open! Import
open Js_of_ocaml
include Virtual_dom.Vdom.Effect

module Deferred_fun_arg = struct
  module Action = struct
    type 'r t = T : 'a * ('a -> 'r Deferred.t) -> 'r t
  end

  let handle (Action.T (a, f)) ~on_response =
    don't_wait_for
      (let%map.Deferred result = f a in
       on_response result)
  ;;
end

module Deferred_fun = Ui_effect.Define1 (Deferred_fun_arg)

let of_deferred_fun f a = Deferred_fun.inject (T (a, f))

let focus_handle =
  let focus_fun path =
    let element =
      Dom_html.document##querySelector (Js.string [%string "[data-focus-handle=%{path}]"])
    in
    match Js.Opt.to_option element with
    | None -> ()
    | Some element -> element##focus
  in
  let focus_effect = of_sync_fun focus_fun in
  let open Bonsai.Let_syntax in
  fun ~name_for_testing ->
    match Util.am_running_how with
    | `Node_test | `Node_benchmark | `Node ->
      let print_effect = print_s [%message "focus effect for" name_for_testing] in
      Bonsai.const (Vdom.Attr.empty, print_effect)
    | `Browser | `Browser_benchmark ->
      let%sub path = Bonsai.path_id in
      let%arr path = path in
      let attr = Vdom.Attr.create "data-focus-handle" path in
      attr, focus_effect path
;;
