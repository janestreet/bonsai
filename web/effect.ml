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
let of_deferred_thunk f = of_deferred_fun f ()

module Focus = struct
  type nonrec t =
    { attr : Vdom.Attr.t
    ; focus : unit t
    ; blur : unit t
    }

  let on_effect =
    let control_focus ~on_element path =
      let element =
        Dom_html.document##querySelector
          (Js.string [%string "[data-focus-handle=%{path}]"])
      in
      match Js.Opt.to_option element with
      | None -> ()
      | Some element -> on_element element
    in
    let focus_effect =
      of_sync_fun (control_focus ~on_element:(fun element -> element##focus))
    in
    let blur_effect =
      of_sync_fun (control_focus ~on_element:(fun element -> element##blur))
    in
    let open Bonsai.Let_syntax in
    fun ?(name_for_testing = "element") () ->
      match Util.am_running_how with
      | `Node_test | `Node_benchmark | `Node ->
        let print_effect_focus = print_s [%message "focus effect for" name_for_testing] in
        let print_effect_blur = print_s [%message "blur effect for" name_for_testing] in
        Bonsai.const
          { attr = Vdom.Attr.empty; focus = print_effect_focus; blur = print_effect_blur }
      | `Browser | `Browser_benchmark ->
        let%sub path = Bonsai.path_id in
        let%arr path = path in
        let attr = Vdom.Attr.create "data-focus-handle" path in
        { attr; focus = focus_effect path; blur = blur_effect path }
  ;;

  let on_activate ?name_for_testing () =
    let open Bonsai.Let_syntax in
    let%sub { attr; focus; blur = _ } = on_effect ?name_for_testing () in
    let%sub () = Bonsai.Edge.lifecycle ~on_activate:focus () in
    return attr
  ;;
end

let reload_page =
  of_thunk (fun () ->
    match Util.am_running_how with
    | `Browser -> Dom_html.window##.location##reload
    | `Node_test | `Node | `Node_benchmark | `Browser_benchmark ->
      Core.print_s [%message "Reloading page skipped in test"])
;;
