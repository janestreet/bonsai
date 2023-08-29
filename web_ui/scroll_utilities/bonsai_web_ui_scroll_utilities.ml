open! Core
open! Bonsai_web
open Js_of_ocaml

let is_browser =
  match Bonsai_web.am_running_how with
  | `Browser | `Browser_benchmark -> true
  | `Node | `Node_benchmark | `Node_test -> false
;;

let query_selector s =
  if is_browser
  then Dom_html.document##querySelector (Js.string s) |> Js.Opt.to_option
  else None
;;

let can't_find_error_message ~selector ~fun_name =
  Error.create_s
    [%message "couldn't find element" (selector : string) "for" (fun_name : string)]
;;

module Scroll_into_view = struct
  let scroll_into_view_options ~align ~smooth =
    let smooth = if smooth then "smooth" else "auto" in
    let open Js_of_ocaml in
    object%js
      val behavior = Js.string smooth

      val block =
        match align with
        | `Top -> Js.string "start"
        | `Bottom -> Js.string "end"
    end
  ;;

  let element_to_scrollable element
    : < scrollIntoViewIfNeeded : bool Js.t -> unit Js.meth
      ; scrollIntoView : Js.Unsafe.any -> unit Js.meth >
        Js.t
    =
    Js.Unsafe.coerce element
  ;;

  let f ~smooth element ~how =
    let scrollable = element_to_scrollable element in
    match how with
    | `Minimal -> scrollable##scrollIntoViewIfNeeded (Js.bool false)
    | `To_top ->
      scrollable##scrollIntoView
        (Js.Unsafe.inject (scroll_into_view_options ~align:`Top ~smooth))
    | `To_bottom ->
      scrollable##scrollIntoView
        (Js.Unsafe.inject (scroll_into_view_options ~align:`Bottom ~smooth))
  ;;

  let for_effect (smooth, selector, how) =
    match query_selector selector with
    | None -> Error (can't_find_error_message ~selector ~fun_name:"into_view")
    | Some element ->
      f ~smooth element ~how;
      Ok ()
  ;;

  let effect = Effect.of_sync_fun for_effect
  let f' ?(smooth = false) ~selector how = effect (smooth, selector, how)
end

module To_position_inside_element = struct
  let f ~smooth ~selector ~x_px ~y_px ~how =
    match query_selector selector with
    | None ->
      Error (can't_find_error_message ~selector ~fun_name:"to_position_inside_element")
    | Some parent ->
      let new_node = Dom_html.createDiv Dom_html.document in
      new_node##.style##.position := Js.string "absolute";
      new_node##.style##.left
      := Js.string (Virtual_dom.Dom_float.to_string_fixed 5 x_px ^ "px");
      new_node##.style##.top
      := Js.string (Virtual_dom.Dom_float.to_string_fixed 5 y_px ^ "px");
      let (_ : Dom.node Js.t) = parent##appendChild (new_node :> Dom.node Js.t) in
      Scroll_into_view.f ~smooth new_node ~how;
      let (_ : _ Js.t) = parent##removeChild (new_node :> Dom.node Js.t) in
      Ok ()
  ;;

  let for_effect (smooth, selector, x_px, y_px, how) =
    f ~smooth ~selector ~x_px ~y_px ~how
  ;;

  let effect = Effect.of_sync_fun for_effect

  let f ?(smooth = false) ~selector ~x_px ~y_px how =
    effect (smooth, selector, x_px, y_px, how)
  ;;
end

let to_position_inside_element = To_position_inside_element.f
let into_view = Scroll_into_view.f'
