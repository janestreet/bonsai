open! Core
open! Import
open! Js_of_ocaml
open! Virtual_dom

module Expert = struct
  let set_width element =
    let rect = element##getBoundingClientRect in
    let width = Js.Optdef.to_option rect##.width in
    Option.iter width ~f:(set_width element)
  ;;

  let set_height element =
    let rect = element##getBoundingClientRect in
    let height = Js.Optdef.to_option rect##.height in
    Option.iter height ~f:(set_height element)
  ;;

  let reset_width = reset_width
  let reset_height = reset_height
end

module T = struct
  module Input = struct
    type t =
      { set : Dom_html.element Js.t -> unit
      ; reset : Dom_html.element Js.t -> unit
      }
    [@@deriving sexp]

    let combine left right =
      { set =
          (fun element ->
            left.set element;
            right.set element)
      ; reset =
          (fun element ->
            left.reset element;
            right.reset element)
      }
    ;;
  end

  module State = Unit

  let init _input _element = ()
  let on_mount { Input.set; _ } () element = set element
  let on_mount = `Schedule_animation_frame on_mount
  let update ~old_input:_ ~new_input:_ () _element = ()
  let destroy { Input.reset; _ } () element = reset element
end

module Hook = Vdom.Attr.Hooks.Make (T)

let on_mount ~name ~set ~reset =
  Vdom.Attr.create_hook name (Hook.create { T.Input.set; reset })
;;

let width = on_mount ~name:"freeze_width" ~set:Expert.set_width ~reset:reset_width
let height = on_mount ~name:"freeze_height" ~set:Expert.set_height ~reset:reset_height
