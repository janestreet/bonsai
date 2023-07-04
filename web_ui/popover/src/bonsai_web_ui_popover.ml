open! Core
open Bonsai_web
open Bonsai.Let_syntax
open Js_of_ocaml

module Result = struct
  type t =
    { wrap : Vdom.Node.t -> Vdom.Node.t
    ; open_ : unit Effect.t
    ; close : unit Effect.t
    ; toggle : unit Effect.t
    ; is_open : bool
    }
end

module Direction = struct
  type t =
    | Left
    | Right
    | Down
    | Up
end

module Alignment = struct
  type t =
    | Start
    | Center
    | End
end

let direction_to_attr = function
  | Direction.Down -> Style.bottom
  | Up -> Style.top
  | Left -> Style.left
  | Right -> Style.right
;;

let alignment_to_attr = function
  | Alignment.Start -> Style.align_start
  | Center -> Vdom.Attr.empty
  | End -> Style.align_end
;;

let has_clicked_outside : popover_id:string -> Dom.node Js.t Js.opt -> bool =
  fun ~popover_id element ->
  let rec loop : Dom.node Js.t Js.opt -> bool =
    fun element ->
    match Js.Opt.to_option element with
    | None -> true
    | Some element ->
      (match Dom.nodeType element with
       | Attr _ | Text _ | Other _ -> loop element##.parentNode
       | Element element ->
         (match Js.Opt.to_option (element##getAttribute (Js.string "id")) with
          | None -> loop element##.parentNode
          | Some id ->
            if String.equal (Js.to_string id) popover_id
            then false
            else loop element##.parentNode))
  in
  loop element
;;


let default_popover_styles =
  let%sub theme = View.Theme.current in
  let%arr theme = theme in
  let constants = View.constants theme in
  let vars =
    Style.Variables.set
      ~bg:(Css_gen.Color.to_string_css constants.extreme.background)
      ~fg:(Css_gen.Color.to_string_css constants.extreme.foreground)
      ~border:(Css_gen.Color.to_string_css constants.extreme_primary_border)
      ()
  in
  Vdom.Attr.many [ vars; Style.default_tooltip_styles ]
;;

let component
      ?popover_extra_attr
      ?popover_style_attr
      ?base_extra_attr
      ?(allow_event_propagation_when_clicked_outside :
          ([ `Left_click | `Right_click | `Escape ] -> bool) Value.t =
          Value.return (fun _ -> false))
      ?(on_close = Value.return Effect.Ignore)
      ?(keep_popover_inside_window = Value.return false)
      ~close_when_clicked_outside
      ~direction
      ~alignment
      ~popover
      ()
  =
  let%sub popover_id = Bonsai.path_id in
  let%sub popover_extra_attr =
    Option.value_map popover_extra_attr ~default:(Bonsai.const Vdom.Attr.empty) ~f:return
  in
  let%sub popover_style_attr =
    Option.value_map popover_style_attr ~default:default_popover_styles ~f:return
  in
  let%sub base_extra_attr =
    Option.value_map base_extra_attr ~default:(Bonsai.const Vdom.Attr.empty) ~f:return
  in
  let%sub { state = is_open; set_state = set_is_open; toggle } =
    Bonsai.toggle' ~default_model:false
  in
  let%sub direction_class =
    let%arr direction = direction in
    direction_to_attr direction
  in
  let%sub alignment_class =
    let%arr alignment = alignment in
    alignment_to_attr alignment
  in
  let%sub open_, close =
    let%arr set_is_open = set_is_open
    and on_close = on_close in
    set_is_open true, Effect.Many [ set_is_open false; on_close ]
  in
  let%sub popover =
    match%sub is_open with
    | false -> Bonsai.const Vdom.Node.none
    | true ->
      let%sub outside_click_listener_attr =
        match close_when_clicked_outside with
        | false -> Bonsai.const Vdom.Attr.empty
        | true ->
          let%arr close = close
          and popover_id = popover_id
          and allow_event_propagation_when_clicked_outside =
            allow_event_propagation_when_clicked_outside
          in
          let f ~source event =
            let target = (event##.target :> Dom.node Js.t Js.opt) in
            match has_clicked_outside ~popover_id target with
            | true ->
              let should_block =
                not (allow_event_propagation_when_clicked_outside source)
              in
              (match should_block with
               | false -> close
               | true ->
                 Effect.Many
                   [ close
                   ; Effect.Stop_propagation
                   (* Prevents other listeners/from trigerring their events. *)
                   ; Effect.Prevent_default
                     (* Prevents non-event interactions like context menus from opening
                        and interactions with form elements + clicking on links. *)
                   ])
            | false -> Effect.Ignore
          in
          let handle_if_escape event =
            match Dom_html.Keyboard_code.of_event event with
            | Escape -> f ~source:`Escape event
            | _ -> Effect.Ignore
          in
          Vdom.Attr.many
            [ Vdom.Attr.Global_listeners.click (f ~source:`Left_click)
            ; Vdom.Attr.Global_listeners.contextmenu (f ~source:`Right_click)
            ; Vdom.Attr.Global_listeners.keydown handle_if_escape
            ]
      in
      let%sub popover = popover ~close in
      let%arr popover = popover
      and popover_id = popover_id
      and outside_click_listener_attr = outside_click_listener_attr
      and popover_extra_attr = popover_extra_attr
      and popover_style_attr = popover_style_attr
      and keep_popover_inside_window = keep_popover_inside_window in
      let reposition_hook =
        match keep_popover_inside_window with
        | true -> Vdom.Attr.create_hook "reposition" (Reposition_hook.create ())
        | false -> Vdom.Attr.empty
      in
      Vdom.Node.div
        ~attrs:
          [ Style.tooltip
          ; Vdom.Attr.id popover_id
          ; reposition_hook
          ; popover_style_attr
          ; popover_extra_attr
          ; outside_click_listener_attr
          ]
        [ popover ]
  in
  let%sub open_attr =
    match%arr is_open with
    | false -> Vdom.Attr.empty
    | true -> Style.tooltip_open
  in
  let%sub wrap =
    let%arr popover = popover
    and direction_class = direction_class
    and alignment_class = alignment_class
    and open_attr = open_attr
    and base_extra_attr = base_extra_attr in
    fun popover_base ->
      Vdom.Node.span
        ~attrs:
          [ Style.tooltip_container
          ; open_attr
          ; direction_class
          ; alignment_class
          ; base_extra_attr
          ]
        [ popover_base; popover ]
  in
  let%arr open_ = open_
  and close = close
  and wrap = wrap
  and toggle = toggle
  and is_open = is_open in
  { Result.wrap; open_; close; toggle; is_open }
;;
