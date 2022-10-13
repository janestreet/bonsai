open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Size_hooks = Bonsai_web_ui_element_size_hooks

module Page = struct
  type t =
    | Bulk_size
    | Size
    | Visibility
    | Resizer
    | Fit
    | Position
  [@@deriving enumerate, sexp, compare, equal]
end

module Size = struct
  type t =
    { width : float
    ; height : float
    }
  [@@deriving sexp, equal]
end

let bulk_size_component =
  let%sub state = Size_hooks.Bulk_size_tracker.component (module Int) Prune_stale in
  let%arr sizes, size_attr = state in
  let mk i =
    let key = sprintf "resizable-using-css-%d" i in
    let attr =
      Vdom.Attr.many [ Vdom.Attr.class_ Style.resizable_using_css; size_attr i ]
    in
    Vdom.Node.div ~key ~attr []
  in
  Vdom.Node.div
    [ Vdom.Node.h3 [ Vdom.Node.text "Resize me!" ]
    ; sizes
      |> [%sexp_of: Size_hooks.Bulk_size_tracker.Dimensions.t Map.M(Int).t]
      |> Sexp.to_string_hum
      |> Vdom.Node.text
      |> List.return
      |> Vdom.Node.pre ~attr:(Vdom.Attr.class_ Style.pre_for_display)
    ; mk 0
    ; mk 1
    ; mk 2
    ]
;;

let position =
  let%sub { positions; get_attr; update } =
    Size_hooks.Position_tracker.component (module Int)
  in
  let module Model = struct
    type t = Size_hooks.Position_tracker.Position.t Int.Map.t [@@deriving sexp, equal]
  end
  in
  let%sub () =
    Bonsai.Edge.on_change
      (module Model)
      positions
      ~callback:
        (Fn.const ((Effect.of_sync_fun print_endline) "position changed!") |> Value.return)
  in
  let%sub () =
    Bonsai.Clock.every
      ~when_to_start_next_effect:`Every_multiple_of_period_blocking
      (Time_ns.Span.of_sec 2.0)
      update
  in
  let%arr positions = positions
  and get_attr = get_attr in
  let mk i =
    let attr =
      Vdom.Attr.many [ Vdom.Attr.class_ Style.resizable_using_css; get_attr i ]
    in
    Vdom.Node.div ~attr []
  in
  Vdom.Node.div
    [ Vdom.Node.h3 [ Vdom.Node.text "Resize me!" ]
    ; positions
      |> [%sexp_of: Size_hooks.Position_tracker.Position.t Map.M(Int).t]
      |> Sexp.to_string_hum
      |> Vdom.Node.text
      |> List.return
      |> Vdom.Node.pre ~attr:(Vdom.Attr.class_ Style.pre_for_display)
    ; mk 0
    ; mk 1
    ; mk 2
    ; mk 2
    ]
;;

let size_component =
  let%sub state = Bonsai.state_opt (module Size) in
  let%arr size, inject_size = state in
  Vdom.Node.div
    [ Vdom.Node.h3 [ Vdom.Node.text "Resize me!" ]
    ; Vdom.Node.div
        ~key:"resizable-using-css"
        ~attr:
          (Vdom.Attr.many
             [ Vdom.Attr.class_ Style.resizable_using_css
             ; Size_hooks.Size_tracker.on_change (fun ~width ~height ->
                 inject_size (Some Size.{ width; height }))
             ])
        [ Vdom.Node.textf !"%{sexp:Size.t option}" size ]
    ]
;;

let fit =
  let open Vdom in
  let make s behavior =
    Node.div
      [ Node.h2 [ Node.text s ]
      ; Node.div
          ~key:"resizable-using-css"
          ~attr:
            (Vdom.Attr.many
               [ Attr.class_ "resizable-using-css"
               ; Bonsai_web_ui_element_size_hooks.Resize_to_fit
                 .attr_for_parent__recommended
               ])
          [ Node.span
              ~attr:
                (Vdom.Attr.many
                   [ Bonsai_web_ui_element_size_hooks.Resize_to_fit.attr ~behavior ()
                   ; Vdom.Attr.create "contenteditable" ""
                   ; Vdom.Attr.style (Css_gen.outline ~style:`None ())
                   ])
              [ Node.text "hello world" ]
          ]
      ]
  in
  Bonsai.const
    (Node.div
       [ make "shrink to avoid overflow" Shrink_to_avoid_overflow
       ; make "grow to fill" Grow_to_fill
       ; make "grow or shrink to match parent size" Grow_or_shrink_to_match_parent_size
       ])
;;

let visibility_component =
  let open Size_hooks.Visibility_tracker in
  let%sub pos_x = Bonsai.state (module Int) ~default_model:0 in
  let%sub pos_y = Bonsai.state (module Int) ~default_model:0 in
  let%sub client_rect, set_client_rect = Bonsai.state_opt (module Bbox.Int) in
  let%sub visible_rect, set_visible_rect = Bonsai.state_opt (module Bbox.Int) in
  let%arr pos_x, inject_pos_x = pos_x
  and pos_y, inject_pos_y = pos_y
  and client_rect = client_rect
  and set_visible_rect = set_visible_rect
  and set_client_rect = set_client_rect
  and visible_rect = visible_rect in
  let pos_to_color pos = float_of_int pos /. 2000. *. 360. |> Float.iround_down_exn in
  let attributes =
    [ Vdom.Attr.class_ Style.visibility_child
    ; Vdom.Attr.style
        (let h = pos_to_color pos_y in
         let s = Percent.of_mult (1.0 -. (float_of_int pos_x /. 2000.)) in
         Css_gen.background_color
           (`HSLA (Css_gen.Color.HSLA.create ~h ~s ~l:(Percent.of_mult 0.5) ())))
    ; Size_hooks.Visibility_tracker.detect
        ()
        ~visible_rect_changed:(fun bounds ->
          Effect.Many
            [ inject_pos_x bounds.min_x
            ; inject_pos_y bounds.min_y
            ; set_visible_rect (Some bounds)
            ])
        ~client_rect_changed:(Fn.compose set_client_rect Option.some)
    ]
  in
  Vdom.Node.div
    [ Vdom.Node.h3 [ Vdom.Node.text "Scroll me!" ]
    ; Vdom.Node.sexp_for_debugging
        [%message (client_rect : Bbox.Int.t option) (visible_rect : Bbox.Int.t option)]
    ; Vdom.Node.div
        ~attr:(Vdom.Attr.class_ Style.outer_visibility_parent)
        [ Vdom.Node.div
            ~attr:(Vdom.Attr.class_ Style.inner_visibility_parent)
            [ Vdom.Node.div ~attr:(Vdom.Attr.many attributes) [] ]
        ; Vdom.Node.div
            ~attr:(Vdom.Attr.class_ Style.inner_visibility_parent)
            [ Vdom.Node.text "padding..." ]
        ]
    ]
;;

let buttons current inject =
  let make_button_for_tab page =
    let click_handler = Vdom.Attr.on_click (fun _ -> inject page) in
    let attr =
      if Page.equal page current
      then Vdom.Attr.(class_ Style.primary @ click_handler)
      else click_handler
    in
    Vdom.Node.button ~attr [ page |> Page.sexp_of_t |> Sexp.to_string |> Vdom.Node.text ]
  in
  Page.all |> List.map ~f:make_button_for_tab |> Vdom.Node.div
;;

let resizer_component =
  Bonsai.const
    (Vdom.Node.div
       [ Vdom.Node.h3 [ Vdom.Node.text "Resize me!" ]
       ; Vdom.Node.div
           ~attr:(Vdom.Attr.class_ Style.resizable_using_resizer)
           [ Vdom.Node.text (String.concat (List.init 20 ~f:(Fn.const "Hello world. ")))
           ; Vdom.Node.div
               ~attr:
                 (Vdom.Attr.many
                    [ Vdom.Attr.class_ Style.resizer; Size_hooks.Expert.Resizer.attr ])
               []
           ]
       ])
;;

let component =
  let%sub page, inject_page = Bonsai.state (module Page) ~default_model:Bulk_size in
  let%sub page_component =
    match%sub page with
    | Bulk_size -> bulk_size_component
    | Size -> size_component
    | Visibility -> visibility_component
    | Resizer -> resizer_component
    | Fit -> fit
    | Position -> position
  in
  let%arr page_component = page_component
  and page = page
  and inject_page = inject_page in
  let buttons = buttons page inject_page in
  Vdom.Node.div [ buttons; page_component ]
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
