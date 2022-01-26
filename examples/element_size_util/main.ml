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

let size_component =
  let%sub state = Bonsai.state_opt [%here] (module Size) in
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
  let%sub pos_x = Bonsai.state [%here] (module Int) ~default_model:0 in
  let%sub pos_y = Bonsai.state [%here] (module Int) ~default_model:0 in
  let%arr pos_x, inject_pos_x = pos_x
  and pos_y, inject_pos_y = pos_y in
  let pos_to_color pos = float_of_int pos /. 2000. *. 256. |> Float.iround_down_exn in
  let attributes =
    [ Vdom.Attr.class_ Style.visibility_child
    ; Vdom.Attr.style
        (let r = pos_to_color pos_x in
         let g = pos_to_color pos_y in
         Css_gen.background_color (`RGBA (Css_gen.Color.RGBA.create ~r ~g ~b:0 ())))
    ; Size_hooks.Visibility_tracker.on_change (fun bounds ->
        Ui_effect.Many [ inject_pos_x bounds.min_x; inject_pos_y bounds.min_y ])
    ]
  in
  Vdom.Node.div
    [ Vdom.Node.h3 [ Vdom.Node.text "Scroll me!" ]
    ; Vdom.Node.div
        ~attr:(Vdom.Attr.class_ Style.visibility_parent)
        [ Vdom.Node.div ~attr:(Vdom.Attr.many attributes) [] ]
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
  let%sub page, inject_page =
    Bonsai.state [%here] (module Page) ~default_model:Bulk_size
  in
  let%sub page_component =
    match%sub page with
    | Bulk_size -> bulk_size_component
    | Size -> size_component
    | Visibility -> visibility_component
    | Resizer -> resizer_component
    | Fit -> fit
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
