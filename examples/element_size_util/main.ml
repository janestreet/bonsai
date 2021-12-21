open! Core
open! Bonsai_web

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
  let open Bonsai.Let_syntax in
  let%sub state =
    Bonsai_web_ui_element_size_hooks.Bulk_size_tracker.component (module Int) Prune_stale
  in
  return
  @@ let%pattern_map sizes, size_attr = state in
  let open Vdom in
  let mk i =
    let key = sprintf "resizable-using-css-%d" i in
    let attr = Vdom.Attr.many [ Attr.class_ "resizable-using-css"; size_attr i ] in
    Node.div ~key ~attr []
  in
  Node.div
    [ Node.h3 [ Node.text "Resize me!" ]
    ; sizes
      |> [%sexp_of:
        Bonsai_web_ui_element_size_hooks.Bulk_size_tracker.Dimensions.t Map.M(Int).t]
      |> Sexp.to_string_hum
      |> Vdom.Node.text
      |> List.return
      |> Node.pre
    ; mk 0
    ; mk 1
    ; mk 2
    ]
;;

let size_component =
  let open Bonsai.Let_syntax in
  let%sub state = Bonsai.state_opt [%here] (module Size) in
  return
  @@ let%pattern_map size, inject_size = state in
  let open Vdom in
  Node.div
    [ Node.h3 [ Node.text "Resize me!" ]
    ; Node.div
        ~key:"resizable-using-css"
        ~attr:
          (Vdom.Attr.many
             [ Attr.class_ "resizable-using-css"
             ; Bonsai_web_ui_element_size_hooks.Size_tracker.on_change
                 (fun ~width ~height -> inject_size (Some Size.{ width; height }))
             ])
        [ Node.textf !"%{sexp:Size.t option}" size ]
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
  let open Bonsai.Let_syntax in
  let%sub pos_x = Bonsai.state [%here] (module Int) ~default_model:0 in
  let%sub pos_y = Bonsai.state [%here] (module Int) ~default_model:0 in
  return
  @@ let%pattern_map pos_x, inject_pos_x = pos_x
  and pos_y, inject_pos_y = pos_y in
  let pos_to_color pos = float_of_int pos /. 2000. *. 256. |> Float.iround_down_exn in
  let open Vdom in
  Node.div
    [ Node.h3 [ Node.text "Scroll me!" ]
    ; Node.div
        ~attr:(Attr.class_ "visibility-parent")
        [ Node.div
            ~attr:
              (Vdom.Attr.many
                 [ Attr.class_ "visibility-child"
                 ; Attr.style
                     (let r = pos_to_color pos_x in
                      let g = pos_to_color pos_y in
                      Css_gen.background_color
                        (`RGBA (Css_gen.Color.RGBA.create ~r ~g ~b:0 ())))
                 ; Bonsai_web_ui_element_size_hooks.Visibility_tracker.on_change
                     (fun
                       (bounds :
                          Bonsai_web_ui_element_size_hooks.Visibility_tracker.Bounds.t)
                       ->
                         Ui_effect.Many
                           [ inject_pos_x bounds.min_x; inject_pos_y bounds.min_y ])
                 ])
            []
        ]
    ]
;;

let buttons
      (type a)
      (module E : Bonsai.Enum with type t = a)
      (current : a)
      (inject : a -> unit Vdom.Effect.t)
  =
  List.map E.all ~f:(fun v ->
    let extra_attrs =
      if E.equal v current then [ Vdom.Attr.class_ "primary" ] else []
    in
    Vdom_input_widgets.Button.simple
      ~extra_attrs
      ~on_click:(fun () -> inject v)
      (E.sexp_of_t v |> Sexp.to_string))
  |> Vdom.Node.div
;;

let resizer_component =
  Bonsai.const
  @@
  let open Vdom in
  Node.div
    [ Node.h3 [ Node.text "Resize me!" ]
    ; Node.div
        ~attr:(Attr.class_ "resizable-using-resizer")
        [ Node.text (String.concat (List.init 20 ~f:(Fn.const "Hello world. ")))
        ; Node.div
            ~attr:
              (Attr.many
                 [ Attr.class_ "resizer"
                 ; Bonsai_web_ui_element_size_hooks.Expert.Resizer.attr
                 ])
            []
        ]
    ]
;;

let component =
  let open Bonsai.Let_syntax in
  let%sub page = Bonsai.state [%here] (module Page) ~default_model:Bulk_size in
  let%pattern_bind page, inject_page = page in
  let%sub page_component =
    Bonsai.enum
      (module Page)
      ~match_:page
      ~with_:(function
        | Bulk_size -> bulk_size_component
        | Size -> size_component
        | Visibility -> visibility_component
        | Resizer -> resizer_component
        | Fit -> fit)
  in
  return
  @@ let%map page_component = page_component
  and page = page
  and inject_page = inject_page in
  let buttons = buttons (module Page) page inject_page in
  Vdom.Node.div [ buttons; page_component ]
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
