open! Core
open Bonsai_web
module Attr = Vdom.Attr
module Node = Vdom.Node

module Style =
  [%css.raw
    {|
        .clear_fieldset_styles {
          border: 0;
          margin: 0;
          padding: 0;
        }
      |}]

module Error_details = struct
  type t =
    { error : Error.t
    ; on_mouse_over : (unit Ui_effect.t[@sexp.opaque])
    ; on_mouse_out : (unit Ui_effect.t[@sexp.opaque])
    ; on_click : (unit Ui_effect.t[@sexp.opaque])
    ; is_viewing : bool
    ; is_toggled : bool
    }
  [@@deriving sexp_of]
end

module Row = struct
  type t =
    { label : (Node.t option[@sexp.opaque])
    ; tooltip : (Node.t option[@sexp.opaque])
    ; form : (Node.t[@sexp.opaque])
    ; id : string
    ; error : Error_details.t option
    }
  [@@deriving sexp_of]
end

type t =
  | Empty
  | Row of Row.t
  | List of t list
  | Group of
      { label : (Node.t option[@sexp.opaque])
      ; tooltip : (Node.t option[@sexp.opaque])
      ; view : t
      }
  | Header_group of
      { label : (Node.t option[@sexp.opaque])
      ; tooltip : (Node.t option[@sexp.opaque])
      ; header_view : t
      ; view : t
      }
  | Submit_button of
      { text : string
      ; (* none implies that the button is disabled *)
        on_submit : (unit Ui_effect.t option[@sexp.opaque])
      }
[@@deriving sexp_of]

let suggest_error e1 = function
  | Row row ->
    let error =
      match row.error with
      (* Keep the inner error if one exists *)
      | Some e2 -> Some e2
      | None -> Some e1
    in
    Row { row with error }
  | other -> other
;;

let rec set_label label t =
  match t with
  | Empty -> Empty
  | List [] -> List []
  (* [set_label] on a list will traverse the list and attach it to the head *)
  | List (hd :: tl) -> List (set_label label hd :: tl)
  | Row row -> Row { row with label = Some label }
  | Group group -> Group { group with label = Some label }
  | Header_group group -> Header_group { group with label = Some label }
  | Submit_button _ -> t
;;

let rec set_tooltip tooltip t =
  match t with
  | Empty -> Empty
  | List [] -> List []
  (* [set_tooltip] on a list will traverse the list and attach it to the head *)
  | List (hd :: tl) -> List (set_tooltip tooltip hd :: tl)
  | Row row -> Row { row with tooltip = Some tooltip }
  | Group group -> Group { group with tooltip = Some tooltip }
  | Header_group group -> Header_group { group with tooltip = Some tooltip }
  | Submit_button _ -> t
;;

let group_list t =
  match t with
  | List _ -> Group { view = t; label = None; tooltip = None }
  | _ -> t
;;

let rec suggest_label label t =
  match t with
  | Empty -> Empty
  | List [] -> List []
  (* [suggest_label] on a list will traverse the list and attach it to the head *)
  | List (hd :: tl) -> List (suggest_label label hd :: tl)
  (* If it already has a label, keep it *)
  | Row { label = Some _; _ }
  | Group { label = Some _; _ }
  | Header_group { label = Some _; _ } -> t
  | Row ({ label = None; _ } as row) -> Row { row with label = Some label }
  | Group ({ label = None; _ } as group) -> Group { group with label = Some label }
  | Header_group ({ label = None; _ } as group) ->
    Header_group { group with label = Some label }
  | Submit_button _ -> t
;;

let group label view = Group { label = Some label; tooltip = None; view }
let of_vdom ~id form = Row { label = None; tooltip = None; form; id; error = None }

let concat a b =
  match a, b with
  | Empty, x | x, Empty -> x
  | List a, List b -> List (a @ b)
  | a, List b -> List (a :: b)
  | List a, b -> List (List.append a [ b ])
  | a, b -> List [ a; b ]
;;

let rec view_error (e : Error.Internal_repr.t) : Node.t list =
  let bold text = Node.strong [ Node.text text ] in
  let pre text = Node.pre [ Node.text text ] in
  let view_sexp sexp = sexp |> Sexp.to_string_hum |> pre in
  let div contents = Node.div [ contents ] in
  match e with
  | Could_not_construct sexp -> [ div (bold "could not construct"); div (view_sexp sexp) ]
  | String s -> [ div (Node.text s) ]
  | Exn e -> [ div (pre (Exn.to_string e)) ]
  | Sexp s -> [ div (view_sexp s) ]
  | Tag_sexp (string, sexp, Some there) ->
    [ div (bold string)
    ; div (view_sexp sexp)
    ; div (pre (Source_code_position.to_string there))
    ]
  | Tag_sexp (string, sexp, None) -> [ div (bold string); div (view_sexp sexp) ]
  | Tag_t (string, error) -> div (bold string) :: view_error error
  | Tag_arg (string, sexp, error) ->
    div (bold string) :: div (view_sexp sexp) :: view_error error
  | Of_list (Some truncate_after, errors) ->
    errors |> Fn.flip List.take truncate_after |> List.bind ~f:view_error
  | Of_list (None, errors) -> errors |> List.bind ~f:view_error
  | With_backtrace (error, backtrace) ->
    List.append (view_error error) [ div (pre backtrace) ]
;;

let view_error error = view_error (Error.Internal_repr.of_info error)

let view_error_details
      { Error_details.error; on_mouse_over; on_mouse_out; on_click; is_viewing; is_toggled }
  =
  let flag =
    Node.div
      ~attr:
        (Attr.many_without_merge
           [ Attr.on_mouseover (fun _ -> on_mouse_over)
           ; Attr.on_mouseout (fun _ -> on_mouse_out)
           ; Attr.on_click (fun _ -> on_click)
           ; Attr.style
               Css_gen.(
                 (if is_toggled then color (`Name "black") else color (`Hex "#f54646"))
                 @> font_size (`Em_float 1.2)
                 @> Css_gen.create ~field:"cursor" ~value:"pointer")
           ])
      [ Node.text "⚠" ]
  in
  let contents =
    if not is_viewing
    then Node.none
    else
      Node.div
        ~attr:
          (Attr.style
             Css_gen.(
               (if is_toggled
                then border ~width:(`Px 1) ~color:(`Name "black") ~style:`Solid ()
                else border ~width:(`Px 1) ~color:(`Name "red") ~style:`Solid ())
               @> position ~top:(`Px 0) ~left:(`Em 2) `Absolute
               @> padding ~left:(`Em 1) ~right:(`Em 1) ()
               @> border_radius (`Px 3)
               @> background_color (`Name "pink")))
        (view_error error)
  in
  let result =
    Node.div
      ~attr:
        (Attr.many_without_merge
           [ Attr.style (Css_gen.position `Relative); Attr.class_ "bonsai-forms-error" ])
      [ flag; contents ]
  in
  result
;;

module Tooltip = struct
  module Css =
    [%css.raw
      {|
    .container {
      position: relative;
      display: inline-block;
    }

    .label {
      cursor: pointer;
      color: blue;
    }

    .text {
      visibility: hidden;
      width: 300px;
      background-color: azure;
      color: black;
      text-align: center;
      border-radius: 3px;
      padding: 0.5em 1em 0.5em 1em;
      border: 1px solid darkblue;
      position: absolute;
      z-index: 1;
      bottom: 100%;
      left: 50%;
      margin-left: -150px;
      cursor: text;
    }

    .container:hover .text {
      visibility: visible;
    }

    .checkbox:checked ~ .text {
      visibility: visible;
    }

    .checkbox:checked ~ .span {
      color: black;
    }

    .checkbox {
      position: absolute;
      opacity: 0%;
      cursor: pointer;
    }
    |}]

  let view inner =
    Node.div
      ~attr:(Attr.class_ Css.container)
      [ Node.label
          ~attr:(Attr.class_ Css.label)
          [ Node.input ~attr:Attr.(type_ "checkbox" @ class_ Css.checkbox) []
          ; Node.span ~attr:(Attr.class_ Css.span) [ Node.text "ⓘ" ]
          ; Node.div ~attr:(Attr.class_ Css.text) [ inner ]
          ]
      ]
  ;;
end

let rec to_vdom ~depth =
  let depth_td ~extra_attrs =
    let attr = Attr.(style (Css_gen.padding_left (`Em depth)) @ extra_attrs) in
    Node.td ~attr
  in
  function
  | Empty -> []
  | Group { label; tooltip; view } ->
    let rest = to_vdom view ~depth:(depth + 1) in
    let header_is_inhabited = Option.is_some label || Option.is_some tooltip in
    if header_is_inhabited
    then (
      let label =
        match label with
        | Some label ->
          depth_td
            ~extra_attrs:Attr.(style (Css_gen.font_weight `Bold) @ colspan 2)
            [ label ]
        | None -> Node.None
      in
      let tooltip =
        match tooltip with
        | Some tooltip -> Node.td [ Tooltip.view tooltip ]
        | None -> Node.None
      in
      Node.tr [ label; tooltip ] :: rest)
    else rest
  | Header_group { label; tooltip; view; header_view } ->
    let rest = to_vdom view ~depth:(depth + 1) in
    let header_view =
      let colspan = if Option.is_some label then Attr.empty else Attr.colspan 2 in
      Node.td ~attr:colspan (to_vdom_plain header_view)
    in
    let label =
      match label with
      | Some label ->
        depth_td ~extra_attrs:(Attr.style (Css_gen.font_weight `Bold)) [ label ]
      | None -> Node.None
    in
    let tooltip =
      match tooltip with
      | Some tooltip -> Node.td [ Tooltip.view tooltip ]
      | None -> Node.None
    in
    Node.tr [ label; header_view; tooltip ] :: rest
  | Submit_button _ as btn ->
    let button = to_vdom_plain btn in
    [ Node.tr [ depth_td ~extra_attrs:Attr.(colspan 2) button ] ]
  | Row { label; tooltip; id; form; error } ->
    let label =
      match label with
      | Some label ->
        (* <label> nodes can be clicked on to focus the input element contained
           inside.  By setting display:block, even the whitespace to the right
           of the label is clickable, meaning that mis-clicking on particularly
           small labels is less likely. *)
        Node.label
          ~attr:
            (Attr.many_without_merge
               [ Attr.for_ id; Attr.style (Css_gen.display `Block) ])
          [ label ]
      | _ -> Node.text ""
    in
    let tooltip =
      match tooltip with
      | Some tooltip -> Tooltip.view tooltip
      | None -> Node.text ""
    in
    let error =
      match error with
      | None -> Node.text ""
      | Some e -> view_error_details e
    in
    let label_attrs =
      Attr.style
        Css_gen.(
          padding_right (`Em 1)
          @> text_align `Left
          @> font_weight `Bold
          @> user_select `None)
    in
    [ (* This key prevents inputs of different "kinds" from clobbering each other *)
      Node.tr
        ~key:id
        [ depth_td ~extra_attrs:label_attrs [ label ]
        ; Node.td [ form ]
        ; Node.td
            [ Node.div
                ~attr:(Attr.style (Css_gen.flex_container ~direction:`Row ()))
                [ tooltip; error ]
            ]
        ]
    ]
  | List l -> List.concat_map l ~f:(to_vdom ~depth)

(* If the form is just a single row, return the view for it without wrapping *)
and to_vdom_plain = function
  | Empty -> []
  | Header_group { label = _; tooltip = _; header_view; view } ->
    to_vdom_plain header_view @ to_vdom_plain view
  | Group { label = _; tooltip = _; view } -> to_vdom_plain view
  | Row { label = _; tooltip = _; id = _; form; error = _ } -> [ form ]
  | List l -> List.concat_map l ~f:to_vdom_plain
  | Submit_button { on_submit; text } ->
    (match on_submit with
     | Some event ->
       let event = Vdom.Effect.(Many [ event; Prevent_default; Stop_propagation ]) in
       [ Node.button ~attr:(Attr.on_click (fun _ -> event)) [ Vdom.Node.text text ] ]
     | None -> [ Node.button ~attr:Attr.disabled [ Vdom.Node.text text ] ])
;;

type submission_options =
  { on_submit : unit Ui_effect.t option
  ; handle_enter : bool
  ; button_text : string option
  }

type editable =
  [ `Yes_always
  | `Currently_yes
  | `Currently_no
  ]

let with_fieldset ~currently_editable view =
  let disabled_ = if currently_editable then Vdom.Attr.empty else Vdom.Attr.disabled in
  Vdom.Node.fieldset
    ~attr:Vdom.Attr.(disabled_ @ class_ Style.clear_fieldset_styles)
    [ view ]
;;

let to_vdom ?on_submit ?(editable = `Yes_always) view =
  let view =
    match on_submit with
    | Some { on_submit; button_text = Some button_text; handle_enter = _ } ->
      let button = Submit_button { text = button_text; on_submit } in
      concat view button
    | _ -> view
  in
  let inner_table = Node.table [ Node.tbody (to_vdom view ~depth:0) ] in
  let inner_table =
    match editable with
    | `Yes_always -> inner_table
    | `Currently_yes -> with_fieldset ~currently_editable:true inner_table
    | `Currently_no -> with_fieldset ~currently_editable:false inner_table
  in
  match on_submit with
  | Some { on_submit; handle_enter = true; _ } ->
    let always_use = [ Vdom.Effect.Prevent_default; Vdom.Effect.Stop_propagation ] in
    let event =
      match on_submit with
      | None -> Vdom.Effect.Many always_use
      | Some event -> Vdom.Effect.Many (event :: always_use)
    in
    Node.create "form" ~attr:(Vdom.Attr.on_submit (fun _ -> event)) [ inner_table ]
  | _ -> inner_table
;;
