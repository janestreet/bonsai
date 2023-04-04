open! Core
open Import
open Form_view
module Toggle = Bonsai_web_ui_toggle

let rec view_error (e : Error.Internal_repr.t) : Vdom.Node.t list =
  let bold text = Vdom.Node.strong [ Vdom.Node.text text ] in
  let pre text = Vdom.Node.pre [ Vdom.Node.text text ] in
  let view_sexp sexp = pre (Sexp.to_string_hum sexp) in
  let div contents = Vdom.Node.div [ contents ] in
  match e with
  | Could_not_construct sexp -> [ div (view_sexp sexp) ]
  | String s -> [ div (Vdom.Node.text s) ]
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
    errors |> Fn.flip List.take truncate_after |> List.concat_map ~f:view_error
  | Of_list (None, errors) -> List.concat_map ~f:view_error errors
  | With_backtrace (error, backtrace) ->
    List.append (view_error error) [ div (pre backtrace) ]
;;

let view_error error = view_error (Error.Internal_repr.of_info error)

(* A form is disabled if it is not editable *)
let disabled_of_editable = function
  | `Currently_no -> true
  | `Yes_always | `Currently_yes -> false
;;

let render_append_item self ~eval_context append_item =
  match append_item with
  | Append_view view -> view
  | Append_info { append; text } ->
    let text = Option.value text ~default:"Add new element" in
    self#button
      ~attrs:[ Vdom.Attr.type_ "button" ]
      ~disabled:(disabled_of_editable (Form_context.editable eval_context))
      ~intent:None
      ~tooltip:None
      ~on_click:append
      [ Vdom.Node.text text ]
;;

let render_remove_item self ~eval_context remove_item ~index =
  match remove_item with
  | Remove_view view -> view
  | Remove_info { remove; element_label } ->
    let delete_button =
      self#button
        ~attrs:
          [ Vdom.Attr.many
              [ Vdom.Attr.type_ "button"
              ; Vdom.Attr.style
                  Css_gen.(
                    border ~style:`None ()
                    @> create ~field:"cursor" ~value:"pointer"
                    @> color (`Name "blue")
                    @> create ~field:"background" ~value:"none")
              ]
          ]
        ~disabled:(disabled_of_editable (Form_context.editable eval_context))
        ~intent:None
        ~tooltip:None
        ~on_click:remove
        [ Vdom.Node.text "[ remove ]" ]
    in
    (match element_label with
     | None -> Vdom.Node.div [ Vdom.Node.textf "%d - " index; delete_button ]
     | Some f -> f ~delete_button index)
;;

let view_error_details self error =
  let constants : Constants.t = self#constants in
  Toggle.view
    { Toggle.Colors.toggle_text = constants.form.error_toggle_text
    ; inner_background = constants.form.error_message.background
    ; inner_border = constants.form.error_border
    ; inner_text = constants.form.error_message.foreground
    }
    ~toggle:(Vdom.Node.text "⚠")
    ~inner:(Vdom.Node.div (self#form_view_error error))
    ~direction:`Right
;;

let view_tooltip self inner =
  let constants : Constants.t = self#constants in
  Toggle.view
    { Toggle.Colors.inner_text = constants.form.tooltip_message.foreground
    ; inner_border = constants.form.tooltip_border
    ; inner_background = constants.form.tooltip_message.background
    ; toggle_text = constants.form.tooltip_toggle_text
    }
    ~toggle:(Vdom.Node.text "ⓘ")
    ~inner
    ~direction:`Above
;;

let wrap_tooltip_and_error self ~tooltip ~error =
  match Option.is_none tooltip && Option.is_none error with
  | true -> Vdom.Node.none
  | false ->
    let tooltip =
      Option.value_map tooltip ~f:self#form_view_tooltip ~default:Vdom.Node.none
    in
    let error =
      Option.value_map error ~f:self#form_view_error_details ~default:(Vdom.Node.text "")
    in
    Vdom.Node.td
      [ Vdom.Node.div
          ~attrs:[ Vdom.Attr.style (Css_gen.flex_container ~direction:`Row ()) ]
          [ tooltip; error ]
      ]
;;

let depth_td' ~depth ~extra_attrs =
  let attr = Vdom.Attr.(style (Css_gen.padding_left (`Em depth)) @ extra_attrs) in
  Vdom.Node.td ~attrs:[ attr ]
;;

let depth_td_of_context context = depth_td' ~depth:(Form_context.depth context)

let render_label
      ?id
      ?colspan
      ?(extra_attrs = [])
      label
      ~(depth_td : extra_attrs:Vdom.Attr.t -> ?key:string -> Vdom.Node.t list -> Vdom.Node.t)
  =
  let colspan = Option.value_map colspan ~default:Vdom.Attr.empty ~f:Vdom.Attr.colspan in
  let for_ = Option.value_map id ~default:Vdom.Attr.empty ~f:Vdom.Attr.for_ in
  let label =
    match label with
    | None -> Vdom.Node.text ""
    | Some label ->
      Vdom.Node.label ~attrs:[ for_; Vdom.Attr.style (Css_gen.display `Block) ] [ label ]
  in
  depth_td ~extra_attrs:(Vdom.Attr.many (colspan :: extra_attrs)) [ label ]
;;

let render_context_above self ~view_context ~eval_context =
  let depth_td = depth_td_of_context eval_context in
  let inhabited_header =
    Option.is_some view_context.label
    || Option.is_some view_context.error
    || Option.is_some view_context.tooltip
  in
  match inhabited_header with
  | false -> []
  | true ->
    let label =
      render_label
        ~extra_attrs:[ Vdom.Attr.style (Css_gen.font_weight `Bold) ]
        ~colspan:2
        ~depth_td
        view_context.label
    in
    let tooltip_and_error =
      wrap_tooltip_and_error ~tooltip:view_context.tooltip ~error:view_context.error
    in
    [ Vdom.Node.tr [ label; tooltip_and_error self ] ]
;;

let render_context_inline self ?id inline_view ~view_context ~eval_context =
  let depth_td = depth_td_of_context eval_context in
  let inline_view = Vdom.Node.td [ inline_view ] in
  let label_attrs =
    [ Vdom.Attr.style
        Css_gen.(
          padding_right (`Em 1)
          @> text_align `Left
          @> font_weight `Bold
          @> user_select `None)
    ]
  in
  let label = render_label ?id ~extra_attrs:label_attrs ~depth_td view_context.label in
  let tooltip_and_error =
    wrap_tooltip_and_error ~tooltip:view_context.tooltip ~error:view_context.error
  in
  (* This key prevents inputs of different "kinds" from clobbering each other *)
  Vdom.Node.tr ?key:id [ label; inline_view; tooltip_and_error self ]
;;

let empty ~eval_context:_ ~view_context:_ () = []

let collapsible
      self
      ~eval_context
      ~view_context
      ({ collapse_label; state } : Form_view.collapsible)
  =
  let tooltip_and_error =
    wrap_tooltip_and_error self ~tooltip:view_context.tooltip ~error:view_context.error
  in
  let label =
    Vdom.Node.tr
      [ render_label
          ~extra_attrs:[ Vdom.Attr.style (Css_gen.font_weight `Bold) ]
          ~colspan:2
          (Some collapse_label)
          ~depth_td:(depth_td_of_context eval_context)
      ; tooltip_and_error
      ]
  in
  let rest =
    match state with
    | Collapsed None -> []
    | Collapsed (Some t) | Expanded t ->
      self#form_view ~eval_context:(Form_context.incr_depth eval_context) t
  in
  label :: rest
;;

let tuple self ~eval_context ~view_context ts =
  render_context_above self ~view_context ~eval_context
  @ List.concat_map ts ~f:(self#form_view ~eval_context)
;;

let raw self ~eval_context ~view_context ({ raw_view; id } : Form_view.raw) =
  [ render_context_inline
      self
      ~id
      (raw_view view_context ~editable:(Form_context.editable eval_context))
      ~view_context
      ~eval_context
  ]
;;

let record self ~eval_context ~view_context fields =
  let rest =
    List.concat_map fields ~f:(fun { field_name; field_view } ->
      self#form_view
        ~eval_context:(Form_context.incr_depth eval_context)
        (Form_view.suggest_label field_name field_view))
  in
  render_context_above self ~eval_context ~view_context @ rest
;;

let variant
      self
      ~eval_context
      ~view_context
      ({ clause_selector; selected_clause } : Form_view.variant)
  =
  let rest =
    match selected_clause with
    | None -> []
    | Some clause ->
      self#form_view
        ~eval_context:(Form_context.incr_depth eval_context)
        clause.clause_view
  in
  render_context_inline self clause_selector ~eval_context ~view_context :: rest
;;

let option self ~eval_context ~view_context ({ toggle; status } : Form_view.option_view) =
  let rest =
    match status with
    | Currently_some view | Currently_none (Some view) ->
      self#form_view ~eval_context:(Form_context.incr_depth eval_context) view
    | Currently_none None -> []
  in
  render_context_inline self toggle ~eval_context ~view_context :: rest
;;

let list
      self
      ~eval_context
      ~view_context
      ({ list_items; append_item; legacy_button_position } : Form_view.list_view)
  =
  let items_and_removals =
    List.concat_mapi list_items ~f:(fun i { item_view; remove_item } ->
      let remove_button = render_remove_item self ~eval_context remove_item ~index:i in
      Vdom.Node.tr
        [ depth_td_of_context
            eval_context
            ~extra_attrs:
              (Vdom.Attr.many
                 [ Vdom.Attr.colspan 2; Vdom.Attr.style (Css_gen.font_weight `Bold) ])
            [ remove_button ]
        ]
      :: self#form_view ~eval_context:(Form_context.incr_depth eval_context) item_view)
  in
  let append_item =
    let append_button_context =
      match list_items, legacy_button_position with
      | [], _ | _, `Inline -> eval_context
      | _, `Indented -> Form_context.incr_depth eval_context
    in
    Vdom.Node.tr
      [ depth_td_of_context
          append_button_context
          ~extra_attrs:
            (Vdom.Attr.many
               [ Vdom.Attr.colspan 2; Vdom.Attr.style (Css_gen.font_weight `Bold) ])
          [ render_append_item self ~eval_context append_item ]
      ]
  in
  render_context_above self ~view_context ~eval_context
  @ items_and_removals
  @ [ append_item ]
;;

let view self ~eval_context t =
  match t.view with
  | Empty -> self#form_empty ~eval_context ~view_context:t.context ()
  | Collapsible collapsible ->
    self#form_collapsible ~eval_context ~view_context:t.context collapsible
  | Tuple ts -> self#form_tuple ~eval_context ~view_context:t.context ts
  | Raw raw -> self#form_raw ~eval_context ~view_context:t.context raw
  | Record fields -> self#form_record ~eval_context ~view_context:t.context fields
  | Variant variant -> self#form_variant ~eval_context ~view_context:t.context variant
  | Option option -> self#form_option ~eval_context ~view_context:t.context option
  | List list -> self#form_list ~eval_context ~view_context:t.context list
;;

let to_vdom_plain self ~eval_context t =
  let rec to_vdom_plain t =
    match t.view with
    | Empty -> []
    | Collapsible { state = Collapsed None; _ } -> []
    | Collapsible { state = Collapsed (Some t); _ }
    | Collapsible { state = Expanded t; _ } -> to_vdom_plain t
    | Raw { raw_view; _ } ->
      [ raw_view t.context ~editable:(Form_context.editable eval_context) ]
    | Record fields ->
      List.concat_map fields ~f:(fun { field_view; _ } -> to_vdom_plain field_view)
    | Variant { clause_selector; selected_clause = None } -> [ clause_selector ]
    | Variant { clause_selector; selected_clause = Some { clause_view; _ } } ->
      clause_selector :: to_vdom_plain clause_view
    | Tuple ts -> List.concat_map ts ~f:to_vdom_plain
    | Option { toggle; status = Currently_none None } -> [ toggle ]
    | Option { toggle; status = Currently_none (Some t) }
    | Option { toggle; status = Currently_some t } -> toggle :: to_vdom_plain t
    | List { list_items; append_item; _ } ->
      List.concat_map list_items ~f:(fun { item_view; _ } -> to_vdom_plain item_view)
      @ [ render_append_item self ~eval_context append_item ]
  in
  to_vdom_plain t
;;

let hidden_submit_input =
  Vdom.Node.input
    ~attrs:[ Vdom.Attr.style (Css_gen.display `None); Vdom.Attr.type_ "submit" ]
    ()
;;

let to_vdom self ?on_submit ~eval_context view =
  let submit_button =
    match on_submit with
    | Some
        { on_submit
        ; handle_enter = _
        ; button_text = Some button_text
        ; button_attr
        ; button_location = _
        } ->
      [ Vdom.Node.tr
          [ depth_td'
              ~depth:0
              ~extra_attrs:(Vdom.Attr.colspan 3)
              [ (match on_submit with
                  | None ->
                    Vdom.Node.button
                      ~attrs:[ button_attr; Vdom.Attr.disabled ]
                      [ Vdom.Node.text button_text ]
                  | Some event ->
                    let event =
                      Effect.(Many [ event; Prevent_default; Stop_propagation ])
                    in
                    Vdom.Node.button
                      ~attrs:[ button_attr; Vdom.Attr.on_click (fun _ -> event) ]
                      [ Vdom.Node.text button_text ])
              ]
          ]
      ]
    | None | Some _ -> []
  in
  let inner_table =
    let form_parts =
      match Option.map on_submit ~f:(fun o -> o.button_location) with
      | None | Some `After -> self#form_view ~eval_context view @ submit_button
      | Some `Before -> submit_button @ self#form_view ~eval_context view
    in
    self#form_toplevel_combine form_parts
  in
  let inner_table =
    match Form_context.editable eval_context with
    | `Yes_always -> inner_table
    | `Currently_yes -> with_fieldset ~currently_editable:true inner_table
    | `Currently_no -> with_fieldset ~currently_editable:false inner_table
  in
  match on_submit with
  | Some { on_submit; handle_enter = true; button_text; _ } ->
    let always_use = [ Vdom.Effect.Prevent_default; Vdom.Effect.Stop_propagation ] in
    let event =
      match on_submit with
      | None -> Vdom.Effect.Many always_use
      | Some event -> Vdom.Effect.Many (event :: always_use)
    in
    Vdom.Node.create
      "form"
      ~attrs:[ Vdom.Attr.on_submit (fun _ -> event) ]
      [ inner_table
      ; (if Option.is_none button_text then hidden_submit_input else Vdom.Node.none)
      ]
  | _ -> inner_table
;;

let toplevel_combine rows = Vdom.Node.table [ Vdom.Node.tbody rows ]
