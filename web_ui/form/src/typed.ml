open! Core
module Form_view = View
open! Bonsai_web
open! Bonsai.Let_syntax
module Form2 = Bonsai_web_ui_form2
open Form2.Typed

module Record = struct
  module type S = sig
    module Typed_field : Typed_fields_lib.S

    val label_for_field
      : [ `Inferred
        | `Computed of 'a Typed_field.t -> string
        | `Dynamic of (Typed_field.Packed.t -> string) Value.t
        ]

    val form_for_field : 'a Typed_field.t -> 'a Form.t Computation.t
  end

  let attach_fieldname_to_error name t =
    Form.map_error t ~f:(Error.tag ~tag:(sprintf "in field " ^ name))
  ;;

  let make (type a) (module M : S with type Typed_field.derived_on = a) =
    (* We will do the error tagging ourselves in this function. *)
    Record.make_without_tagging_errors
      (module struct
        include M

        type field_view = Form_view.t
        type resulting_view = Form_view.t

        let get_label =
          match M.label_for_field with
          | `Inferred ->
            Value.return (fun { M.Typed_field.Packed.f = T t } -> M.Typed_field.name t)
          | `Computed f -> Value.return (fun { M.Typed_field.Packed.f = T t } -> f t)
          | `Dynamic f -> f
        ;;

        let form_for_field : type a. a Typed_field.t -> a Form.t Computation.t =
          fun field ->
          let%sub form = M.form_for_field field in
          let%sub form = Form.Dynamic.error_hint form in
          let%arr form = form
          and get_label = get_label in
          let label = get_label { M.Typed_field.Packed.f = T field } in
          Form.Private.suggest_label label form |> attach_fieldname_to_error label
        ;;

        type form_of_field_fn =
          { f : 'a. 'a Typed_field.t -> ('a, field_view) Form2.t Value.t }

        let finalize_view { f } =
          let all_fields =
            M.Typed_field.Packed.all
            |> List.map ~f:(fun ({ f = T field } as packed) ->
                 let%map field = f field in
                 packed, Form.view field)
            |> Value.all
          in
          let%arr all_fields = all_fields
          and get_label = get_label in
          List.map all_fields ~f:(fun (packed, view) ->
            { Form_view.field_name = get_label packed; field_view = view })
          |> Form_view.record
        ;;
      end)
  ;;

  let make_table (type a) (module M : S with type Typed_field.derived_on = a) =
    let%sub table =
      Record.make_table
        (module struct
          include M

          let get_label =
            match M.label_for_field with
            | `Inferred ->
              Value.return (fun { M.Typed_field.Packed.f = T t } -> M.Typed_field.name t)
            | `Computed f -> Value.return (fun { M.Typed_field.Packed.f = T t } -> f t)
            | `Dynamic f -> f
          ;;

          let form_for_field field =
            let%sub form =
              let%sub form = M.form_for_field field in
              let%sub form = Form.Dynamic.error_hint form in
              let%arr form = form
              and get_label = get_label in
              let label = get_label { M.Typed_field.Packed.f = T field } in
              attach_fieldname_to_error label form
            in
            let%arr form = form in
            Form2.map_view form ~f:Form.View.to_vdom
          ;;
        end)
    in
    let%sub path = Bonsai.path_id in
    let%arr table = table
    and path = path in
    Form2.map_view table ~f:(Form.View.of_vdom ~unique_key:path)
  ;;
end

module Variant = struct
  module type S = sig
    module Typed_variant : Typed_variants_lib.S

    val label_for_variant
      : [ `Inferred
        | `Computed of 'a Typed_variant.t -> string
        | `Dynamic of (Typed_variant.Packed.t -> string) Value.t
        ]

    val form_for_variant : 'a Typed_variant.t -> 'a Form.t Computation.t
    val initial_choice : [ `First_constructor | `Empty | `This of Typed_variant.Packed.t ]
  end

  module type S_set = sig
    include Comparator.S
    module Typed_variant : Typed_variants_lib.S with type derived_on = t

    val label_for_variant
      : [ `Inferred
        | `Computed of 'a Typed_variant.t -> string
        | `Dynamic of (Typed_variant.Packed.t -> string) Value.t
        ]

    val sexp_of_variant_argument
      : [ `Use_sexp_of_variant | `Custom of 'a Typed_variant.t -> 'a -> Sexp.t ]

    val form_for_variant
      :  'a Typed_variant.t
      -> ('a, 'cmp) Bonsai.comparator
      -> ('a, 'cmp) Set.t Form.t Computation.t
  end

  module type Opts = sig
    type t [@@deriving sexp, equal, enumerate, compare]
  end

  let form_for_picker
    (type a)
    (module M : Opts with type t = a)
    ~to_string
    ~picker_kind
    ~picker_attr
    ~initial_choice
    =
    let module M_opt = struct
      type t = M.t option [@@deriving sexp, equal, enumerate, compare]

      let all_some = List.filter all ~f:Option.is_some
      let first_some = List.hd_exn all_some
    end
    in
    let default_model =
      match initial_choice with
      | `Empty -> None
      | `First_constructor -> M_opt.first_some
      | `This example -> Some example
    in
    let%sub picker_value, set_picker_value = Bonsai.state default_model in
    let%sub extra_attrs =
      match picker_attr with
      | None -> Bonsai.const []
      | Some attr -> Bonsai.pure List.return attr
    in
    let%sub picker_value, set_picker_value, view =
      match picker_kind with
      | `Dropdown ->
        let all =
          match initial_choice with
          | `Empty -> M_opt.all
          | `First_constructor | `This _ -> M_opt.all_some
        in
        let%sub path = Bonsai.path_id in
        let%arr picker_value = picker_value
        and set_picker_value = set_picker_value
        and path = path
        and extra_attrs = extra_attrs
        and to_string = to_string in
        let view =
          Vdom_input_widgets.Dropdown.of_values
            (module struct
              include M_opt

              let to_string t =
                match t with
                | None -> ""
                | Some x -> to_string x
              ;;
            end)
            all
            ~merge_behavior:Legacy_dont_merge
            ~extra_attrs:
              ([ Vdom.Attr.id path
               ; Vdom.Attr.style (Css_gen.width (`Percent Percent.one_hundred_percent))
               ]
               @ extra_attrs)
            ~selected:picker_value
            ~on_change:set_picker_value
        in
        picker_value, set_picker_value, view
      | `Radio layout ->
        let%sub path = Bonsai.path_id in
        let%arr picker_value = picker_value
        and set_picker_value = set_picker_value
        and extra_attrs = extra_attrs
        and path = path
        and to_string = to_string in
        let node_fun =
          match layout with
          | `Vertical -> Vdom_input_widgets.Radio_buttons.of_values
          | `Horizontal -> Vdom_input_widgets.Radio_buttons.of_values_horizontal
        in
        let view =
          node_fun
            ~extra_container_attrs:(Vdom.Attr.id path :: extra_attrs)
            (module struct
              include M

              let to_string = to_string
            end)
            ~on_click:(fun opt -> set_picker_value (Some opt))
            ~selected:picker_value
            ~name:path
            M.all
        in
        picker_value, set_picker_value, view
    in
    let%arr picker_value = picker_value
    and set_picker_value = set_picker_value
    and view = view in
    let value =
      match picker_value with
      | Some value -> Ok value
      | None -> Error (Error.of_string "a value is required")
    in
    let set value = set_picker_value (Some value) in
    ({ value; set; view } : (M.t, Vdom.Node.t) Form2.t)
  ;;

  let make
    (type a)
    ?(picker = `Dropdown)
    ?picker_attr
    (module M : S with type Typed_variant.derived_on = a)
    =
    Variant.make
      (module struct
        include M

        type picker_view = Vdom.Node.t
        type variant_view = Form_view.t
        type resulting_view = Form_view.t

        let form_for_picker =
          let to_string =
            match M.label_for_variant with
            | `Inferred ->
              Value.return (fun t ->
                Form_view.sexp_to_pretty_string M.Typed_variant.Packed.sexp_of_t t)
            | `Computed variant_to_string ->
              Value.return (fun ({ f = T field } : M.Typed_variant.Packed.t) ->
                variant_to_string field)
            | `Dynamic variant_to_string -> variant_to_string
          in
          form_for_picker
            (module M.Typed_variant.Packed)
            ~to_string
            ~picker_kind:picker
            ~picker_attr
            ~initial_choice:M.initial_choice
        ;;

        let finalize_view picker_view selected =
          match selected with
          | Error _ ->
            Form_view.variant ~clause_selector:picker_view ~selected_clause:None
          | Ok (field, form) ->
            let clause_name = M.Typed_variant.name field in
            Form_view.variant
              ~clause_selector:picker_view
              ~selected_clause:
                (Some { Form_view.clause_name; clause_view = Form.view form })
        ;;
      end)
  ;;

  let make_optional
    (type a)
    ?picker
    ?picker_attr
    ?(empty_label = "(none)")
    (module M : S with type Typed_variant.derived_on = a)
    : a option Form.t Computation.t
    =
    let module Transformed = struct
      module Original = struct
        type t = M.Typed_variant.derived_on

        module Typed_variant = M.Typed_variant
      end

      type t =
        | None
        | Some of Original.t [@subvariant]
      [@@deriving typed_variants, variants]

      let label_for_variant =
        `Dynamic
          (* The fact we can't sub + arr here is a little bit sad, but note that the
             [let%map] is not doing anything expensive, just wrapping the provided
             function (which is usually constant). *)
          (let%map to_string =
             match M.label_for_variant with
             | `Inferred ->
               Value.return (fun ({ f = T field } : Original.Typed_variant.Packed.t) ->
                 M.Typed_variant.name field)
             | `Computed f ->
               Value.return (fun ({ f = T field } : Original.Typed_variant.Packed.t) ->
                 f field)
             | `Dynamic f -> f
           in
           fun ({ f = T v } : Typed_variant.Packed.t) ->
             match v with
             | None -> empty_label
             | Some subvariant -> to_string { f = T subvariant })
      ;;

      let form_for_variant : type a. a Typed_variant.t -> a Form.t Computation.t
        = function
        | None -> Bonsai.const (Form.return ())
        | Some subvariant -> M.form_for_variant subvariant
      ;;

      let initial_choice =
        match M.initial_choice with
        | `First_constructor -> `First_constructor
        | `Empty -> `Empty
        | `This { f = T a } ->
          let tv : Typed_variant.Packed.t = { f = T (Some a) } in
          `This tv
      ;;
    end
    in
    make ?picker ?picker_attr (module Transformed)
    |> Computation.map
         ~f:
           (Form.project
              ~parse_exn:Transformed.some_val
              ~unparse:(Option.value_map ~f:Transformed.some ~default:Transformed.none))
  ;;

  let attach_variant_name_to_error name t =
    Form.map_error t ~f:(Error.tag ~tag:(sprintf "in variant " ^ name))
  ;;

  let make_set
    (type a cmp)
    (module M : S_set with type t = a and type comparator_witness = cmp)
    : (a, cmp) Set.t Form.t Computation.t
    =
    Variant.make_set_without_tagging_errors
      (module struct
        include M

        type variant_view = Form_view.t
        type resulting_view = Form_view.t

        let get_label =
          match M.label_for_variant with
          | `Inferred ->
            Value.return (fun t ->
              Form_view.sexp_to_pretty_string M.Typed_variant.Packed.sexp_of_t t)
          | `Computed f -> Value.return (fun { M.Typed_variant.Packed.f = T t } -> f t)
          | `Dynamic f -> f
        ;;

        let form_for_variant
          : type a cmp.
            a Typed_variant.t
            -> (a, cmp) Bonsai.comparator
            -> (a, cmp) Set.t Form.t Computation.t
          =
          fun variant comparator ->
          let%sub form = M.form_for_variant variant comparator in
          let%sub form = Form.Dynamic.error_hint form in
          let%arr form = form
          and get_label = get_label in
          let label = get_label { M.Typed_variant.Packed.f = T variant } in
          Form.Private.suggest_label label form |> attach_variant_name_to_error label
        ;;

        type form_of_variant_fn =
          { f :
              'a.
              'a Typed_variant.t -> ('a, variant_view) Variant.Packed_set_form.t Value.t
          }

        let finalize_view { f } =
          let all_variants =
            M.Typed_variant.Packed.all
            |> List.map ~f:(fun ({ f = T variant } as packed) ->
                 let%map (Variant.Packed_set_form.T { form; _ }) = f variant in
                 packed, Form.view form)
            |> Value.all
          in
          let%arr all_variants = all_variants
          and get_label = get_label in
          List.map all_variants ~f:(fun (packed, view) ->
            { Form_view.field_name = get_label packed; field_view = view })
          |> Form_view.record
        ;;
      end)
  ;;
end
