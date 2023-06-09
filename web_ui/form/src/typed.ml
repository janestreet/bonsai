open! Core
module Form_view = View
open! Bonsai_web
open! Bonsai.Let_syntax

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
    let module Form_value = struct
      type 'a t = 'a Form.t Computation.t
    end
    in
    let module App = struct
      include Computation

      type 'a s = 'a Form.t

      let translate = Fn.id
    end
    in
    let module The_form_values = Typed_field_map.Make (M.Typed_field) (Form_value) in
    let module The_forms = Typed_field_map.Make (M.Typed_field) (Form) in
    let module The_results = Typed_field_map.Make (M.Typed_field) (Or_error) in
    let module To_forms = The_form_values.As_applicative.To_other_map (App) (The_forms) in
    let get_label =
      match M.label_for_field with
      | `Inferred ->
        Value.return (fun { M.Typed_field.Packed.f = T t } -> M.Typed_field.name t)
      | `Computed f -> Value.return (fun { M.Typed_field.Packed.f = T t } -> f t)
      | `Dynamic f -> f
    in
    let form_values_per_field =
      let f field =
        let%sub subform = M.form_for_field field in
        let%sub subform = Form.Dynamic.error_hint subform in
        let%arr subform = subform
        and get_label = get_label in
        let label = get_label { f = T field } in
        subform |> Form.Private.suggest_label label |> attach_fieldname_to_error label
      in
      The_form_values.create { f }
    in
    let%sub forms_per_field = To_forms.run form_values_per_field in
    let%arr forms_per_field = forms_per_field
    and get_label = get_label in
    let view =
      M.Typed_field.Packed.all
      |> List.map ~f:(fun { f = T field } ->
        { Form_view.field_name = get_label (M.Typed_field.Packed.pack field)
        ; field_view = Form.view (The_forms.find forms_per_field field)
        })
      |> Form_view.record
    in
    let value =
      let f field = Form.value (The_forms.find forms_per_field field) in
      The_results.As_applicative.transpose
        (module Or_error)
        ~create:(fun { f } -> M.Typed_field.create { f })
        (The_results.create { f })
    in
    let set r =
      M.Typed_field.Packed.all
      |> List.map ~f:(fun { f = T field } ->
        Form.set (The_forms.find forms_per_field field) (M.Typed_field.get field r))
      |> Vdom.Effect.Many
    in
    Form.Expert.create ~view ~value ~set
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

  let make
        (type a)
        ?(picker = `Dropdown)
        ?picker_attr
        (module M : S with type Typed_variant.derived_on = a)
    =
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
    let module M_opt = struct
      type t = M.Typed_variant.Packed.t option
      [@@deriving sexp, equal, enumerate, compare]

      let all_some = List.filter all ~f:Option.is_some
      let first_some = List.hd_exn all_some
    end
    in
    let%sub extra_attrs =
      match picker_attr with
      | None -> Bonsai.const []
      | Some attr -> Bonsai.pure List.return attr
    in
    let%sub picker_value, set_picker_value, picker_view =
      match picker with
      | `Dropdown ->
        let default_model, all =
          match M.initial_choice with
          | `Empty -> None, M_opt.all
          | `First_constructor -> M_opt.first_some, M_opt.all_some
          | `This example -> Some example, M_opt.all_some
        in
        let%sub picker_value, set_picker_value =
          Bonsai.state
            default_model
            ~sexp_of_model:[%sexp_of: M_opt.t]
            ~equal:[%equal: M_opt.t]
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
        let default_model =
          match M.initial_choice with
          | `Empty -> None
          | `First_constructor -> M_opt.first_some
          | `This example -> Some example
        in
        let%sub picker_value, set_picker_value =
          Bonsai.state
            default_model
            ~sexp_of_model:[%sexp_of: M_opt.t]
            ~equal:[%equal: M_opt.t]
        in
        let%sub path = Bonsai.path_id in
        let%arr picker_value = picker_value
        and set_picker_value = set_picker_value
        and extra_attrs = extra_attrs
        and path = path
        and to_string = to_string in
        let node_fun =
          match layout with
          | `Vertical ->
            Vdom_input_widgets.Radio_buttons.of_values ~merge_behavior:Legacy_dont_merge
          | `Horizontal ->
            Vdom_input_widgets.Radio_buttons.of_values_horizontal
              ~merge_behavior:Legacy_dont_merge
        in
        let view =
          node_fun
            ~extra_attrs:(Vdom.Attr.id path :: extra_attrs)
            (module struct
              include M.Typed_variant.Packed

              let to_string = to_string
            end)
            ~on_click:(fun opt -> set_picker_value (Some opt))
            ~selected:picker_value
            ~name:path
            M.Typed_variant.Packed.all
        in
        picker_value, set_picker_value, view
    in
    let%sub inner =
      Bonsai.enum
        (module M_opt)
        ~match_:picker_value
        ~with_:(function
          | None ->
            Bonsai.const (Form.return_error (Error.of_string "a value is required"))
          | Some { f = T p } ->
            let%map.Computation form_for_constructor = M.form_for_variant p in
            let parse_exn content = M.Typed_variant.create p content in
            let unparse kind =
              match M.Typed_variant.get p kind with
              | None ->
                let expected = M.Typed_variant.Packed.pack p in
                let found = M.Typed_variant.which kind in
                raise_s
                  [%message
                    "BUG"
                      [%here]
                      (expected : M.Typed_variant.Packed.t)
                      (found : M.Typed_variant.Packed.t)]
              | Some v -> v
            in
            Form.project form_for_constructor ~parse_exn ~unparse)
    in
    let%sub get_inner_form = Bonsai.yoink inner in
    let%arr inner = inner
    and picker_value = picker_value
    and picker_view = picker_view
    and set_picker_value = set_picker_value
    and get_inner_form = get_inner_form in
    let view =
      match picker_value with
      | None -> Form_view.variant ~clause_selector:picker_view ~selected_clause:None
      | Some { f = T p } ->
        let clause_name = M.Typed_variant.name p in
        Form_view.variant
          ~clause_selector:picker_view
          ~selected_clause:(Some { Form_view.clause_name; clause_view = Form.view inner })
    in
    let value = Form.value inner in
    let set value =
      let constructor = M.Typed_variant.which value in
      let open Ui_effect.Let_syntax in
      (* sequence this so that the result of evaluating the picker is visible
         when setting the innermost form *)
      let%bind () = set_picker_value (Some constructor) in
      let%bind inner =
        match%bind.Effect get_inner_form with
        | Active inner -> Effect.return inner
        | Inactive ->
          Effect.never
      in
      Form.set inner value
    in
    Form.Expert.create ~view ~value ~set
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
end
