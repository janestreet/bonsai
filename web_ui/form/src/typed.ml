open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

module Record = struct
  module type S = sig
    module Typed_field : Typed_fields_lib.S

    val label_for_field : [ `Inferred | `Computed of 'a Typed_field.t -> string ]
    val form_for_field : 'a Typed_field.t -> 'a Form.t Computation.t
  end

  let attach_fieldname_to_error name t =
    Form.map_error t ~f:(Error.tag ~tag:(sprintf "in field " ^ name))
  ;;

  let make (type a) (module M : S with type Typed_field.derived_on = a) =
    let module Form_value = struct
      type 'a t = 'a Form.t Bonsai.Computation.t
    end
    in
    let module App = struct
      include Bonsai.Computation

      type 'a s = 'a Form.t

      let translate = Fn.id
    end
    in
    let module The_form_values = Typed_field_map.Make (M.Typed_field) (Form_value) in
    let module The_forms = Typed_field_map.Make (M.Typed_field) (Form) in
    let module The_results = Typed_field_map.Make (M.Typed_field) (Or_error) in
    let module To_forms = The_form_values.As_applicative.To_other_map (App) (The_forms) in
    let form_values_per_field =
      let get_label =
        match M.label_for_field with
        | `Inferred -> M.Typed_field.name
        | `Computed f -> f
      in
      let f field =
        let label = get_label field in
        let%sub subform = M.form_for_field field in
        let%map.Computation subform = Form.Dynamic.error_hint subform in
        subform
        |> Form.Private.group_list
        |> Form.Private.suggest_label label
        |> attach_fieldname_to_error label
      in
      The_form_values.create { f }
    in
    let%map.Computation forms_per_field = To_forms.run form_values_per_field in
    let view =
      M.Typed_field.Packed.all
      |> List.map ~f:(fun { f = T field } ->
        Form.view (The_forms.find forms_per_field field))
      |> Form.View.Private.List
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

    val label_for_variant : [ `Inferred | `Computed of 'a Typed_variant.t -> string ]
    val form_for_variant : 'a Typed_variant.t -> 'a Form.t Computation.t
  end

  let make
        (type a)
        ?(picker = `Dropdown `Init_with_first_item)
        ?picker_attr
        (module M : S with type Typed_variant.derived_on = a)
    =
    let to_string =
      match M.label_for_variant with
      | `Inferred -> None
      | `Computed variant_to_string ->
        Some (fun ({ f = T field } : M.Typed_variant.Packed.t) -> variant_to_string field)
    in
    let%sub extra_attrs =
      match picker_attr with
      | None -> Bonsai.const []
      | Some attr -> Bonsai.pure List.return attr
    in
    let%sub picker =
      match picker with
      | `Dropdown default_case ->
        Elements.Dropdown.enumerable
          ?to_string
          ~init:
            (match default_case with
             | `Init_with_second_item ->
               (* This is not exposed, given only for make_optional below *)
               (match M.Typed_variant.Packed.all with
                | [ item ] (* Provided an empty type *) | _ :: item :: _ ->
                  `This (Value.return item)
                | [] ->
                  raise_s
                    [%message
                      "Got `Init_with_second_item even though M.Typed_variant.Packed.all \
                       is empty"])
             | `Init_with_first_item -> `First_item
             | `Init_with_empty -> `Empty)
          ~extra_attrs
          (module struct
            include M.Typed_variant.Packed

            let equal = [%compare.equal: t]
          end)
      | `Radio layout ->
        Elements.Radio_buttons.enumerable
          ~extra_attrs
          ~layout
          (module struct
            include M.Typed_variant.Packed

            let equal = [%compare.equal: t]
          end)
    in
    let picker_value =
      picker >>| Form.value_or_default ~default:(List.hd_exn M.Typed_variant.Packed.all)
    in
    let%sub inner =
      Bonsai.enum
        (module M.Typed_variant.Packed)
        ~match_:picker_value
        ~with_:(fun { f = T p } ->
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
    let%sub get_inner_form = Bonsai_extra.yoink inner in
    let%arr inner = inner
    and picker = picker
    and get_inner_form = get_inner_form in
    let view =
      Form.View.Private.Header_group
        { header_view = Form.view picker
        ; view = Form.view inner
        ; label = None
        ; tooltip = None
        ; error = None
        }
    in
    let value = Form.value inner in
    let set value =
      let constructor = M.Typed_variant.which value in
      let open Ui_effect.Let_syntax in
      (* sequence this so that the result of evaluating the picker is visible
         when setting the innermost form *)
      let%bind () = Form.set picker constructor in
      let%bind inner = get_inner_form in
      Form.set inner value
    in
    Form.Expert.create ~view ~value ~set
  ;;

  let make_optional
        (type a)
        ?(picker = `Dropdown)
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

      let label_for_variant : [ `Inferred | `Computed of 'a Typed_variant.t -> string ] =
        `Computed
          (fun (type a) (v : a Typed_variant.t) ->
             match v with
             | None -> empty_label
             | Some subvariant ->
               (match M.label_for_variant with
                | `Inferred -> M.Typed_variant.name subvariant
                | `Computed f -> f subvariant))
      ;;

      let form_for_variant : type a. a Typed_variant.t -> a Form.t Computation.t
        = function
          | None -> Computation.return (Form.return ())
          | Some subvariant -> M.form_for_variant subvariant
      ;;
    end
    in
    let picker =
      match picker with
      | `Dropdown -> `Dropdown `Init_with_first_item
      | `Radio `Vertical -> `Radio `Vertical
      | `Radio `Horizontal -> `Radio `Horizontal
    in
    make ~picker ?picker_attr (module Transformed)
    |> Computation.map
         ~f:
           (Form.project
              ~parse_exn:Transformed.some_val
              ~unparse:(Option.value_map ~f:Transformed.some ~default:Transformed.none))
  ;;
end
