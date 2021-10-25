open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

module Record = struct
  module type S = sig
    module Typed_field : Typed_fields_lib.S

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
      let f field =
        let%sub subform = M.form_for_field field in
        let%map.Computation subform = Form.Dynamic.error_hint subform in
        subform
        |> Form.label (M.Typed_field.name field)
        |> attach_fieldname_to_error (M.Typed_field.name field)
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

    val form_for_variant : 'a Typed_variant.t -> 'a Form.t Computation.t
  end

  let make (type a) (module M : S with type Typed_variant.derived_on = a) =
    let%sub picker =
      Elements.Dropdown.enumerable
        [%here]
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
    let view = Form.View.Private.List [ Form.view picker; Form.view inner ] in
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
end
