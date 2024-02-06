open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Form = Form_manual
module Elements = Elements_manual

let sexp_to_pretty_string sexp_of_t t =
  t
  |> sexp_of_t
  |> Sexp.to_string_mach
  |> String.lowercase
  |> String.map ~f:(function
       | '(' | ')' | '-' | '_' -> ' '
       | o -> o)
;;

module Record = struct
  module type S = sig
    module Typed_field : Typed_fields_lib.S

    type field_view
    type resulting_view

    val form_for_field : 'a Typed_field.t -> ('a, field_view) Form.t Computation.t

    type form_of_field_fn =
      { f : 'a. 'a Typed_field.t -> ('a, field_view) Form.t Value.t }

    val finalize_view : form_of_field_fn -> resulting_view Computation.t
  end

  module type S' = sig
    include S

    val augment_error : 'a Typed_field.t -> Error.t -> Error.t
  end

  module type S_for_table = sig
    module Typed_field : Typed_fields_lib.S

    val label_for_field
      : [ `Inferred
        | `Computed of 'a Typed_field.t -> string
        | `Dynamic of (Typed_field.Packed.t -> string) Value.t
        ]

    val form_for_field : 'a Typed_field.t -> ('a, Vdom.Node.t) Form.t Computation.t
  end

  let make_shared
    (type a field_view resulting_view)
    (module M : S'
      with type Typed_field.derived_on = a
       and type resulting_view = resulting_view
       and type field_view = field_view)
    =
    let module Form_value = struct
      type 'a t = ('a, field_view) Form.t Computation.t
    end
    in
    let module App = struct
      include Computation

      type 'a s = ('a, field_view) Form.t

      let translate = Fn.id
    end
    in
    let module Form' = struct
      type 'a t = ('a, field_view) Form.t
    end
    in
    let module The_form_values = Typed_field_map.Make (M.Typed_field) (Form_value) in
    let module The_forms = Typed_field_map.Make (M.Typed_field) (Form') in
    let module The_results = Typed_field_map.Make (M.Typed_field) (Or_error) in
    let module To_forms = The_form_values.As_applicative.To_other_map (App) (The_forms) in
    let form_values_per_field =
      let f field =
        let%sub subform = M.form_for_field field in
        let%arr subform = subform in
        Form.map_error subform ~f:(M.augment_error field)
      in
      The_form_values.create { f }
    in
    let%sub forms_per_field = To_forms.run form_values_per_field in
    let lookup field =
      let%map forms_per_field = forms_per_field in
      The_forms.find forms_per_field field
    in
    let%sub view = M.finalize_view { f = lookup } in
    let%arr forms_per_field = forms_per_field
    and view = view in
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
    { Form.view; value; set }
  ;;

  let make
    (type a field_view resulting_view)
    (module M : S
      with type Typed_field.derived_on = a
       and type resulting_view = resulting_view
       and type field_view = field_view)
    =
    make_shared
      (module struct
        include M

        let augment_error field error =
          Error.tag error ~tag:("in field " ^ M.Typed_field.name field)
        ;;
      end)
  ;;

  let make_without_tagging_errors
    (type a field_view resulting_view)
    (module M : S
      with type Typed_field.derived_on = a
       and type resulting_view = resulting_view
       and type field_view = field_view)
    =
    make_shared
      (module struct
        include M

        (* Don't augment the error *)
        let augment_error _ error = error
      end)
  ;;

  let make_table (type a) (module M : S_for_table with type Typed_field.derived_on = a) =
    let module View_value_by_field =
      Typed_field_map.Make
        (M.Typed_field)
        (struct
          type 'a t = Vdom.Node.t Value.t
        end)
    in
    let module View_by_field =
      Typed_field_map.Make
        (M.Typed_field)
        (struct
          type 'a t = Vdom.Node.t
        end)
    in
    let module App = struct
      include Value

      type 'a s = Vdom.Node.t

      let translate = Fn.id
    end
    in
    let module To_views =
      View_value_by_field.As_applicative.To_other_map (App) (View_by_field)
    in
    let record =
      make_without_tagging_errors
        (module struct
          include M

          type field_view = Vdom.Node.t
          type resulting_view = View_by_field.t

          type form_of_field_fn =
            { f : 'a. 'a Typed_field.t -> ('a, field_view) Form.t Value.t }

          let finalize_view { f } =
            let f : type a. a Typed_field.t -> Vdom.Node.t Value.t =
              fun field ->
              let%map form = f field in
              Form.view form
            in
            return (To_views.run (View_value_by_field.create { f }))
          ;;
        end)
    in
    let%sub many_records = Elements.Multiple.list record in
    let%sub render_table =
      let%sub to_string =
        return
          (match M.label_for_field with
           | `Inferred ->
             Value.return (fun t ->
               sexp_to_pretty_string M.Typed_field.Packed.sexp_of_t t)
           | `Computed field_to_string ->
             Value.return (fun ({ f = T field } : M.Typed_field.Packed.t) ->
               field_to_string field)
           | `Dynamic field_to_string -> field_to_string)
      in
      let%sub cols =
        let%arr to_string = to_string in
        let remove_column =
          View.Table.Col.make
            "Remove"
            ~get:(fun (_map, remove) -> remove)
            ~render:(fun theme remove ->
              View.hbox
                ~main_axis_alignment:Center
                [ View.button theme "X" ~on_click:remove ])
        in
        (M.Typed_field.Packed.all
         |> List.map ~f:(fun ({ f = T field } as packed) ->
              View.Table.Col.make
                (to_string packed)
                ~get:(fun (map, _remove) -> View_by_field.find map field)
                ~render:(fun _theme vdom -> vdom)))
        @ [ remove_column ]
      in
      let%sub theme = View.Theme.current in
      let%arr cols = cols
      and theme = theme in
      fun ({ items; add_element } : (a, View_by_field.t) Elements.Multiple.t) ->
        let items = List.map items ~f:(fun { form; remove } -> Form.view form, remove) in
        View.vbox
          [ View.Table.render theme cols items
          ; View.button theme "+" ~on_click:add_element
          ]
    in
    let%arr many_records = many_records
    and render_table = render_table in
    Form.map_view many_records ~f:render_table
  ;;
end

module Variant = struct
  module type S = sig
    (**  This module should be generated by deriving [typed_variants] on a
         sum type. *)
    module Typed_variant : Typed_variants_lib.S

    type picker_view
    type variant_view
    type resulting_view

    val form_for_picker : (Typed_variant.Packed.t, picker_view) Form.t Computation.t
    val form_for_variant : 'a Typed_variant.t -> ('a, variant_view) Form.t Computation.t

    val finalize_view
      :  picker_view
      -> ('a Typed_variant.t * ('a, variant_view) Form.t) Or_error.t
      -> resulting_view
  end

  module Packed_set_form = struct
    type ('a, 'view) t =
      | T :
          { form : (('a, 'cmp) Set.t, 'view) Form.t
          ; comparator : ('a, 'cmp) Bonsai.comparator
          }
          -> ('a, 'view) t
  end

  module type S_set = sig
    include Comparator.S

    (**  This module should be generated by deriving [typed_variants] on a
         sum type. *)
    module Typed_variant : Typed_variants_lib.S with type derived_on = t

    type variant_view
    type resulting_view

    val sexp_of_variant_argument
      : [ `Use_sexp_of_variant | `Custom of 'a Typed_variant.t -> 'a -> Sexp.t ]

    val form_for_variant
      :  'a Typed_variant.t
      -> ('a, 'cmp) Bonsai.comparator
      -> (('a, 'cmp) Set.t, variant_view) Form.t Computation.t

    type form_of_variant_fn =
      { f : 'a 'cmp. 'a Typed_variant.t -> ('a, variant_view) Packed_set_form.t Value.t }

    val finalize_view : form_of_variant_fn -> resulting_view Computation.t
  end

  module type S_set' = sig
    include S_set

    val augment_error : 'a Typed_variant.t -> Error.t -> Error.t
  end

  let make
    (type a view picker_view variant_view)
    (module M : S
      with type Typed_variant.derived_on = a
       and type resulting_view = view
       and type variant_view = variant_view
       and type picker_view = picker_view)
    =
    let%sub { Form.value = picker_value; set = set_picker_value; view = picker_view } =
      M.form_for_picker
    in
    let module Packed_with_form = struct
      type t =
        | T :
            { variant : 'a M.Typed_variant.t
            ; form : ('a, M.variant_view) Form.t
            }
            -> t
    end
    in
    let%sub view, inner =
      match%sub picker_value with
      | Error e ->
        let%arr e = e
        and picker_view = picker_view in
        let view = M.finalize_view picker_view (Error e) in
        view, Error e
      | Ok picker_value ->
        let%sub inner =
          Bonsai.enum
            (module M.Typed_variant.Packed)
            ~match_:picker_value
            ~with_:(function
              | { f = T variant } ->
                let%map.Computation form = M.form_for_variant variant in
                Packed_with_form.T { variant; form })
        in
        let%arr (T { variant = picker_value; form = inner }) = inner
        and picker_view = picker_view in
        let view = M.finalize_view picker_view (Ok (picker_value, inner)) in
        let projected =
          let parse_exn content = M.Typed_variant.create picker_value content in
          let unparse kind =
            match M.Typed_variant.get picker_value kind with
            | None ->
              let expected = M.Typed_variant.Packed.pack picker_value in
              let found = M.Typed_variant.which kind in
              raise_s
                [%message
                  "BUG"
                    [%here]
                    (expected : M.Typed_variant.Packed.t)
                    (found : M.Typed_variant.Packed.t)]
            | Some v -> v
          in
          Form.project inner ~parse_exn ~unparse
        in
        view, Ok projected
    in
    let%sub get_inner_form = Bonsai.yoink inner in
    let%arr inner = inner
    and set_picker_value = set_picker_value
    and get_inner_form = get_inner_form
    and view = view in
    let set value =
      let constructor = M.Typed_variant.which value in
      let open Ui_effect.Let_syntax in
      (* sequence this so that the result of evaluating the picker is visible
         when setting the innermost form *)
      let%bind () = set_picker_value constructor in
      match%bind.Effect get_inner_form with
      | Active (Ok inner) -> Form.set inner value
      | Active (Error e) -> Effect.print_s [%sexp "BUG", [%here], (e : Error.t)]
      | Inactive -> Effect.never
    in
    let value =
      match inner with
      | Error e -> Error e
      | Ok form -> form.value
    in
    { Form.view; value; set }
  ;;

  let make_set_shared
    (type a cmp resulting_view)
    (module M : S_set'
      with type t = a
       and type comparator_witness = cmp
       and type resulting_view = resulting_view)
    : ((a, cmp) Set.t, resulting_view) Form.t Computation.t
    =
    let module Packed = struct
      include M.Typed_variant.Packed
      include Comparable.Make_plain (M.Typed_variant.Packed)
    end
    in
    let module Form_with_comparator = struct
      type 'a t =
        | T :
            { cmp : ('a, 'cmp) Bonsai.comparator
            ; form : (('a, 'cmp) Set.t, M.variant_view) Form.t
            ; wrap : ('a, 'cmp) Set.t -> (a, cmp) Set.t
            ; unwrap_exn : (a, cmp) Set.t -> ('a, 'cmp) Set.t
            }
            -> 'a t
    end
    in
    let module Form_computations =
      Typed_field_map.Make
        (M.Typed_variant)
        (struct
          type 'a t = 'a Form_with_comparator.t Computation.t
        end)
    in
    let module Forms = Typed_field_map.Make (M.Typed_variant) (Form_with_comparator) in
    let module To_forms =
      Form_computations.As_applicative.To_other_map
        (struct
          include Computation

          type 'a s = 'a Form_with_comparator.t

          let translate = Fn.id
        end)
        (Forms)
    in
    let create_form_with_comparator (type a) (variant : a M.Typed_variant.t) =
      let wrap = M.Typed_variant.create variant in
      (* This is safe because this function only ever gets types of this argument. *)
      let unwrap_exn v = M.Typed_variant.get variant v |> Option.value_exn in
      let module Comparator = struct
        module T = struct
          type t = a

          let sexp_of_t =
            match M.sexp_of_variant_argument with
            | `Use_sexp_of_variant -> fun a -> M.comparator.sexp_of_t (wrap a)
            | `Custom f -> f variant
          ;;

          let compare = Comparable.lift ~f:wrap M.comparator.compare
        end

        include T
        include Comparator.Make (T)
      end
      in
      let%sub form = M.form_for_variant variant (module Comparator) in
      let%arr form = form in
      let form = Form.map_error form ~f:(M.augment_error variant) in
      Form_with_comparator.T
        { cmp = (module Comparator)
        ; wrap = Set.map (module M) ~f:wrap
        ; form
        ; unwrap_exn = Set.map (module Comparator) ~f:unwrap_exn
        }
    in
    let form_computations =
      Form_computations.create { f = create_form_with_comparator }
    in
    let%sub forms = To_forms.run form_computations in
    let%sub view =
      let lookup variant =
        let%map forms = forms in
        let (Form_with_comparator.T { form; cmp; _ }) = Forms.find forms variant in
        Packed_set_form.T { form; comparator = cmp }
      in
      M.finalize_view { f = lookup }
    in
    let%arr forms = forms
    and view = view in
    let value =
      List.map Packed.all ~f:(fun { Packed.f = T variant } ->
        let (Form_with_comparator.T { form; wrap; _ }) = Forms.find forms variant in
        Form.value form |> Or_error.map ~f:wrap)
      |> Or_error.all
      |> Or_error.map ~f:(Set.union_list (module M))
    in
    let set set =
      let values =
        Set.fold set ~init:Packed.Map.empty ~f:(fun map value ->
          Map.update map (M.Typed_variant.which value) ~f:(function
            | None -> Set.singleton (module M) value
            | Some set -> Set.add set value))
      in
      Map.to_alist values
      |> List.map ~f:(fun ({ f = T variant }, values) ->
           let (Form_with_comparator.T { form; unwrap_exn; _ }) =
             Forms.find forms variant
           in
           Form.set form (unwrap_exn values))
      |> Ui_effect.all_unit
    in
    { Form.view; value; set }
  ;;

  let make_set_without_tagging_errors
    (type a cmp variant_view resulting_view)
    (module M : S_set
      with type t = a
       and type comparator_witness = cmp
       and type resulting_view = resulting_view
       and type variant_view = variant_view)
    =
    make_set_shared
      (module struct
        include M

        (* Don't augment the error *)
        let augment_error _ error = error
      end)
  ;;

  let make_set
    (type a cmp variant_view resulting_view)
    (module M : S_set
      with type t = a
       and type comparator_witness = cmp
       and type resulting_view = resulting_view
       and type variant_view = variant_view)
    =
    make_set_shared
      (module struct
        include M

        let augment_error field error =
          Error.tag error ~tag:("in field " ^ M.Typed_variant.name field)
        ;;
      end)
  ;;
end
