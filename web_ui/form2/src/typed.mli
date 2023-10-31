open! Core
open! Bonsai_web

(** The functions in this module can be hard to understand
    Please look at the examples in lib/bonsai/examples/forms/typed.ml *)

module Record : sig
  module type S = sig
    module Typed_field : Typed_fields_lib.S

    type field_view
    type resulting_view

    (** For each of the fields in your record, you need to provide a form
        component which produces values of that type. *)
    val form_for_field : 'a Typed_field.t -> ('a, field_view) Form.t Computation.t

    type form_of_field_fn =
      { f : 'a. 'a Typed_field.t -> ('a, field_view) Form.t Value.t }

    (** Once each of the forms for your record have been created, you need to combine
        them into a final view of your choosing. *)
    val finalize_view : form_of_field_fn -> resulting_view Computation.t
  end

  val make
    :  (module S
          with type Typed_field.derived_on = 'a
           and type resulting_view = 'view
           and type field_view = _)
    -> ('a, 'view) Form.t Computation.t

  (** Like [make], but don't tag fields' errors with their name. This can be useful when
      you present a label that doesn't match the field's name. If you still want to tag
      errors on your fields, you can do so by manually calling [Form.map_error] in your
      [form_for_field] function. *)
  val make_without_tagging_errors
    :  (module S
          with type Typed_field.derived_on = 'a
           and type resulting_view = 'view
           and type field_view = _)
    -> ('a, 'view) Form.t Computation.t

  module type S_for_table = sig
    module Typed_field : Typed_fields_lib.S

    (** The label to use for each column of the table. *)
    val label_for_field
      : [ `Inferred
        | `Computed of 'a Typed_field.t -> string
        | `Dynamic of (Typed_field.Packed.t -> string) Value.t
        ]

    (** For each of the fields in your record, you need to provide a form
        component which produces values of that type. *)
    val form_for_field : 'a Typed_field.t -> ('a, Vdom.Node.t) Form.t Computation.t
  end

  (** Creates a table whose columns are the fields of the record, and whose rows
      correspond to list items. *)
  val make_table
    :  (module S_for_table with type Typed_field.derived_on = 'a)
    -> ('a list, Vdom.Node.t) Form.t Computation.t
end

module Variant : sig
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

  (** [picker_attr] will be added to the picker for selecting a variant constructor.
      Default appearance is a dropdown, but it can be changed through [?picker]. *)
  val make
    :  (module S
          with type Typed_variant.derived_on = 'a
           and type picker_view = _
           and type variant_view = _
           and type resulting_view = 'view)
    -> ('a, 'view) Form.t Computation.t
end
