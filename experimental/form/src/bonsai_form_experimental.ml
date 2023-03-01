module Combine = Combine
module Product = Product
module Validated = Validated
open! Core
open! Import

type ('input, 'result, 'parsed) t =
  default:'parsed
  -> ( 'input
     , ('result Or_error.t Product.With_view.t, 'parsed) Product.t )
       Bonsai.Arrow_deprecated.t

let form_element (type t) (module M : Bonsai.Model with type t = t) here ~(default : t) =
  Bonsai.Arrow_deprecated.state_machine
    (module M)
    (module M)
    here
    ~default_model:default
    ~apply_action:(fun ~inject:_ ~schedule_event:_ _input _old_model new_model ->
      new_model)
;;

let form_element_dynamic_model
      (type t)
      (module M : Bonsai.Model with type t = t)
      ~(default : t Value.t)
  =
  Bonsai_extra.state_dynamic_model (module M) ~model:(`Given default)
;;

let text_input ~default =
  let%map.Bonsai.Arrow_deprecated value, inject =
    form_element (module String) [%here] ~default
  in
  let view =
    Vdom_input_widgets.Entry.text
      ~merge_behavior:Legacy_dont_merge
      ~value:(Some value)
      ~on_input:(function
        | Some s -> inject s
        | None -> inject "")
      ()
  in
  { Product.value = { Product.With_view.value; view }; set = inject }
;;

let textarea_input ~default =
  let%map.Bonsai.Arrow_deprecated value, inject =
    form_element (module String) [%here] ~default
  in
  let view =
    Vdom_input_widgets.Entry.text_area
      ~merge_behavior:Legacy_dont_merge
      ~value
      ~on_input:inject
      ()
  in
  { Product.value = { Product.With_view.value; view }; set = inject }
;;

let checkbox_input ?(label = "") ~default () =
  let%map.Bonsai.Arrow_deprecated value, inject =
    form_element (module Bool) [%here] ~default
  in
  let view =
    Vdom_input_widgets.Checkbox.simple
      ~merge_behavior:Legacy_dont_merge
      ~is_checked:value
      ~on_toggle:(inject (not value))
      ~label
      ()
  in
  { Product.value = { Product.With_view.value; view }; set = inject }
;;

let inject_or_ignore inject_model = function
  | Some model -> inject_model model
  | None -> Vdom.Effect.Ignore
;;

let date_picker_with_bad_user_experience ~default =
  let%map.Bonsai.Arrow_deprecated value, inject =
    form_element (module Date) [%here] ~default
  in
  let view =
    Vdom_input_widgets.Entry.date
      ~merge_behavior:Legacy_dont_merge
      ~value:(Some value)
      ~on_input:(inject_or_ignore inject)
      ()
  in
  { Product.value = { Product.With_view.value; view }; set = inject }
;;

let date_picker ~default =
  let%map.Bonsai.Arrow_deprecated value, inject =
    form_element
      (module struct
        type t = Date.t option [@@deriving equal, sexp]
      end)
      [%here]
      ~default
  in
  let view =
    Vdom_input_widgets.Entry.date
      ~merge_behavior:Legacy_dont_merge
      ~value
      ~on_input:inject
      ()
  in
  { Product.value = { Product.With_view.value; view }; set = inject }
;;

module Dropdown = struct
  module type Equal = sig
    type t [@@deriving equal, sexp]

    val to_string : t -> string
  end

  let of_input (type t) (module M : Equal with type t = t) ~default =
    let%map.Bonsai.Arrow_deprecated value, inject =
      form_element (module M) [%here] ~default
    and all = Bonsai.Arrow_deprecated.input in
    let view =
      Vdom_input_widgets.Dropdown.of_values
        ~merge_behavior:Legacy_dont_merge
        (module M)
        all
        ~selected:value
        ~on_change:inject
    in
    { Product.value = { Product.With_view.value; view }; set = inject }
  ;;

  let of_input_opt (type t) (module M : Equal with type t = t) ~default =
    let%map.Bonsai.Arrow_deprecated value, inject =
      form_element
        (module struct
          type t = M.t option [@@deriving sexp, equal]
        end)
        [%here]
        ~default
    and all = Bonsai.Arrow_deprecated.input in
    let view =
      Vdom_input_widgets.Dropdown.of_values_opt
        ~merge_behavior:Legacy_dont_merge
        (module M)
        all
        ~selected:value
        ~on_change:inject
    in
    { Product.value = { Product.With_view.value; view }; set = inject }
  ;;

  module type Enum = sig
    type t [@@deriving enumerate, equal, sexp]

    val to_string : t -> string
  end

  let of_enum (type t) (module M : Enum with type t = t) ~default =
    let%map.Bonsai.Arrow_deprecated value, inject =
      form_element (module M) [%here] ~default
    in
    let view =
      Vdom_input_widgets.Dropdown.of_enum
        ~merge_behavior:Legacy_dont_merge
        ~selected:value
        ~on_change:inject
        (module M)
    in
    { Product.value = { Product.With_view.value; view }; set = inject }
  ;;

  let of_enum_dynamic_model (type t) (module M : Enum with type t = t) ~default =
    let open Bonsai.Let_syntax in
    let%sub value, inject = form_element_dynamic_model (module M) ~default in
    let%arr value = value
    and inject = inject in
    let view =
      Vdom_input_widgets.Dropdown.of_enum
        ~merge_behavior:Legacy_dont_merge
        ~selected:value
        ~on_change:inject
        (module M)
    in
    { Product.value = { Product.With_view.value; view }; set = inject }
  ;;

  let of_enum_opt (type t) (module M : Enum with type t = t) ~default =
    let%map.Bonsai.Arrow_deprecated value, inject =
      form_element
        (module struct
          type t = M.t option [@@deriving enumerate, sexp, equal]
        end)
        [%here]
        ~default
    in
    let view =
      Vdom_input_widgets.Dropdown.of_enum_opt
        ~merge_behavior:Legacy_dont_merge
        ~selected:value
        ~on_change:inject
        (module M)
    in
    { Product.value = { Product.With_view.value; view }; set = inject }
  ;;
end
