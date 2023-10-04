open! Core
open (Bonsai_web : module type of Bonsai_web with module View := Bonsai_web.View)
open Bonsai.Let_syntax
open Bonsai_web_ui_form2

module T = struct
  type nonrec 'a t = ('a, View.t) Bonsai_web_ui_form2.t

  let value t = t.value
  let view t = t.view
  let set t = t.set
  let create ~value ~view ~set = { value; view; set }
  let value_or_default t ~default = t |> value |> Or_error.ok |> Option.value ~default

  let normalize { value; set; view = _ } =
    match value with
    | Ok value -> set value
    | Error _ -> Ui_effect.Ignore
  ;;
end

include T

module Submit = struct
  type 'a t =
    { f : 'a -> unit Ui_effect.t
    ; handle_enter : bool
    ; button_text : string option
    ; button_attr : Vdom.Attr.t
    ; button_location : View.button_location
    }

  let create
    ?(handle_enter = true)
    ?(button = Some "submit")
    ?(button_attr = Vdom.Attr.empty)
    ?(button_location = `After)
    ~f
    ()
    =
    { f; handle_enter; button_text = button; button_attr; button_location }
  ;;
end

module View = View

let view_as_vdom ?theme ?on_submit ?editable t =
  let on_submit =
    Option.map
      on_submit
      ~f:(fun { Submit.f; handle_enter; button_text; button_attr; button_location } ->
      { View.on_submit = Option.map ~f (Or_error.ok t.value)
      ; handle_enter
      ; button_text
      ; button_attr
      ; button_location
      })
  in
  View.to_vdom ?theme ?on_submit ?editable t.view
;;

let label' label t = { t with view = View.set_label label t.view }
let label text = label' (Vdom.Node.text text)
let tooltip' tooltip t = { t with view = View.set_tooltip tooltip t.view }
let tooltip text = tooltip' (Vdom.Node.text text)

module For_profunctor = struct
  type ('read, 'write) unbalanced =
    { value : 'read Or_error.t
    ; view : View.t
    ; set : 'write -> unit Vdom.Effect.t
    }

  let unbalanced_of_t ({ value; view; set } : _ t) : _ unbalanced = { value; view; set }

  type ('read, 'write) t =
    | Return :
        { name : string
        ; form : ('read, 'write) unbalanced
        }
        -> ('read, 'write) t
    | Both : ('a, 'write) t * ('b, 'write) t -> ('a * 'b, 'write) t
    | Map : ('a, 'write) t * ('a -> 'b) -> ('b, 'write) t
    | Contra_map : ('read, 'a) t * ('b -> 'a) -> ('read, 'b) t

  let both a b = Both (a, b)
  let map a ~f = Map (a, f)
  let contra_map a ~f = Contra_map (a, f)

  let rec finalize_view
    : type read write.
      (read, write) t -> read Or_error.t * (write -> unit Effect.t) * View.field list
    = function
    | Return { name; form } ->
      form.value, form.set, [ { View.field_name = name; field_view = form.view } ]
    | Map (form, f) ->
      let value, set, fields = finalize_view form in
      Or_error.map value ~f, set, fields
    | Contra_map (form, g) ->
      let value, set, fields = finalize_view form in
      value, (fun x -> Effect.lazy_ (lazy (set (g x)))), fields
    | Both (a, b) ->
      let a_value, a_set, a_fields = finalize_view a in
      let b_value, b_set, b_fields = finalize_view b in
      let value = Or_error.both a_value b_value in
      let set t = Effect.lazy_ (lazy (Effect.Many [ a_set t; b_set t ])) in
      let fields = a_fields @ b_fields in
      value, set, fields
  ;;
end

module Record_builder = struct
  include Profunctor.Record_builder (For_profunctor)

  let label_of_field fieldslib_field =
    fieldslib_field
    |> Fieldslib.Field.name
    |> String.map ~f:(function
         | '_' -> ' '
         | other -> other)
  ;;

  let attach_fieldname_to_error t fieldslib_field =
    Result.map_error
      t.value
      ~f:(Error.tag ~tag:(sprintf "in field %s" (Fieldslib.Field.name fieldslib_field)))
  ;;

  (* This function "overrides" the [field] function inside of Record_builder
     by adding a label *)
  let field' t ~label_of_field fieldslib_field =
    let value = attach_fieldname_to_error t fieldslib_field in
    let with_label =
      For_profunctor.Return
        { name = label_of_field fieldslib_field
        ; form = { (For_profunctor.unbalanced_of_t t) with value }
        }
    in
    field with_label fieldslib_field
  ;;

  let field = field' ~label_of_field

  let build_for_record a =
    let value, set, fields = For_profunctor.finalize_view (build_for_record a) in
    { value; set; view = View.record fields }
  ;;
end

module Expert = struct
  let create = create
end

include Bonsai_web_ui_form2

module Dynamic = struct
  include Dynamic

  let error_hint t =
    let%arr t = t in
    match Result.error t.value with
    | Some err -> { t with view = View.suggest_error err t.view }
    | None -> t
  ;;

  let collapsible_group ?(starts_open = true) label t =
    let%sub open_state = Bonsai.toggle ~default_model:starts_open in
    let%arr is_open, toggle_is_open = open_state
    and label = label
    and t = t in
    let label =
      Vdom.Node.div
        ~attrs:
          [ Vdom.Attr.on_click (fun _ -> toggle_is_open)
          ; Vdom.Attr.style
              Css_gen.(
                user_select `None @> Css_gen.create ~field:"cursor" ~value:"pointer")
          ]
        [ Vdom.Node.text (if is_open then "▾ " ^ label else "► " ^ label) ]
    in
    let view =
      match is_open with
      | false -> View.collapsible ~label ~state:(Collapsed None)
      | true -> View.collapsible ~label ~state:(Expanded t.view)
    in
    { t with view }
  ;;

  module Record_builder = struct
    include Profunctor.Record_builder (struct
      type ('read, 'write) t = ('read, 'write) For_profunctor.t Value.t

      let both a b = Value.map2 a b ~f:For_profunctor.both
      let map a ~f = Value.map a ~f:(For_profunctor.map ~f)
      let contra_map a ~f = Value.map a ~f:(For_profunctor.contra_map ~f)
    end)

    let field' t ~label_of_field fieldslib_field =
      let for_profunctor =
        let%map t = t in
        let t =
          { t with value = Record_builder.attach_fieldname_to_error t fieldslib_field }
        in
        For_profunctor.Return
          { name = label_of_field fieldslib_field
          ; form = For_profunctor.unbalanced_of_t t
          }
      in
      field for_profunctor fieldslib_field
    ;;

    let field = field' ~label_of_field:Record_builder.label_of_field

    let build_for_record creator =
      let%arr t = build_for_record creator in
      let value, set, fields = For_profunctor.finalize_view t in
      { value; set; view = View.record fields }
    ;;
  end
end

module Private = struct
  let suggest_label label t =
    { t with view = View.suggest_label' (Vdom.Node.text label) t.view }
  ;;
end

include T

let return ?sexp_of_t value = map_view (return ?sexp_of_t value) ~f:(fun () -> View.empty)

let return_settable ?sexp_of_model ~equal value =
  let%sub form = return_settable ?sexp_of_model ~equal value in
  let%arr form = form in
  map_view form ~f:(fun () -> View.empty)
;;

let return_error error = map_view (return_error error) ~f:(fun () -> View.empty)
let both a b = map_view (both a b) ~f:(fun (a, b) -> View.tuple [ a; b ])
let all forms = map_view (all forms) ~f:View.tuple
let all_map forms = map_view (all_map forms) ~f:(fun views -> View.tuple (Map.data views))
let project form ~parse_exn ~unparse = project form ~parse_exn ~unparse
let project' form ~parse ~unparse = project' form ~parse ~unparse
let validate form ~f = validate form ~f
