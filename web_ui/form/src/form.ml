open! Core
open (Bonsai_web : module type of Bonsai_web with module View := Bonsai_web.View)
open Bonsai.Let_syntax

module View = struct
  module Private = View
  include View

  let to_vdom ?(custom = View.to_vdom) ?on_submit ?editable t =
    custom ?on_submit ?editable t
  ;;
end

module T = struct
  type ('read, 'write) unbalanced =
    { value : 'read Or_error.t
    ; view : View.t
    ; set : 'write -> unit Vdom.Effect.t
    }
  [@@deriving fields]

  type 'a t = ('a, 'a) unbalanced

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
    }

  let create
        ?(handle_enter = true)
        ?(button = Some "submit")
        ?(button_attr = Vdom.Attr.empty)
        ~f
        ()
    =
    { f; handle_enter; button_text = button; button_attr }
  ;;
end

let view_as_vdom ?on_submit ?(editable = `Yes_always) t =
  let on_submit =
    Option.map on_submit ~f:(fun { Submit.f; handle_enter; button_text; button_attr } ->
      let on_submit = t.value |> Result.ok |> Option.map ~f in
      { View.on_submit; handle_enter; button_text; button_attr })
  in
  View.to_vdom ?on_submit ~editable t.view
;;

let is_valid t = Or_error.is_ok t.value

let return value =
  { value = Ok value; view = View.Empty; set = (fun _ -> Ui_effect.Ignore) }
;;

let return_settable (type a) (module M : Bonsai.Model with type t = a) (value : a) =
  let%sub value, set_value = Bonsai.state (module M) ~default_model:value in
  let%arr value = value
  and set_value = set_value in
  { value = Ok value; view = View.Empty; set = set_value }
;;

let return_error error =
  { value = Error error; view = View.Empty; set = (fun _ -> Ui_effect.Ignore) }
;;

let map_error t ~f = { t with value = t.value |> Result.map_error ~f }

let map t ~f =
  let value = Or_error.map t.value ~f in
  { value; view = t.view; set = t.set }
;;

let contra_map t ~f =
  let set a = t.set (f a) in
  { value = t.value; view = t.view; set }
;;

let both a b =
  let value = Or_error.both a.value b.value in
  let view = View.concat a.view b.view in
  let set (ea, eb) = Ui_effect.Many [ a.set ea; b.set eb ] in
  { value; view; set }
;;

let both_for_profunctor a b =
  let value = Or_error.both a.value b.value in
  let view = View.concat a.view b.view in
  let set v = Ui_effect.Many [ a.set v; b.set v ] in
  { value; view; set }
;;

let label' label t = { t with view = View.set_label label t.view }
let label text = label' (Vdom.Node.text text)
let tooltip' tooltip t = { t with view = View.set_tooltip tooltip t.view }
let tooltip text = tooltip' (Vdom.Node.text text)
let group' label t = { t with view = View.group label t.view }
let group text = group' (Vdom.Node.text text)

let project' t ~parse ~unparse =
  let value =
    Or_error.bind t.value ~f:(fun a -> Or_error.try_with_join (fun () -> parse a))
  in
  let set a = t.set (unparse a) in
  { value; view = t.view; set }
;;

let validate t ~f =
  project' t ~parse:(fun a -> f a |> Or_error.map ~f:(fun () -> a)) ~unparse:Fn.id
;;

let project t ~parse_exn ~unparse = project' t ~parse:(fun a -> Ok (parse_exn a)) ~unparse

let optional' (type a b) (t : a t) ~parse ~unparse ~none : b option t =
  project' t ~parse ~unparse:(Option.value_map ~default:none ~f:unparse)
;;

let optional t ~is_some ~none =
  let parse a = if is_some a then Ok (Some a) else Ok None in
  optional' t ~parse ~unparse:Fn.id ~none
;;

module Record_builder = struct
  include Profunctor.Record_builder (struct
      type ('read, 'write) t = ('read, 'write) unbalanced

      let both = both_for_profunctor
      let map = map
      let contra_map = contra_map
    end)

  let label_of_field fieldslib_field =
    fieldslib_field
    |> Fieldslib.Field.name
    |> String.map ~f:(function
      | '_' -> ' '
      | other -> other)
    |> Vdom.Node.text
  ;;

  let attach_fieldname_to_error t fieldslib_field =
    Result.map_error
      t.value
      ~f:(Error.tag ~tag:(sprintf "in field %s" (Fieldslib.Field.name fieldslib_field)))
  ;;

  (* This function "overrides" the [field] function inside of Record_builder
     by adding a label *)
  let field t fieldslib_field =
    let label = label_of_field fieldslib_field in
    let value = attach_fieldname_to_error t fieldslib_field in
    let view = View.suggest_label label t.view in
    let with_label = { t with view; value } in
    field with_label fieldslib_field
  ;;

  let build_for_record a =
    let t = build_for_record a in
    { t with
      view = View.Group { label = None; tooltip = None; view = t.view; error = None }
    }
  ;;
end

module Dynamic = struct
  let with_default default form =
    let open Bonsai.Let_syntax in
    let%sub is_loaded, set_is_loaded = Bonsai.state (module Bool) ~default_model:false in
    let%sub () =
      Bonsai.Edge.lifecycle
        ~on_activate:
          (let%map default = default
           and is_loaded = is_loaded
           and set_is_loaded = set_is_loaded
           and form = form in
           if not is_loaded
           then Ui_effect.Many [ set form default; set_is_loaded true ]
           else Ui_effect.Ignore)
        ()
    in
    return form
  ;;

  let with_default_always default form =
    let open Bonsai.Let_syntax in
    let%sub () =
      Bonsai.Edge.lifecycle
        ~on_activate:
          (let%map default = default
           and form = form in
           set form default)
        ()
    in
    return form
  ;;

  let error_hint t =
    let f view ~error =
      let if_not_none value ~f = Option.value_map value ~default:Fn.id ~f in
      view |> if_not_none error ~f:View.suggest_error
    in
    let t =
      let%sub error_hovered = Bonsai.state (module Bool) ~default_model:false in
      let%sub error_clicked = Bonsai.state (module Bool) ~default_model:false in
      let%arr t = t
      and is_error_hovered, set_error_hovered = error_hovered
      and is_clicked, set_clicked = error_clicked in
      let on_click = set_clicked (not is_clicked) in
      let error_details =
        Option.map (Result.error t.value) ~f:(fun error ->
          { View.Error_details.is_viewing = is_error_hovered || is_clicked
          ; error
          ; on_mouse_over = set_error_hovered true
          ; on_mouse_out = set_error_hovered false
          ; on_click
          ; is_toggled = is_clicked
          })
      in
      { t with view = f t.view ~error:error_details }
    in
    t
  ;;

  let collapsible_group ?(starts_open = true) label t =
    let%sub open_state = Bonsai.state (module Bool) ~default_model:starts_open in
    let%arr is_open, set_is_open = open_state
    and label = label
    and t = t in
    let label =
      Vdom.Node.div
        ~attr:
          (Vdom.Attr.many_without_merge
             [ Vdom.Attr.on_click (fun _ -> set_is_open (not is_open))
             ; Vdom.Attr.style
                 Css_gen.(
                   user_select `None @> Css_gen.create ~field:"cursor" ~value:"pointer")
             ])
        [ Vdom.Node.text (if is_open then "▾ " ^ label else "► " ^ label) ]
    in
    let form = group' label t in
    let view =
      match is_open, form.view with
      | false, Group { label; tooltip; _ } ->
        View.Group { label; tooltip; view = Empty; error = None }
      | _, other -> other
    in
    { form with view }
  ;;

  let on_change
        (type a)
        ?(on_error = Value.return (Fn.const Ui_effect.Ignore))
        (module M : Bonsai.Model with type t = a)
        ~f
        value_to_watch
    =
    let module M_or_error = struct
      type t = M.t Or_error.t [@@deriving equal, sexp]
    end
    in
    let callback =
      let%map f = f
      and on_error = on_error in
      function
      | Error e -> on_error e
      | Ok new_value -> f new_value
    in
    Bonsai.Edge.on_change (module M_or_error) (value_to_watch >>| value) ~callback
  ;;

  let validate_via_effect
        (type a)
        (module Input : Bonsai.Model with type t = a)
        (t : a t Bonsai.Value.t)
        ~f
    =
    let module Validated = struct
      type t = Input.t Or_error.t [@@deriving sexp, equal]
    end
    in
    match%sub t >>| value with
    | Error _ -> Bonsai.read t
    | Ok value ->
      let%sub validation =
        Bonsai.Edge.Poll.(
          effect_on_change
            (module Input)
            (module Validated)
            Starting.empty
            value
            ~effect:
              (let%map f = f in
               fun a ->
                 match%map.Effect f a with
                 | Ok () -> Ok a
                 | Error e -> Error e))
      in
      let%arr t = t
      and validation = validation in
      validate t ~f:(fun a ->
        match validation with
        | Some (Ok x) when Input.equal a x -> Ok ()
        | None | Some (Ok _) -> Error (Error.of_string "validating...")
        | Some (Error e) -> Error e)
  ;;

  module Record_builder = struct
    include Profunctor.Record_builder (struct
        type ('read, 'write) t = ('read, 'write) unbalanced Value.t

        let both a b = Value.map2 a b ~f:both_for_profunctor
        let map a ~f = Value.map a ~f:(map ~f)
        let contra_map a ~f = Value.map a ~f:(contra_map ~f)
      end)

    let field ?(group_lists = true) t fieldslib_field =
      let label = Record_builder.label_of_field fieldslib_field in
      let with_label =
        let%map t = t in
        let view =
          (if group_lists then View.group_list t.view else t.view)
          |> View.suggest_label label
        in
        let value = Record_builder.attach_fieldname_to_error t fieldslib_field in
        { t with view; value }
      in
      field with_label fieldslib_field
    ;;

    let build_for_record creator =
      let%arr t = build_for_record creator in
      { t with
        view = View.Group { label = None; tooltip = None; view = t.view; error = None }
      }
    ;;
  end
end

module Expert = struct
  let create = Fields_of_unbalanced.create
end

module Private = struct
  let group_list t = { t with view = View.group_list t.view }

  let suggest_label label t =
    { t with view = View.suggest_label (Vdom.Node.text label) t.view }
  ;;
end
