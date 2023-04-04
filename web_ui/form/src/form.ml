open! Core
open (Bonsai_web : module type of Bonsai_web with module View := Bonsai_web.View)
open Bonsai.Let_syntax

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

let view t = t.view
let map_view t ~f = { t with view = f t.view }

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

let is_valid t = Or_error.is_ok t.value

let return value =
  { value = Ok value; view = View.empty; set = (fun _ -> Ui_effect.Ignore) }
;;

let return_settable (type a) (module M : Bonsai.Model with type t = a) (value : a) =
  let%sub value, set_value = Bonsai.state (module M) ~default_model:value in
  let%arr value = value
  and set_value = set_value in
  { value = Ok value; view = View.empty; set = set_value }
;;

let return_error error =
  { value = Error error; view = View.empty; set = (fun _ -> Ui_effect.Ignore) }
;;

let map_error t ~f = { t with value = t.value |> Result.map_error ~f }

let both a b =
  let value = Or_error.both a.value b.value in
  let view = View.tuple [ a.view; b.view ] in
  let set (ea, eb) = Effect.lazy_ (lazy (Ui_effect.Many [ a.set ea; b.set eb ])) in
  { value; view; set }
;;

let all forms =
  let value = Or_error.all (List.map forms ~f:(fun a -> a.value)) in
  let view = View.tuple (List.map forms ~f:(fun a -> a.view)) in
  let set edits =
    Effect.lazy_
      (lazy
        (let paired, remainder = List.zip_with_remainder forms edits in
         let error_message =
           match remainder with
           | None -> Effect.Ignore
           | Some mismatch ->
             let form_count = List.length forms in
             let edits_count = List.length edits in
             let print_warning detail resolution =
               Effect.print_s
                 [%message
                   {|WARNING: Form.set called on result of Form.all with a list value whose length doesn't match the number of forms |}
                     detail
                     (form_count : int)
                     (edits_count : int)
                     resolution]
             in
             (match mismatch with
              | First (_ : _ t list) ->
                print_warning "more forms than values" "not setting left-over forms"
              | Second (_ : _ list) ->
                print_warning "more values than forms" "dropping left-over values")
         in
         Ui_effect.Many (error_message :: List.map paired ~f:(fun (a, edit) -> a.set edit))))
  in
  { value; view; set }
;;

let all_map (type k cmp) (forms : (k, _, cmp) Map.t) =
  let comparator = Map.comparator_s forms in
  let module C = (val comparator) in
  let forms_as_alist = Map.to_alist forms in
  let value =
    forms_as_alist
    |> List.map ~f:(fun (k, form) ->
      let%map.Or_error value = form.value in
      k, value)
    |> Or_error.all
    |> Or_error.map ~f:(Map.of_alist_exn comparator)
  in
  let view = forms_as_alist |> List.map ~f:(fun (_, a) -> a.view) |> View.tuple in
  let set edits =
    Effect.lazy_
      (lazy
        (let updates =
           Map.fold2 forms edits ~init:[] ~f:(fun ~key ~data acc ->
             let warning_m details =
               Effect.print_s
                 [%message
                   {|WARNING: Form.set on the result of Form.all_map has mismatched keys|}
                     ~_:(details : string)
                     ~key:(C.comparator.sexp_of_t key : Sexp.t)]
             in
             match data with
             | `Left _form ->
               let eff = warning_m "update is missing key present in active form" in
               eff :: acc
             | `Right _update ->
               let eff = warning_m "update contains key not present in active forms" in
               eff :: acc
             | `Both (form, update) -> form.set update :: acc)
         in
         Effect.Many updates))
  in
  { value; view; set }
;;

let label' label t = { t with view = View.set_label label t.view }
let label text = label' (Vdom.Node.text text)
let tooltip' tooltip t = { t with view = View.set_tooltip tooltip t.view }
let tooltip text = tooltip' (Vdom.Node.text text)

let project' t ~parse ~unparse =
  let value =
    Or_error.bind t.value ~f:(fun a -> Or_error.try_with_join (fun () -> parse a))
  in
  let set a = Effect.lazy_ (lazy (t.set (unparse a))) in
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

let fallback_to t ~value =
  match t.value with
  | Ok _ -> t
  | Error _ -> { t with value = Ok value }
;;

module For_profunctor = struct
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
  let field t fieldslib_field =
    let value = attach_fieldname_to_error t fieldslib_field in
    let with_label =
      For_profunctor.Return
        { name = label_of_field fieldslib_field; form = { t with value } }
    in
    field with_label fieldslib_field
  ;;

  let build_for_record a =
    let value, set, fields = For_profunctor.finalize_view (build_for_record a) in
    { value; set; view = View.record fields }
  ;;
end

module Dynamic = struct
  let with_default_from_effect effect form =
    let open Bonsai.Let_syntax in
    let%sub is_loaded, set_is_loaded = Bonsai.state (module Bool) ~default_model:false in
    let%sub () =
      match%sub is_loaded with
      | true -> Bonsai.const ()
      | false ->
        let%sub after_display =
          let%arr effect = effect
          and set_is_loaded = set_is_loaded
          and form = form in
          let%bind.Effect default = effect in
          Ui_effect.Many [ set form default; set_is_loaded true ]
        in
        Bonsai.Edge.lifecycle ~after_display ()
    in
    return form
  ;;

  let sync_with m ~store_value ~store_set form =
    let%sub interactive_value, interactive_set =
      let%arr form = form in
      Or_error.ok (value form), set form
    in
    Bonsai_extra.mirror' m ~store_value ~store_set ~interactive_value ~interactive_set
  ;;

  let with_default default form =
    let%sub get_default = Bonsai.yoink default in
    let%sub effect =
      let%arr get_default = get_default in
      match%bind.Effect get_default with
      | Active default -> Effect.return default
      | Inactive ->
        Effect.never
    in
    with_default_from_effect effect form
  ;;

  let with_default_always default form =
    let open Bonsai.Let_syntax in
    let%sub is_loaded, set_is_loaded = Bonsai.state (module Bool) ~default_model:false in
    let%sub () =
      match%sub is_loaded with
      | true -> Bonsai.const ()
      | false ->
        let%sub after_display =
          let%arr default = default
          and set_is_loaded = set_is_loaded
          and { set; _ } = form in
          Effect.lazy_ (lazy (Ui_effect.Many [ set default; set_is_loaded true ]))
        in
        Bonsai.Edge.lifecycle ~after_display ()
    in
    let%sub () =
      let%sub on_activate =
        let%arr default = default
        and { set; _ } = form
        and is_loaded = is_loaded in
        if is_loaded then Effect.lazy_ (lazy (set default)) else Effect.Ignore
      in
      Bonsai.Edge.lifecycle ~on_activate ()
    in
    return form
  ;;

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
        ?(one_at_a_time = false)
        ?debounce_ui
        (t : a t Bonsai.Value.t)
        ~f
    =
    let open Bonsai.Effect_throttling in
    let module Validated = struct
      type t = Input.t Or_error.t Poll_result.t [@@deriving sexp, equal]
    end
    in
    match%sub t >>| value with
    | Error _ -> Bonsai.read t
    | Ok value ->
      let%sub validation =
        let%sub f =
          if one_at_a_time
          then poll f
          else (
            let%arr f = f in
            fun a ->
              let%map.Effect result = f a in
              Poll_result.Finished result)
        in
        let%sub effect =
          let%arr f = f in
          fun a ->
            match%map.Effect f a with
            | Aborted -> Poll_result.Aborted
            | Finished (Ok ()) -> Finished (Ok a)
            | Finished (Error e) -> Finished (Error e)
        in
        Bonsai.Edge.Poll.effect_on_change
          (module Input)
          (module Validated)
          Bonsai.Edge.Poll.Starting.empty
          value
          ~effect
      in
      let%sub is_stable =
        match debounce_ui with
        | None -> Bonsai.const true
        | Some time_to_stable ->
          Bonsai_extra.is_stable ~equal:Input.equal value ~time_to_stable
      in
      let%arr t = t
      and validation = validation
      and is_stable = is_stable in
      let validating_error = Error (Error.of_string "validating...") in
      validate t ~f:(fun a ->
        if not is_stable
        then validating_error
        else (
          match validation with
          | Some (Finished (Ok x)) when Input.equal a x -> Ok ()
          | None | Some Aborted | Some (Finished (Ok _)) -> validating_error
          | Some (Finished (Error e)) -> Error e))
  ;;

  module Record_builder = struct
    include Profunctor.Record_builder (struct
        type ('read, 'write) t = ('read, 'write) For_profunctor.t Value.t

        let both a b = Value.map2 a b ~f:For_profunctor.both
        let map a ~f = Value.map a ~f:(For_profunctor.map ~f)
        let contra_map a ~f = Value.map a ~f:(For_profunctor.contra_map ~f)
      end)

    let field t fieldslib_field =
      let for_profunctor =
        let%map t = t in
        let t =
          { t with value = Record_builder.attach_fieldname_to_error t fieldslib_field }
        in
        For_profunctor.Return
          { name = Record_builder.label_of_field fieldslib_field; form = t }
      in
      field for_profunctor fieldslib_field
    ;;

    let build_for_record creator =
      let%arr t = build_for_record creator in
      let value, set, fields = For_profunctor.finalize_view t in
      { value; set; view = View.record fields }
    ;;
  end
end

module Expert = struct
  let create = Fields_of_unbalanced.create
end

module Private = struct
  let suggest_label label t =
    { t with view = View.suggest_label' (Vdom.Node.text label) t.view }
  ;;
end
