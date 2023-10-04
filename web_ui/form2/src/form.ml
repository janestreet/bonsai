open! Core
open Bonsai_web
open Bonsai.Let_syntax

type ('a, 'view) t =
  { value : 'a Or_error.t
  ; view : 'view
  ; set : 'a -> unit Vdom.Effect.t
  }
[@@deriving fields ~getters]

let value_or_default t ~default = t |> value |> Or_error.ok |> Option.value ~default

let normalize { value; set; view = _ } =
  match value with
  | Ok value -> set value
  | Error _ -> Ui_effect.Ignore
;;

let view t = t.view
let map_view t ~f = { t with view = f t.view }
let is_valid t = Or_error.is_ok t.value

let return ?sexp_of_t value =
  let set value =
    match sexp_of_t with
    | Some sexp_of_t ->
      Effect.print_s
        [%message "Form.return was set, but setting is ignored." ~set_value:(value : t)]
    | None -> Effect.print_s [%message "Form.return was set, but setting is ignored."]
  in
  { value = Ok value; view = (); set }
;;

let return_settable ?sexp_of_model ~equal value =
  let%sub value, set_value = Bonsai.state value ?sexp_of_model ~equal in
  let%arr value = value
  and set_value = set_value in
  { value = Ok value; view = (); set = set_value }
;;

let return_error error =
  { value = Error error; view = (); set = (fun _ -> Ui_effect.Ignore) }
;;

let map_error t ~f = { t with value = t.value |> Result.map_error ~f }

let both a b =
  let value = Or_error.both a.value b.value in
  let view = a.view, b.view in
  let set (ea, eb) = Effect.lazy_ (lazy (Ui_effect.Many [ a.set ea; b.set eb ])) in
  { value; view; set }
;;

let all forms =
  let values, view = List.map forms ~f:(fun a -> a.value, a.view) |> List.unzip in
  let value = Or_error.all values in
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
  let values, views =
    Map.to_alist forms
    |> List.map ~f:(fun (k, form) ->
         let value =
           let%map.Or_error value = form.value in
           k, value
         in
         value, (k, form.view))
    |> List.unzip
  in
  let value = values |> Or_error.all |> Or_error.map ~f:(Map.of_alist_exn comparator) in
  let view = Map.of_alist_exn comparator views in
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

let project' ?(extend_view_with_error = fun view _error -> view) t ~parse ~unparse =
  let value, view =
    match t.value with
    | Ok a ->
      (match Or_error.try_with_join (fun () -> parse a) with
       | Ok a -> Ok a, t.view
       | Error e -> Error e, extend_view_with_error t.view e)
    | Error _ as e -> e, t.view
  in
  let set a = Effect.lazy_ (lazy (t.set (unparse a))) in
  { value; view; set }
;;

let validate ?extend_view_with_error t ~f =
  project'
    t
    ?extend_view_with_error
    ~parse:(fun a -> f a |> Or_error.map ~f:(fun () -> a))
    ~unparse:Fn.id
;;

let project ?extend_view_with_error t ~parse_exn ~unparse =
  project' t ?extend_view_with_error ~parse:(fun a -> Ok (parse_exn a)) ~unparse
;;

let optional' (type a b) (t : (a, _) t) ~parse ~unparse ~none : (b option, _) t =
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

module Dynamic = struct
  let with_default_from_effect effect form =
    let open Bonsai.Let_syntax in
    let%sub is_loaded, set_is_loaded =
      Bonsai.state false ~sexp_of_model:[%sexp_of: Bool.t] ~equal:[%equal: Bool.t]
    in
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

  let sync_with ?sexp_of_model ~equal ~store_value ~store_set form =
    let%sub interactive_value, interactive_set =
      let%arr form = form in
      Or_error.ok (value form), set form
    in
    Bonsai_extra.mirror'
      ()
      ?sexp_of_model
      ~equal
      ~store_value
      ~store_set
      ~interactive_value
      ~interactive_set
  ;;

  let with_default default form =
    let%sub get_default = Bonsai.yoink default in
    let%sub effect =
      let%arr get_default = get_default in
      match%bind.Effect get_default with
      | Active default -> Effect.return default
      | Inactive -> Effect.never
    in
    with_default_from_effect effect form
  ;;

  let with_default_always default form =
    let open Bonsai.Let_syntax in
    let%sub is_loaded, set_is_loaded =
      Bonsai.state false ~sexp_of_model:[%sexp_of: Bool.t] ~equal:[%equal: Bool.t]
    in
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

  let on_change
    (type a)
    ?(on_error = Value.return (Fn.const Ui_effect.Ignore))
    ?sexp_of_model
    ~equal
    ~f
    value_to_watch
    =
    let module M_or_error = struct
      type model = a

      let equal_model = equal
      let sexp_of_model = Option.value ~default:sexp_of_opaque sexp_of_model

      type t = model Or_error.t [@@deriving equal, sexp_of]
    end
    in
    let callback =
      let%map f = f
      and on_error = on_error in
      function
      | Error e -> on_error e
      | Ok new_value -> f new_value
    in
    Bonsai.Edge.on_change
      ~sexp_of_model:[%sexp_of: M_or_error.t]
      ~equal:[%equal: M_or_error.t]
      (value_to_watch >>| value)
      ~callback
  ;;

  let project_via_effect
    (type a b view)
    ?sexp_of_input
    ?sexp_of_result
    ~equal_input
    ~equal_result
    ?(one_at_a_time = false)
    ?debounce_ui
    (t : (a, view) t Bonsai.Value.t)
    ~unparse
    ~parse
    =
    let open Bonsai.Effect_throttling in
    let module Validated = struct
      let equal_a = equal_input
      let equal_b = equal_result

      type t = (a * b) Or_error.t Poll_result.t [@@deriving equal]
    end
    in
    let value = t >>| value in
    let%sub validation =
      let%sub parse =
        if one_at_a_time
        then poll parse
        else (
          let%arr parse = parse in
          fun a ->
            let%map.Effect result = parse a in
            Poll_result.Finished result)
      in
      let%sub effect =
        let%arr parse = parse in
        function
        | Error e -> Poll_result.Finished (Error e) |> Ui_effect.return
        | Ok a ->
          (match%map.Effect parse a with
           | Aborted -> Poll_result.Aborted
           | Finished (Ok b) -> Finished (Ok (a, b))
           | Finished (Error e) -> Finished (Error e))
      in
      let sexp_of_result =
        let f sexp_of_input sexp_of_result =
          Poll_result.sexp_of_t
            (Or_error.sexp_of_t (sexp_of_pair sexp_of_input sexp_of_result))
        in
        Option.map2 sexp_of_input sexp_of_result ~f
      in
      Bonsai.Edge.Poll.effect_on_change
        ?sexp_of_input:(Option.map sexp_of_input ~f:Or_error.sexp_of_t)
        ?sexp_of_result
        ~equal_input:(Or_error.equal equal_input)
        ~equal_result:[%equal: Validated.t]
        Bonsai.Edge.Poll.Starting.empty
        value
        ~effect
    in
    let%sub is_stable =
      match%sub value with
      | Error _ -> Bonsai.const false
      | Ok value ->
        (match debounce_ui with
         | None -> Bonsai.const true
         | Some time_to_stable ->
           Bonsai_extra.is_stable
             ~equal:equal_input
             value
             ~time_to_stable:(Value.return time_to_stable))
    in
    let%arr t = t
    and validation = validation
    and is_stable = is_stable in
    let validating_error = Error (Error.of_string "validating...") in
    project'
      t
      ~parse:(fun x ->
        if not is_stable
        then validating_error
        else (
          match validation with
          | Some (Finished (Ok (a, b))) when equal_input a x -> Ok b
          | None | Some Aborted | Some (Finished (Ok _)) -> validating_error
          | Some (Finished (Error e)) -> Error e))
      ~unparse
  ;;

  let validate_via_effect
    (type a view)
    ?sexp_of_model
    ~equal
    ?one_at_a_time
    ?debounce_ui
    (t : (a, view) t Bonsai.Value.t)
    ~f
    =
    let%sub parse =
      let%arr f = f in
      fun value ->
        let%bind.Effect validation = f value in
        Effect.return
          (let%bind.Or_error () = validation in
           Ok value)
    in
    project_via_effect
      ?sexp_of_input:sexp_of_model
      ?sexp_of_result:sexp_of_model
      ~equal_input:equal
      ~equal_result:equal
      ?one_at_a_time
      ?debounce_ui
      t
      ~unparse:Fn.id
      ~parse
  ;;
end
