open! Core
open! Bonsai_web
open  Bonsai.Let_syntax
module Notifications = Bonsai_web_ui_notifications
module Form          = Bonsai_web_ui_form

module Settings = struct
  type t =
    { dismiss_errors_automatically : bool
    ; dismiss_notifications_after  : Time_ns.Span.t
    }
  [@@deriving fields]

  let form : t Form.t Computation.t =
    let open Form.Dynamic.Record_builder in
    let%sub dismiss_errors_automatically =
      let%map.Computation input = Form.Elements.Checkbox.bool [%here] ~default:false in
      Form.label "Dismiss errors automatically" input
    in
    let%sub dismiss_notifications_after =
      let%map.Computation input =
        Form.Elements.Textbox.sexpable [%here] (module Time_ns.Span)
      in
      Form.label "Dismiss notifications after" input
    in
    let%sub form =
      Fields.make_creator
        ~dismiss_errors_automatically:
          (field ~group_lists:false dismiss_errors_automatically)
        ~dismiss_notifications_after:
          (field ~group_lists:false dismiss_notifications_after)
      |> build_for_record
    in
    Form.Dynamic.with_default
      (Value.return
         { dismiss_errors_automatically = false
         ; dismiss_notifications_after  = Time_ns.Span.create ~sec:15 ()
         })
      form
  ;;
end

module Notification_form = struct
  type t =
    { contents : [ `Sequence | `User_defined of string         ]
    ; level    : [ `Success  | `Error        of Error.t option ]
    }
  [@@deriving fields]

  let form ~sequence : t Form.t Computation.t =
    let%sub contents_chooser =
      Form.Elements.Dropdown.enumerable
        [%here]
        (module struct
          type t =
            [ `Sequence
            | `User_defined
            ]
          [@@deriving compare, enumerate, equal, sexp]
        end)
    in
    let%sub contents =
      Form.Elements.Textbox.string [%here] ~placeholder:"Notification contents"
    in
    let%sub error_chooser =
      Form.Elements.Dropdown.enumerable
        [%here]
        (module struct
          type t =
            [ `Success
            | `Error
            ]
          [@@deriving compare, enumerate, equal, sexp]
        end)
    in
    let%sub error_message = Form.Elements.Textbox.string [%here] in
    return
    @@ let%map contents_chooser =
         contents_chooser >>| Form.label "How to generate notification contents"
    and contents =
      contents
      >>| Form.label "Notification contents"
      >>| Form.validate ~f:(function
        | "" ->
          Or_error.error_s [%message "Notification contents cannot be empty!"]
        | _  -> Ok ())
    and error_chooser = error_chooser >>| Form.label "Level"
    and error_message =
      error_message
      >>| Form.optional'
            ~parse:(function
              | ""    -> Ok None
              | error -> Ok (Some (Error.of_string error)))
            ~unparse:Error.to_string_mach
            ~none:""
      >>| Form.label "Technical error message"
    and sequence = sequence in
    let view =
      Nonempty_list.fold
        ~init:Form.View.Private.Empty
        ~f:Form.View.Private.concat
        [ Form.view contents_chooser
        ; (match Form.value contents_chooser with
           | Error _ | Ok `Sequence ->
             Form.View.Private.of_vdom
               ~id:"current-seqno"
               (Vdom.Node.pre [ Vdom.Node.textf "Current seqno: %d" sequence ])
           | Ok `User_defined -> Form.view contents)
        ; Form.view error_chooser
        ; (match Form.value error_chooser with
           | Error _ | Ok `Success -> Form.View.Private.Empty
           | Ok `Error             -> Form.view error_message)
        ]
    in
    let set t =
      Ui_effect.Many
        [ (match t.contents with
            | `Sequence               -> Ui_effect.Ignore
            | `User_defined contents' -> Form.set contents contents')
        ; (match t.level with
           | `Success           -> Form.set error_chooser `Success
           | `Error maybe_error ->
             Ui_effect.Many
               [ Form.set error_chooser `Error; Form.set error_message maybe_error ])
        ]
    in
    let value =
      let open Or_error.Let_syntax in
      match%bind Form.value error_chooser with
      | `Success ->
        (match%bind Form.value contents_chooser with
         | `Sequence     -> Ok { contents = `Sequence; level = `Success }
         | `User_defined ->
           let%map contents = Form.value contents in
           { contents = `User_defined contents; level = `Success })
      | `Error ->
        let%map contents =
          match%bind Form.value contents_chooser with
          | `Sequence     -> return `Sequence
          | `User_defined ->
            let%map contents = Form.value contents in
            `User_defined contents
        and error_message = Form.value error_message in
        { contents; level = `Error error_message }
    in
    Form.Expert.create ~value ~set ~view
  ;;
end

let value_from_form_with_default ~f ~default form =
  form >>| Form.value >>| Result.ok >>| Option.value_map ~default ~f
;;

let components =
  let open! Bonsai.Let_syntax in
  let%sub settings_form = Settings.form in
  let%sub sequence = Bonsai.state [%here] ~default_model:1 (module Int) in
  let%sub notification_form = Notification_form.form ~sequence:(sequence >>| fst) in
  let%sub notifications =
    Notifications.create
      ~dismiss_notifications_after:
        (value_from_form_with_default
           ~f:Settings.dismiss_notifications_after
           ~default:(Time_ns.Span.create ~sec:15 ())
           settings_form)
      ~dismiss_errors_automatically:
        (value_from_form_with_default
           ~f:Settings.dismiss_errors_automatically
           ~default:false
           settings_form)
      [%here]
  in
  return
  @@ let%map notifications = notifications
  and notification_form = notification_form >>| Form.group "Add notification"
  and settings_form     = settings_form     >>| Form.group "Settings"
  and sequence, inject_sequence = sequence in
  let on_submit =
    { Form.View.Private.on_submit =
        Form.value notification_form
        |> Result.ok
        |> Option.map ~f:(fun { contents; level } ->
          let ui_effect, contents =
            match contents with
            | `User_defined contents -> Ui_effect.Ignore, contents
            | `Sequence              ->
              inject_sequence (Int.succ sequence), Int.to_string sequence
          in
          let notification_ui_effect =
            match level with
            | `Success     -> Notifications.add_success notifications ~text:contents
            | `Error error ->
              Notifications.add_error notifications ?error ~text:contents
          in
          Ui_effect.Many [ ui_effect; notification_ui_effect ])
    ; handle_enter = true
    ; button_text  = Some "Add notification"
    }
  in
  Vdom.Node.div
    [ Form.view_as_vdom settings_form
    ; Vdom.Node.br ()
    ; Form.View.to_vdom ~on_submit (Form.view notification_form)
    ; Notifications.to_vdom notifications
    ]
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" components
;;
