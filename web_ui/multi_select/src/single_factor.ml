open! Core
open! Import
open  Bonsai_web
open  Single_factor_intf

module type S    = S
module type Item = Item

module Make (Item : Item) = struct
  module Searcher = struct
    (* Filters a set of items according to a search string *)
    module T = struct
      module Input = Item.Set

      module Result = struct
        type t =
          { items_matching_search : Item.Set.t
          ; update_search         : string -> unit Bonsai.Effect.t
          ; current_search        : string
          }
      end

      module Model  = String
      module Action = String

      let apply_action ~inject:_ ~schedule_event:_ _input _model new_search = new_search

      let compute ~inject all_items search_string =
        let items_matching_search =
          Set.filter all_items ~f:(fun item ->
            String.Caseless.is_substring
              (Item.to_string item)
              ~substring:search_string)
        in
        { Result.items_matching_search
        ; update_search  = inject
        ; current_search = search_string
        }
      ;;

      let name = Source_code_position.to_string [%here]
    end

    include T

    let bonsai ~initial input =
      Bonsai.of_module1
        (module T)
        ~default_model:initial
        input
    ;;
  end

  module T = struct
    module View_config = struct
      type t =
        { header               : Vdom.Node.t
        ; autofocus_search_box : bool
        ; search_box_id        : string option
        ; extra_row_attrs      : (is_focused:bool -> Vdom.Attr.t) option
        }

      let create ?extra_row_attrs ?(autofocus_search_box = false) ?id ~header () =
        { header; autofocus_search_box; search_box_id = id; extra_row_attrs }
      ;;
    end

    module Input = struct
      type t =
        { items_matching_search    : Item.Set.t
        ; update_search            : string -> unit Bonsai.Effect.t
        ; all_items                : Item.Set.t
        ; default_selection_status : Selection_status.t
        ; current_search           : string
        ; view_config              : View_config.t
        }
    end

    module Model = struct
      type t =
        { selection_status : Selection_status.t Map.M(Item).t
        ; focused_item     : Item.t option
        }
      [@@deriving compare, equal, fields, sexp]

      let create ?(selection_status = Item.Map.empty) ?focused_item () =
        { selection_status; focused_item }
      ;;
    end

    module Action = struct
      type t =
        | Update_search_string       of string
        | Set_item_selected          of
            { item   : Item.t
            ; status : Selection_status.t
            }
        | Set_all_selection_statuses of Selection_status.t Item.Map.t
        | Toggle_focused_item_selected
        | Set_focus                  of Item.t option
        | Move_focus                 of [ `Next | `Prev ]
        | Select_all
        | Select_none
      [@@deriving sexp_of]
    end

    module Result = struct
      type t =
        { view             : Vdom.Node.t
        ; view_for_testing : string Lazy.t
        ; key_handler      : Vdom_keyboard.Keyboard_event_handler.t
        ; inject           : Action.t -> unit Bonsai.Effect.t
        ; selected_items   : Item.Set.t
        }
    end

    let move_in_set set element ~dir =
      match element with
      | None         ->
        (match dir with
         | `Prev -> Set.max_elt set
         | `Next -> Set.min_elt set)
      | Some element ->
        let sequence =
          match dir with
          | `Prev -> Set.to_sequence set ~less_or_equal_to:   element ~order:`Decreasing
          | `Next -> Set.to_sequence set ~greater_or_equal_to:element ~order:`Increasing
        in
        (* The first element in the sequence will be [element], since the arguments that
           we pass are "_or_equal_to". *)
        Sequence.nth sequence 1
    ;;

    let focused_item (input : Input.t) (model : Model.t) =
      Option.bind model.focused_item ~f:(fun focus ->
        if Set.mem input.items_matching_search focus then Some focus else None)
    ;;

    let is_item_selected (input : Input.t) (model : Model.t) ~item =
      match
        Map.find model.selection_status item
        |> Option.value ~default:input.default_selection_status
      with
      | Selected   -> true
      | Unselected -> false
    ;;

    let selected_items (input : Input.t) (model : Model.t) =
      let explicitly_selected =
        List.filter_map (Map.to_alist model.selection_status) ~f:(fun (item, status) ->
          match status with
          | Selected   -> Some item
          | Unselected -> None)
        |> Item.Set.of_list
      in
      let defaults =
        match input.default_selection_status with
        | Unselected -> Item.Set.empty
        | Selected   -> Set.diff input.all_items (Set.of_map_keys model.selection_status)
      in
      Set.union explicitly_selected defaults
    ;;

    let apply_action
          ~inject:_
          ~schedule_event
          (input  : Input.t )
          (model  : Model.t )
          (action : Action.t)
      =
      match action with
      | Update_search_string search_string ->
        let focused_item =
          Option.bind model.focused_item ~f:(fun focused_item ->
            if not (Set.mem input.items_matching_search focused_item)
            then None
            else Some focused_item)
        in
        schedule_event (input.update_search search_string);
        { model with focused_item }
      | Set_item_selected { item; status }          ->
        { model with
          selection_status = Map.set model.selection_status ~key:item ~data:status
        }
      | Set_all_selection_statuses selection_status -> { model with selection_status }
      | Toggle_focused_item_selected                ->
        (match focused_item input model with
         | None              -> model
         | Some focused_item ->
           let selection_status =
             Map.update model.selection_status focused_item ~f:(fun status ->
               let status =
                 Option.value status ~default:input.default_selection_status
               in
               Selection_status.toggle status)
           in
           { model with selection_status })
      | Set_focus item -> { model with focused_item = item }
      | Move_focus dir ->
        let focused_item =
          move_in_set input.items_matching_search (focused_item input model) ~dir
        in
        (match focused_item with
         | None              -> model
         | Some focused_item -> { model with focused_item = Some focused_item })
      | Select_all ->
        let selection_status =
          Set.fold
            input.items_matching_search
            ~init:model.selection_status
            ~f:(fun map item -> Map.set map ~key:item ~data:Selected)
        in
        { model with selection_status }
      | Select_none ->
        let selection_status =
          Set.fold
            input.items_matching_search
            ~init:model.selection_status
            ~f:(fun map item -> Map.set map ~key:item ~data:Unselected)
        in
        { model with selection_status }
    ;;

    let view_for_testing (input : Input.t) (model : Model.t) =
      String.concat
        ~sep:"\n"
        (sprintf "Search string: '%s'" input.current_search
         :: List.map (Set.to_list input.items_matching_search) ~f:(fun item ->
           let is_focused =
             match model.focused_item with
             | None       -> false
             | Some item' -> Item.( = ) item item'
           in
           let is_selected = is_item_selected input model ~item in
           sprintf
             !"%s %s %{Item}"
             (if is_focused  then "->" else "  ")
             (if is_selected then "*"  else " " )
             item))
    ;;

    let search_box_view (input : Input.t) ~inject ~autofocus ~id =
      let open Vdom in
      let on_input = function
        | None      -> inject (Action.Update_search_string ""  )
        | Some text -> inject (Action.Update_search_string text)
      in
      let extra_attrs =
        Attr.combine
          (Attr.autofocus autofocus)
          (Option.value_map id ~f:Attr.id ~default:Attr.empty)
      in
      Vdom_input_widgets.Entry.text
        ~merge_behavior:Legacy_dont_merge
        ~value:(Some input.current_search)
        ~on_input
        ~extra_attrs:[ extra_attrs ]
        ()
    ;;

    let select_all_and_none_view ~inject =
      let open Vdom in
      let link ~text ~(action : Action.t) ~class_ =
        Node.a
          ~attr:
            (Attr.many_without_merge
               [ Attr.href "about:blank"
               ; Attr.on_click (fun ev ->
                   match Bonsai_web.am_within_disabled_fieldset ev with
                   | true  -> Effect.Prevent_default
                   | false -> Effect.Many [ inject action; Effect.Prevent_default ])
               ; Attr.class_ class_
               ])
          [ Node.text text ]
      in
      Node.div
        ~attr:(Attr.class_ "multi-select-select-all-none")
        [ Node.text "Select: "
        ; link ~text:"all" ~action:Select_all ~class_:"multi-select-select-all"
        ; Node.text "; "
        ; link ~text:"none" ~action:Select_none ~class_:"multi-select-select-none"
        ]
    ;;

    let checkboxes_view
          (input : Input.t)
          (model : Model.t)
          ~extra_row_attrs
          ~selected_items
          ~inject
      =
      let open Vdom in
      let focused_item = focused_item input model in
      let checkboxes =
        List.map (Set.to_list input.items_matching_search) ~f:(fun item ->
          let checkbox =
            let on_change =
              Attr.on_change (fun ev _new_value ->
                let status =
                  match Js_of_ocaml.Js.Opt.to_option ev##.target with
                  | None   ->
                    Js_of_ocaml.Firebug.console##error "Target missing";
                    assert false
                  | Some t ->
                    let value = (Js_of_ocaml.Js.Unsafe.coerce t)##.checked in
                    if Js_of_ocaml.Js.to_bool value
                    then Selection_status.Selected
                    else Unselected
                in
                inject (Action.Set_item_selected { item; status }))
            in
            let checked_attrs   = [ Attr.checked; Attr.bool_property "checked" true  ] in
            let unchecked_attrs = [               Attr.bool_property "checked" false ] in
            Node.input
              ~attr:
                (Attr.many_without_merge
                   ([ on_change; Attr.type_ "checkbox" ]
                    @
                    if Set.mem selected_items item
                    then checked_attrs
                    else unchecked_attrs))
              ()
          in
          let is_focused  = [%compare.equal: Item.t option] (Some item) focused_item in
          let extra_attrs = extra_row_attrs ~is_focused                              in
          let focus_attrs =
            if is_focused
            then
              Vdom.Attr.(
                many [ extra_attrs; Vdom.Attr.class_ "multi-select-item-focused" ])
            else extra_attrs
          in
          let on_click =
            Attr.on_click (fun ev ->
              match Bonsai_web.am_within_disabled_fieldset ev with
              | true  -> Effect.Ignore
              | false ->
                Effect.Many
                  [ inject (Action.Set_focus (Some item))
                  ; inject Toggle_focused_item_selected
                  ])
          in
          Node.div
            ~attr:
              (Attr.many
                 [ on_click; Vdom.Attr.class_ "multi-select-item"; focus_attrs ])
            [ checkbox; Node.label [ Node.text (Item.to_string item) ] ])
      in
      Node.div ~attr:(Attr.class_ "multi-select-checkboxes") checkboxes
    ;;

    let view (input : Input.t) model ~selected_items ~inject =
      let open Vdom in
      let select_all_and_none_view = select_all_and_none_view ~inject in
      let search_box =
        search_box_view
          input
          ~inject
          ~autofocus:input.view_config.autofocus_search_box
          ~id:input.view_config.search_box_id
      in
      let extra_row_attrs =
        Option.value input.view_config.extra_row_attrs ~default:(fun ~is_focused:_ ->
          Vdom.Attr.empty)
      in
      let checkboxes =
        checkboxes_view input model ~selected_items ~inject ~extra_row_attrs
      in
      Node.div
        ~attr:(Attr.class_ "multi-select-container")
        [ Node.div ~attr:(Attr.class_ "multi-select-header") [ input.view_config.header ]
        ; search_box
        ; select_all_and_none_view
        ; checkboxes
        ]
    ;;

    let key_handler ~inject =
      let open Vdom_keyboard in
      let command ?cond ~keys ~description f =
        let handler =
          let open Keyboard_event_handler.Handler in
          match cond with
          | None      -> with_prevent_default f
          | Some cond -> only_handle_if cond  f ~prevent_default:()
        in
        { Keyboard_event_handler.Command.keys; description; group = None; handler }
      in
      let key = Keystroke.create' in
      Keyboard_event_handler.of_command_list_exn
        [ command
            ~keys:[ key ArrowUp ]
            ~description:"Move focus one item up"
            (fun _ev -> inject (Action.Move_focus `Prev))
        ; command
            ~keys:[ key ArrowDown ]
            ~description:"Move focus one item down"
            (fun _ev -> inject (Action.Move_focus `Next))
        ; command
            ~keys:[ key Enter ]
            ~description:"Toggle whether focused item is selected"
            (fun _ev -> inject Action.Toggle_focused_item_selected)
        ]
    ;;

    let compute ~inject input model =
      let selected_items = selected_items input model in
      { Result.view_for_testing = lazy (view_for_testing input model)
      ; view                    = view input model ~selected_items ~inject
      ; key_handler             = key_handler ~inject
      ; selected_items
      ; inject
      }
    ;;

    let name = Source_code_position.to_string [%here]
  end

  include T

  let bonsai' input = Bonsai.of_module1 (module T) input

  module _ = struct
    type t =
      { all_items                : Item.Set.t
      ; default_selection_status : Selection_status.t
      ; view_config              : View_config.t
      }
    [@@deriving fields]
  end

  module Initial_model_settings = struct
    type t =
      { search_string    : string
      ; selection_status : Selection_status.t Item.Map.t option
      ; focused_item     : Item.t option
      }

    let create ?(search_string = "") ?selection_status ?focused_item () =
      { search_string; selection_status; focused_item }
    ;;
  end

  let bonsai
        ?(initial_model_settings = Initial_model_settings.create ())
        ?(default_selection_status = Value.return Selection_status.Unselected)
        ~view_config
        all_items
    =
    let open Bonsai.Let_syntax in
    let%sub search_results =
      Searcher.bonsai ~initial:initial_model_settings.search_string all_items
    in
    let input_for_t =
      let%map all_items                = all_items
      and     view_config              = view_config
      and     default_selection_status = default_selection_status
      and { Searcher.Result.items_matching_search; update_search; current_search } =
        search_results
      in
      { T.Input.items_matching_search
      ; update_search
      ; default_selection_status
      ; all_items
      ; current_search
      ; view_config
      }
    in
    bonsai'
      ~default_model:
        (T.Model.create
           ?selection_status:initial_model_settings.selection_status
           ?focused_item:initial_model_settings.focused_item
           ())
      input_for_t
  ;;
end

