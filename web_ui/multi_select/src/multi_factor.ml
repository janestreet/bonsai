open! Core
open! Import
open Multi_factor_intf

module type S = S
module type Key = Key

module Make (Item : Single_factor.Item) (Key : Key) = struct
  module Single_factor = Single_factor.Make (Item)

  module Ring_focus = struct
    module Action = struct
      type t =
        | Cycle_focused_subwidget of [ `Next | `Prev ]
        | Set_focused_subwidget of Key.t
      [@@deriving sexp_of]
    end

    module Model = struct
      type t = Key.t Focus_ring.t [@@deriving compare, equal, sexp]
    end

    module Result = struct
      type t = Key.t * (Action.t -> unit Bonsai_web.Effect.t)
    end

    module Input = Unit

    let apply_action (_ : _ Bonsai.Apply_action_context.t) () model action =
      match (action : Action.t) with
      | Cycle_focused_subwidget `Next -> Focus_ring.next model
      | Cycle_focused_subwidget `Prev -> Focus_ring.prev model
      | Set_focused_subwidget key ->
        Focus_ring.set model ~f:(fun key' -> [%compare.equal: Key.t] key key')
        |> Option.value ~default:model
    ;;

    let compute ~inject () model = Focus_ring.current_focus model, inject
    let name = Source_code_position.to_string [%here]
  end

  module Action = struct
    type t =
      | Cycle_focused_subwidget of [ `Next | `Prev ]
      | Set_focused_subwidget of Key.t
      | Subwidget_action of Key.t * Single_factor.Action.t
      | Select_on_all_subwidgets of [ `All | `None ]
    [@@deriving sexp_of]
  end

  type per_subwidget =
    { default_selection_status : Selection_status.t
    ; all_items : Item.Set.t
    }
  [@@deriving fields ~getters]

  module Result = struct
    type t =
      { view : Vdom.Node.t
      ; view_for_testing : string Lazy.t
      ; key_handler : Vdom_keyboard.Keyboard_event_handler.t
      ; inject : Action.t -> unit Vdom.Effect.t
      ; selection : Item.Set.t Key.Map.t
      }
    [@@deriving fields ~getters]

    let view_with_keydown_handler t =
      let open Vdom in
      let on_keydown =
        Attr.on_keydown (fun ev ->
          Vdom_keyboard.Keyboard_event_handler.handle_or_ignore_event t.key_handler ev)
      in
      Node.div ~attrs:[ on_keydown ] [ t.view ]
    ;;
  end

  let search_box_id key ~id_prefix = sprintf !"%s-search-box-%{Key}" id_prefix key

  let view ~inject ~focus ~subwidgets ~id_prefix =
    let open Vdom in
    let cross_subwidget_actions =
      let link ~text ~(action : Action.t) =
        Node.a
          ~attrs:
            [ Attr.many_without_merge
                [ Attr.href "about:blank"
                ; Attr.on_click (fun _ev ->
                    Effect.Many [ inject action; Effect.Prevent_default ])
                ]
            ]
          [ Node.text text ]
      in
      Node.div
        [ Node.text (sprintf "Select on all %s: " Key.name_plural)
        ; link ~text:"all" ~action:(Select_on_all_subwidgets `All)
        ; Node.text "; "
        ; link ~text:"none" ~action:(Select_on_all_subwidgets `None)
        ]
    in
    let subwidgets =
      Map.mapi subwidgets ~f:(fun ~key ~data:result ->
        let is_focused = [%compare.equal: Key.t] focus key in
        Node.div
          ~attrs:
            [ Attr.many_without_merge
                [ Attr.classes
                    [ "multi-factor-subwidget"
                    ; (if is_focused
                       then "multi-factor-focused-subwidget"
                       else "multi-factor-unfocused-subwidget")
                    ]
                ; Attr.on_click (fun _ev -> inject (Set_focused_subwidget key))
                ; Attr.id (sprintf !"%s-%{Key}" id_prefix key)
                ]
            ]
          [ result.Single_factor.Result.view ])
    in
    Vdom_layout.as_vbox
      Node.div
      [ cross_subwidget_actions; Vdom_layout.as_hbox Node.div (Map.data subwidgets) ]
  ;;

  let key_handler ~inject ~focus ~subwidgets =
    let open Vdom_keyboard in
    let my_key_handler =
      let command ?cond ~keys ~description f =
        let handler =
          let open Keyboard_event_handler.Handler in
          match cond with
          | None -> with_prevent_default f
          | Some cond -> only_handle_if cond f ~prevent_default:()
        in
        { Keyboard_event_handler.Command.keys; description; group = None; handler }
      in
      let key = Keystroke.create' in
      Keyboard_event_handler.of_command_list_exn
        [ command
            ~keys:[ key Tab ]
            ~description:(sprintf "Focus next %s" Key.name_singular)
            (fun _ev -> inject (Action.Cycle_focused_subwidget `Next))
        ; command
            ~keys:[ key ~shift:() Tab ]
            ~description:(sprintf "Focus prev %s" Key.name_singular)
            (fun _ev -> inject (Cycle_focused_subwidget `Prev))
        ]
    in
    let focused_subwidget_key_handler =
      let result = Map.find_exn subwidgets focus in
      result.Single_factor.Result.key_handler
    in
    Keyboard_event_handler.merge
      focused_subwidget_key_handler
      my_key_handler
      ~on_dup:`Override_with_right
  ;;

  let inject ~subwidgets ~inject_ring_focus_action = function
    | Action.Cycle_focused_subwidget dir ->
      inject_ring_focus_action (Ring_focus.Action.Cycle_focused_subwidget dir)
    | Set_focused_subwidget key -> inject_ring_focus_action (Set_focused_subwidget key)
    | Subwidget_action (key, a) ->
      (match Map.find subwidgets key with
       | None -> Bonsai.Effect.Ignore
       | Some { Single_factor.Result.inject; _ } -> inject a)
    | Select_on_all_subwidgets what ->
      Bonsai.Effect.Many
        (List.map (Map.data subwidgets) ~f:(fun subwidget ->
           subwidget.inject
             (match what with
              | `All -> Select_all
              | `None -> Select_none)))
  ;;

  let view_for_testing ~subwidgets ~focus =
    lazy
      (let columns =
         List.map (Map.keys subwidgets) ~f:(fun key ->
           let name =
             sprintf
               !"%s %{Key}"
               (if [%compare.equal: Key.t] focus key then "*" else " ")
               key
           in
           Ascii_table_kernel.Column.create name (fun () ->
             let subwidget = Map.find_exn subwidgets key in
             Lazy.force subwidget.Single_factor.Result.view_for_testing))
       in
       Ascii_table_kernel.draw
         columns
         [ () ]
         ~limit_width_to:2000
         ~prefer_split_on_spaces:false
       |> Option.value_exn
       |> Ascii_table_kernel.Screen.to_string
            ~bars:`Unicode
            ~string_with_attr:(fun _attrs str -> str))
  ;;

  let focus_elt id =
    let open Js_of_ocaml in
    (* In tests, there is no [document] object, so we can't focus elements. *)
    if Js.Optdef.test (Js.def Dom_html.document)
    then
      Option.iter
        (Dom_html.getElementById_coerce id Dom_html.CoerceTo.input)
        ~f:(fun elt ->
        elt##focus;
        elt##select)
  ;;

  let focus_elt =
    let f = Effect.of_sync_fun focus_elt in
    fun ~id -> f id
  ;;

  let bonsai ?(initial_model_settings = Key.Map.empty) ~all_keys ~id_prefix subwidgets =
    let open Bonsai.Let_syntax in
    let single_factor key input =
      let default_selection_status = input >>| default_selection_status in
      let initial_model_settings =
        Map.find initial_model_settings key
        |> Option.value ~default:(Single_factor.Initial_model_settings.create ())
      in
      let view_config =
        let%map id_prefix = id_prefix in
        Single_factor.View_config.create
          ~id:(search_box_id key ~id_prefix)
          ~header:(Vdom.Node.text (Key.to_string key))
          ()
      in
      Single_factor.bonsai
        ~initial_model_settings
        ~default_selection_status
        ~view_config
        (input >>| all_items)
    in
    let%sub single_factors =
      all_keys
      |> Set.to_map ~f:(fun key ->
           match%sub subwidgets >>| Fn.flip Map.find key with
           | Some input -> Computation.map (single_factor key input) ~f:Option.some
           | None -> Bonsai.const None)
      |> Computation.all_map
      |> Computation.map ~f:(Map.filter_map ~f:Fn.id)
    in
    let%sub focus, inject_focus_action =
      Bonsai.of_module0
        (module Ring_focus)
        ~sexp_of_model:[%sexp_of: Ring_focus.Model.t]
        ~equal:[%equal: Ring_focus.Model.t]
        ~default_model:(Focus_ring.of_nonempty_list_exn (Set.to_list all_keys))
    in
    let%sub () =
      let callback =
        let%map id_prefix = id_prefix in
        fun prev new_focus ->
          match prev with
          | Some prev_focus when Key.equal prev_focus new_focus -> Effect.Ignore
          | None | Some _ -> focus_elt ~id:(search_box_id new_focus ~id_prefix)
      in
      Bonsai.Edge.on_change'
        ~sexp_of_model:[%sexp_of: Key.t]
        ~equal:[%equal: Key.t]
        focus
        ~callback
    in
    let%arr subwidgets = single_factors
    and focus = focus
    and inject_ring_focus_action = inject_focus_action
    and id_prefix = id_prefix in
    let inject = inject ~subwidgets ~inject_ring_focus_action in
    let selection =
      Map.map subwidgets ~f:(fun result -> result.Single_factor.Result.selected_items)
    in
    let view = view ~inject ~subwidgets ~focus ~id_prefix in
    let view_for_testing = view_for_testing ~subwidgets ~focus in
    let key_handler = key_handler ~inject ~subwidgets ~focus in
    { Result.selection; view; view_for_testing; key_handler; inject }
  ;;
end
