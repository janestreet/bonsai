open! Core
open! Bonsai_web
open Bonsai.Let_syntax

type t =
  { state : Color_list.t
  ; view : Vdom.Node.t
  ; step : unit Effect.t
  ; is_done : bool
  ; is_automating : bool
  }

module Modification = struct
  type t =
    | Add of int * float
    | Remove of int * float
    | Change of int * float * float
  [@@deriving sexp, equal]

  let apply (map : Color_list.t) (t : t) =
    match t with
    | Add (key, data) -> Map.add_exn map ~key ~data
    | Remove (key, _) -> Map.remove map key
    | Change (key, _, data) -> Map.set map ~key ~data
  ;;
end

module Model = struct
  type t =
    { cur : Color_list.t
    ; diffs : Modification.t list list
    ; pointer : int
    }
  [@@deriving sexp, equal]
end

module Action = struct
  type t =
    | Restart
    | Step
    | Set_state of Model.t
  [@@deriving sexp_of]
end

module Input = struct
  type t =
    { before : Color_list.t
    ; after : Color_list.t
    }
  [@@deriving sexp, equal]
end

let generate_diffs ~before ~after =
  Map.symmetric_diff before after ~data_equal:[%equal: float]
  |> Sequence.map ~f:(function
    | key, `Left v -> Modification.Remove (key, v)
    | key, `Right v -> Add (key, v)
    | key, `Unequal (l, r) -> Change (key, l, r))
  |> Sequence.to_list
  |> List.permute ~random_state:Random.State.default
  |> List.group ~break:(fun _ _ -> Random.bool ())
;;

let component ~(before_state : Color_list.t Value.t) ~(after_state : Color_list.t Value.t)
  =
  let%sub input =
    let%arr before = before_state
    and after = after_state in
    { Input.before; after }
  in
  let%sub state, inject =
    Bonsai.state_machine1
      (module Model)
      (module Action)
      input
      ~default_model:{ cur = Int.Map.empty; diffs = []; pointer = 0 }
      ~apply_action:(fun ~inject:_ ~schedule_event:_ input model action ->
        match input with
        | Active { Input.before; after } ->
          (match action with
           | Set_state model -> model
           | Restart ->
             let diffs = generate_diffs ~before ~after in
             { cur = before; diffs; pointer = 0 }
           | Step ->
             let packet =
               match List.nth model.diffs model.pointer with
               | Some packet -> packet
               | None -> []
             in
             let cur = List.fold packet ~init:model.cur ~f:Modification.apply in
             { model with cur; pointer = model.pointer + 1 })
        | Inactive ->
          eprint_s
            [%message
              [%here]
                "An action sent to a [state_machine1] has been dropped because its input \
                 was not present. This happens when the [state_machine1] is inactive \
                 when it receives a message."
                (action : Action.t)];
          model)
  in
  let%sub () =
    Bonsai.Edge.on_change
      (module Input)
      input
      ~callback:
        (let%map inject = inject in
         fun _ -> inject Restart)
  in
  let%sub help_toggler = Bonsai.toggle ~default_model:false in
  let%sub is_automating, toggle_automating = Bonsai.toggle ~default_model:false in
  let%sub ff_button =
    let%arr toggle_automating = toggle_automating in
    Style.fast_forward `Dark ~on_click:toggle_automating
  in
  let%arr state = state
  and inject = inject
  and help_open, toggle_help = help_toggler
  and is_automating = is_automating
  and ff_button = ff_button in
  let header =
    Vdom.Node.div
      ~attr:Style.header
      [ Style.refresh_button `Dark ~on_click:(inject Restart)
      ; Vdom.Node.text "step"
      ; Style.arrow_right `Dark ~on_click:(inject Step)
      ; ff_button
      ; Style.help `Dark ~on_click:toggle_help
      ]
  in
  let body =
    Vdom.Node.div
      (List.mapi state.diffs ~f:(fun i packet ->
         let has_executed = i < state.pointer in
         let is_selected = i = state.pointer in
         let classes =
           if is_selected
           then [ Style.selected; Style.packet ]
           else if has_executed
           then [ Style.executed; Style.packet ]
           else [ Style.packet ]
         in
         Vdom.Node.div
           ~attr:(Vdom.Attr.many classes)
           (List.map packet ~f:(fun diff ->
              let elements =
                match diff with
                | Add (k, color) ->
                  [ Vdom.Node.span [ Vdom.Node.textf "add %d" k ]
                  ; Style.chip color Vdom.Node.none
                  ]
                | Remove (k, color) ->
                  [ Vdom.Node.span [ Vdom.Node.textf "rem %d" k ]
                  ; Style.chip color Vdom.Node.none
                  ]
                | Change (k, before, after) ->
                  [ Vdom.Node.span [ Vdom.Node.textf "chg %d" k ]
                  ; Style.chip before Vdom.Node.none
                  ; Style.chip after Vdom.Node.none
                  ]
              in
              let classes = [ Style.diff ] in
              Vdom.Node.div ~attr:(Vdom.Attr.many classes) elements))))
  in
  let is_done = state.pointer >= List.length state.diffs in
  let debug =
    if help_open
    then
      Vdom.Node.div
        [ Vdom.Node.textarea
            ~attr:
              (Vdom.Attr.many
                 [ Vdom.Attr.many [ Style.header; Style.debug ]
                 ; Vdom.Attr.value_prop ([%sexp_of: Model.t] state |> Sexp.to_string_hum)
                 ; Vdom.Attr.on_change (fun _ text ->
                     text
                     |> Sexp.of_string
                     |> [%of_sexp: Model.t]
                     |> Action.Set_state
                     |> inject)
                 ])
            []
        ]
    else Vdom.Node.none
  in
  let view = Vdom.Node.div ~attr:Style.color_list [ header; debug; body ] in
  { state = state.cur; is_done; view; step = inject Step; is_automating }
;;
