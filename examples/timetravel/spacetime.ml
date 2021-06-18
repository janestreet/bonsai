open! Core
open Bonsai_web

module Model = struct
  type 'm t =
    { inner : 'm
    ; cursor : Spacetime_tree.Cursor.t
    ; history : 'm Spacetime_tree.t
    }
  [@@deriving equal, fields, sexp]

  let create m =
    let history, cursor = Spacetime_tree.create m in
    { inner = m; cursor; history }
  ;;
end

module Action = struct
  type 'a t =
    | Inner of 'a
    | Set_cursor of Spacetime_tree.Cursor.t
  [@@deriving sexp_of]
end

module Result = struct
  type t = Vdom.Node.t -> Vdom.Node.t
end

let draw_history ~active_cursor ~inject ~data:_ ~cursor ~children =
  let children = List.rev children in
  let on_click = Vdom.Attr.on_click (fun _ -> inject (Action.Set_cursor cursor)) in
  let button_attrs =
    if Spacetime_tree.Cursor.equal active_cursor cursor
    then [ on_click; Vdom.Attr.class_ "current" ]
    else [ on_click ]
  in
  let my_node = Vdom.Node.button ~attr:(many button_attrs) [ Vdom.Node.text "●" ] in
  let children = Vdom.Node.div ~attr:(Vdom.Attr.class_ "ch") children in
  Vdom.Node.div ~attr:(Vdom.Attr.class_ "cnt") [ my_node; children ]
;;

let view cursor history ~inject =
  let open Incr.Let_syntax in
  let%map cursor = cursor
  and history = history in
  let spacetime =
    Spacetime_tree.traverse history ~f:(draw_history ~inject ~active_cursor:cursor)
  in
  let spacetime =
    Vdom.Node.div ~attr:(Vdom.Attr.class_ "history_wrapper") [ spacetime ]
  in
  fun window ->
    Vdom.Node.div ~attr:(Vdom.Attr.class_ "history_wrapper_wrapper") [ window; spacetime ]
;;

let create (type i r) (inner_component : (i, r) Bonsai.t) : (i, r * Result.t) Bonsai.t =
  let (T
         { unpacked = inner_unpacked
         ; action_type_id = inner_action_type_id
         ; model =
             { default = inner_default_model
             ; equal = inner_model_equal
             ; type_id = inner_model_type_id
             ; sexp_of = sexp_of_inner_model
             ; of_sexp = inner_model_of_sexp
             }
         })
    =
    inner_component |> Bonsai.to_generic |> Bonsai_lib.Generic.Expert.reveal
  in
  let open Incr.Let_syntax in
  let action_type_id =
    Type_equal.Id.create
      ~name:(Source_code_position.to_string [%here])
      (function
        | Action.Inner a -> Type_equal.Id.to_sexp inner_action_type_id a
        | Set_cursor cursor -> [%sexp "Set_cursor", (cursor : Spacetime_tree.Cursor.t)])
  in
  let model_type_id =
    Type_equal.Id.create
      ~name:(Source_code_position.to_string [%here])
      (Model.sexp_of_t (Type_equal.Id.to_sexp inner_model_type_id))
  in
  let default_model = Model.create inner_default_model in
  let model_equal = Model.equal inner_model_equal in
  Bonsai_lib.Generic.Expert.of_full
    [%here]
    ~action_type_id
    ~model_type_id
    ~default_model
    ~model_equal
    ~sexp_of_model:[%sexp_of: inner_model Model.t]
    ~model_of_sexp:[%of_sexp: inner_model Model.t]
    ~f:(fun ~input ~old_model ~model ~inject ~environment ~incr_state:_ ->
      let inject_inner a = inject (Action.Inner a) in
      let inner_model = model >>| Model.inner in
      let inner_old_model = old_model >>| Option.map ~f:Model.inner in
      let inner =
        Bonsai_lib.Generic.Expert.eval
          ~input
          ~old_model:inner_old_model
          ~model:inner_model
          ~inject:inject_inner
          ~action_type_id:inner_action_type_id
          ~environment
          ~incr_state:Incr.State.t
          inner_unpacked
      in
      let apply_action =
        let%map model = model
        and inner = inner in
        fun ~schedule_event -> function
          | Action.Inner a ->
            let inner =
              Bonsai_lib.Generic.Expert.Snapshot.apply_action inner ~schedule_event a
            in
            let history, cursor =
              Spacetime_tree.append model.history model.cursor inner
            in
            { Model.inner; history; cursor }
          | Action.Set_cursor cursor ->
            let inner = Spacetime_tree.find model.history cursor in
            { model with inner; cursor }
      in
      let result =
        let%map inner = inner
        and view = view ~inject (model >>| Model.cursor) (model >>| Model.history) in
        Bonsai_lib.Generic.Expert.Snapshot.result inner, view
      in
      let%map apply_action = apply_action
      and result = result in
      Bonsai_lib.Generic.Expert.Snapshot.create ~result ~apply_action)
  |> Bonsai_lib.Generic.Expert.conceal
  |> Bonsai.of_generic
;;
