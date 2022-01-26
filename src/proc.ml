open! Core
open! Import
module Var = Var

let unusable_static_apply_action ~inject:_ ~schedule_event:_ _model =
  Nothing.unreachable_code
;;

let sub
      (type via)
      ?here
      (Computation.T
         { t = from
         ; dynamic_action = from_dynamic_action
         ; static_action = from_static_action
         ; model = from_model
         ; apply_static = from_apply_static
         } :
         via Computation.packed)
      ~f
  =
  match from with
  | Return { here = there; value = Named _ as value } ->
    let here = Option.first_some here there in
    f { Value.here; value }
  | _ ->
    let via : via Type_equal.Id.t =
      Type_equal.Id.create
        ~name:(Source_code_position.to_string [%here])
        [%sexp_of: opaque]
    in
    let (Computation.T
           { t = into
           ; dynamic_action = into_dynamic_action
           ; static_action = into_static_action
           ; model = into_model
           ; apply_static = into_apply_static
           })
      =
      f (Value.named via)
    in
    (match
       Type_equal.Id.(
         ( ( same_witness from_model.type_id Meta.Model.unit.type_id
           , same_witness from_dynamic_action Meta.Action.nothing
           , same_witness from_static_action Meta.Action.nothing )
         , ( same_witness into_model.type_id Meta.Model.unit.type_id
           , same_witness into_dynamic_action Meta.Action.nothing
           , same_witness into_static_action Meta.Action.nothing ) ))
     with
     | (Some T, Some T, Some T), _ ->
       Computation.T
         { t = Subst_stateless_from { from; via; into; here }
         ; dynamic_action = into_dynamic_action
         ; static_action = into_static_action
         ; model = into_model
         ; apply_static = into_apply_static
         }
     | _, (Some T, Some T, Some T) ->
       Computation.T
         { t = Subst_stateless_into { from; via; into; here }
         ; dynamic_action = from_dynamic_action
         ; static_action = from_static_action
         ; model = from_model
         ; apply_static = from_apply_static
         }
     | _ ->
       let apply_static ~inject ~schedule_event (m1, m2) = function
         | First a ->
           let inject a = inject (First a) in
           from_apply_static ~inject ~schedule_event m1 a, m2
         | Second a ->
           let inject a = inject (Second a) in
           m1, into_apply_static ~inject ~schedule_event m2 a
       in
       Computation.T
         { t = Subst { from; via; into; here }
         ; dynamic_action = Meta.Action.both from_dynamic_action into_dynamic_action
         ; static_action = Meta.Action.both from_static_action into_static_action
         ; model = Meta.Model.both from_model into_model
         ; apply_static
         })
;;

let read x =
  Computation.T
    { t = Return x
    ; model = Meta.Model.unit
    ; dynamic_action = Meta.Action.nothing
    ; static_action = Meta.Action.nothing
    ; apply_static = unusable_static_apply_action
    }
;;

let switch ~match_ ~branches ~with_ =
  let create_case key =
    let component = with_ key in
    let (Computation.T { model; _ }) = component in
    let default_model = Hidden.Model.create model model.default in
    component, default_model
  in
  let arms, models =
    List.fold
      (List.range 0 branches)
      ~init:(Int.Map.empty, Int.Map.empty)
      ~f:(fun (components, models) key ->
        let component, model = create_case key in
        let components = Map.add_exn components ~key ~data:component in
        let models = Map.add_exn models ~key ~data:model in
        components, models)
  in
  let apply_static ~inject ~schedule_event model (action : int Hidden.Action.t) =
    let (T { action; type_id = action_type_id; key = index }) = action in
    let (T { model = chosen_model; info = chosen_model_info; _ }) =
      Hidden.Multi_model.find_exn model index
    in
    let inject action =
      inject (Hidden.Action.T { action; type_id = action_type_id; key = index })
    in
    let (T { t = _; model = tm; static_action = am; apply_static; dynamic_action = _ }) =
      Map.find_exn arms index
    in
    let T = Type_equal.Id.same_witness_exn tm.type_id chosen_model_info.type_id in
    let T = Type_equal.Id.same_witness_exn am action_type_id in
    let new_model = apply_static ~inject ~schedule_event chosen_model action in
    let new_model = Hidden.Model.create tm new_model in
    Hidden.Multi_model.set model ~key:index ~data:new_model
  in
  Computation.T
    { t = Switch { match_; arms }
    ; model = Hidden.Multi_model.model_info (module Int) models
    ; dynamic_action = Hidden.Action.type_id [%sexp_of: int]
    ; static_action = Hidden.Action.type_id [%sexp_of: int]
    ; apply_static
    }
;;

module Let_syntax = struct
  let return = read
  let ( <*> ) = Value.( <*> )
  let ( <$> ) f = Value.map ~f
  let ( >>| ) a f = Value.map a ~f

  module Let_syntax = struct
    let sub = sub
    let switch = switch
    let return = return
    let map = Value.map
    let both = Value.both
    let arr ?here t ~f = read { (Value.map t ~f) with here }

    include (Value : Mapn with type 'a t := 'a Value.t)
  end
end

open Let_syntax

let pure f i = read (Value.map i ~f)
let const x = read (Value.return x)

let with_model_resetter
      (Computation.T { t; model; static_action; dynamic_action; apply_static })
  =
  let static_action = Meta.(Action.both unit_type_id static_action) in
  let apply_static ~inject ~schedule_event m =
    let inject a = inject (Second a) in
    function
    | First () -> model.default
    | Second a -> apply_static ~inject ~schedule_event m a
  in
  Computation.T
    { t = With_model_resetter t; model; static_action; dynamic_action; apply_static }
;;

let assoc
      (type k v cmp)
      (comparator : (k, cmp) comparator)
      (map : (k, v, cmp) Map.t Value.t)
      ~f
  =
  let module C = (val comparator) in
  let key_id : k Type_equal.Id.t = Type_equal.Id.create ~name:"key id" C.sexp_of_t in
  let data_id : v Type_equal.Id.t =
    Type_equal.Id.create ~name:"data id" [%sexp_of: opaque]
  in
  let key_var = Value.named key_id in
  let data_var = Value.named data_id in
  let (Computation.T
         { t = by; model = model_info; dynamic_action; static_action; apply_static })
    =
    f key_var data_var
  in
  match
    Simplify.computation_to_function by ~key_compare:C.comparator.compare ~key_id ~data_id
  with
  | Some by ->
    Computation.T
      { t = Assoc_simpl { map; key_id; data_id; by; result_by_k = T }
      ; dynamic_action = Meta.Action.nothing
      ; static_action = Meta.Action.nothing
      ; apply_static = unusable_static_apply_action
      ; model = Meta.Model.unit
      }
  | None ->
    let apply_static ~inject ~schedule_event model (id, action) =
      let inject a = inject (id, a) in
      let specific_model =
        Map.find model id |> Option.value ~default:model_info.default
      in
      let data = apply_static ~inject ~schedule_event specific_model action in
      if model_info.equal data model_info.default
      then Map.remove model id
      else Map.set model ~key:id ~data
    in
    let module Cmp = (val comparator) in
    Computation.T
      { t =
          Assoc
            { map
            ; key_compare = Cmp.comparator.compare
            ; key_id
            ; data_id
            ; by
            ; model_info
            ; action_info = dynamic_action
            ; result_by_k = T
            ; model_by_k = T
            }
      ; dynamic_action = Meta.Action.map comparator dynamic_action
      ; static_action = Meta.Action.map comparator static_action
      ; model = Meta.Model.map comparator model_info
      ; apply_static
      }
;;

let enum (type k) (module E : Enum with type t = k) ~match_ ~with_ =
  let module E = struct
    include E
    include Comparator.Make (E)
  end
  in
  let forward_index = List.to_array E.all in
  let reverse_index =
    Map.of_alist_exn (module E) (List.mapi E.all ~f:(fun i k -> k, i))
  in
  let match_ = match_ >>| Map.find_exn reverse_index in
  let branches = Array.length forward_index in
  let with_ i = with_ (Array.get forward_index i) in
  Let_syntax.switch ~match_ ~branches ~with_
;;

let state_machine1
      (type m a)
      here
      (module M : Model with type t = m)
      (module A : Action with type t = a)
      ~default_model
      ~apply_action
      input
  =
  let name = Source_code_position.to_string here in
  Computation.T
    { t =
        Leaf1 { input; dynamic_apply_action = apply_action; name; kind = "state machine" }
    ; model = Meta.Model.of_module (module M) ~name ~default:default_model
    ; dynamic_action = Meta.Action.of_module (module A) ~name
    ; static_action = Meta.Action.nothing
    ; apply_static = unusable_static_apply_action
    }
;;

let of_module1 (type i m a r) (component : (i, m, a, r) component_s) ~default_model input =
  let (module M) = component in
  let%sub input = return input in
  let%sub model_and_inject =
    state_machine1
      [%here]
      (module M.Model)
      (module M.Action)
      ~default_model
      ~apply_action:M.apply_action
      input
  in
  let%arr model, inject = model_and_inject
  and input = input in
  M.compute ~inject input model
;;

let of_module2 c ~default_model i1 i2 = of_module1 c ~default_model (Value.both i1 i2)

let state_machine0 here model action ~default_model ~apply_action =
  let apply_action ~inject ~schedule_event model action =
    apply_action ~inject ~schedule_event model action
  in
  let compute ~inject model = model, inject in
  let name = Source_code_position.to_string here in
  Computation.T
    { t = Leaf0 { compute; name; kind = "state machine" }
    ; model = Meta.Model.of_module model ~name ~default:default_model
    ; dynamic_action = Meta.Action.nothing
    ; static_action = Meta.Action.of_module action ~name
    ; apply_static = apply_action
    }
;;

let of_module0 (type m a r) (component : (unit, m, a, r) component_s) ~default_model =
  let (module M) = component in
  let%sub model_and_inject =
    state_machine0
      [%here]
      (module M.Model)
      (module M.Action)
      ~default_model
      ~apply_action:(M.apply_action ())
  in
  let%arr model, inject = model_and_inject in
  M.compute ~inject () model
;;

let actor1
  : type input model action return.
    Source_code_position.t
    -> (module Model with type t = model)
    -> (module Action with type t = action)
    -> default_model:model
    -> recv:
         (schedule_event:(unit Ui_effect.t -> unit)
          -> input
          -> model
          -> action
          -> model * return)
    -> input Value.t
    -> (model * (action -> return Effect.t)) Computation.packed
  =
  fun here
    (module M : Model with type t = model)
    (module A : Action with type t = action)
    ~default_model
    ~recv
    input ->
    let module Action_with_callback = struct
      type t = (action, return) Effect.Private.Callback.t

      let sexp_of_t cb = A.sexp_of_t (Effect.Private.Callback.request cb)
    end
    in
    let%sub model_and_inject =
      state_machine1
        here
        (module M)
        (module Action_with_callback)
        ~default_model
        ~apply_action:(fun ~inject:_ ~schedule_event input model callback ->
          let action = Effect.Private.Callback.request callback in
          let new_model, response = recv ~schedule_event input model action in
          schedule_event (Effect.Private.Callback.respond_to callback response);
          new_model)
        input
    in
    let%arr model, inject = model_and_inject in
    let inject action = Effect.Private.make ~request:action ~evaluator:inject in
    model, inject
;;

let actor0 here model action ~default_model ~recv =
  let recv ~schedule_event () = recv ~schedule_event in
  actor1 here model action ~default_model ~recv (Value.return ())
;;

let lazy_ t =
  let open struct
    type model = Hidden.Model.t option [@@deriving equal, sexp_of]
  end in
  let dynamic_action = Hidden.Action.type_id [%sexp_of: unit] in
  let static_action = Hidden.Action.type_id [%sexp_of: unit] in
  let model =
    { Meta.Model.default = None
    ; equal = equal_model
    ; type_id = Type_equal.Id.create ~name:"lazy-model" [%sexp_of: model]
    ; sexp_of = [%sexp_of: model]
    ; of_sexp = (fun _ -> None)
    }
  in
  let apply_static ~inject ~schedule_event model action =
    (* forcing the lazy is fine because actions are finite in length *)
    let (lazy
          (Computation.T
             { t = _
             ; model = model_info
             ; static_action = action_info
             ; dynamic_action = _
             ; apply_static
             }))
      =
      t
    in
    let inject action =
      inject (Hidden.Action.T { action; type_id = action_info; key = () })
    in
    let (Hidden.Action.T { action; type_id = action_type_id; key = () }) = action in
    let (Hidden.Model.T { model = chosen_model; info = chosen_model_info; _ }) =
      Option.value model ~default:(Hidden.Model.create model_info model_info.default)
    in
    let T = Type_equal.Id.same_witness_exn action_type_id action_info in
    let T = Type_equal.Id.same_witness_exn chosen_model_info.type_id model_info.type_id in
    let new_model = apply_static ~inject ~schedule_event chosen_model action in
    Some (Hidden.Model.create model_info new_model)
  in
  Computation.T { t = Lazy t; dynamic_action; static_action; model; apply_static }
;;

let wrap (type model action) model_module ~default_model ~apply_action ~f =
  let model_id : model Type_equal.Id.t =
    Type_equal.Id.create ~name:"model id" [%sexp_of: opaque]
  in
  let action_id : action Type_equal.Id.t =
    Type_equal.Id.create ~name:"action id" [%sexp_of: opaque]
  in
  let inject_id : (action -> unit Effect.t) Type_equal.Id.t =
    Type_equal.Id.create ~name:"inject id" [%sexp_of: opaque]
  in
  let model_var = Value.named model_id in
  let inject_var = Value.named inject_id in
  let (Computation.T
         { t = inner
         ; model = inner_model
         ; dynamic_action = inner_dynamic_action
         ; static_action = inner_static_action
         ; apply_static
         })
    =
    f model_var inject_var
  in
  let dynamic_action = Meta.Action.both action_id inner_dynamic_action in
  let model =
    Meta.Model.both
      (Meta.Model.of_module
         model_module
         ~default:default_model
         ~name:"outer model for wrap")
      inner_model
  in
  let apply_static ~inject ~schedule_event (m1, m2) action =
    m1, apply_static ~inject ~schedule_event m2 action
  in
  Computation.T
    { t =
        Computation.Wrap
          { model_id; inject_id; inner; dynamic_apply_action = apply_action }
    ; dynamic_action
    ; static_action = inner_static_action
    ; apply_static
    ; model
    }
;;

let state (type m) here (module M : Model with type t = m) ~default_model =
  state_machine0
    here
    (module M)
    (module M)
    ~apply_action:(fun ~inject:_ ~schedule_event:_ _old_model new_model -> new_model)
    ~default_model
;;

let state_opt (type m) here ?default_model (module M : Model with type t = m) =
  state
    here
    ~default_model
    (module struct
      type t = M.t option [@@deriving equal, sexp]
    end)
;;

let path =
  Computation.T
    { t = Computation.Path
    ; dynamic_action = Meta.Action.nothing
    ; static_action = Meta.Action.nothing
    ; apply_static = unusable_static_apply_action
    ; model = Meta.Model.unit
    }
;;

let path_id =
  let%sub path = path in
  let%arr path = path in
  Path.to_unique_identifier_string path
;;

module Edge = struct
  let lifecycle' ?on_activate ?on_deactivate ?after_display () =
    let transpose_join : 'a option Value.t option -> 'a option Value.t = function
      | Some a -> a
      | None -> Value.return None
    in
    let%sub triple =
      let%arr a = transpose_join on_activate
      and b = transpose_join on_deactivate
      and c = transpose_join after_display in
      a, b, c
    in
    let%sub t =
      match%arr triple with
      | None, None, None -> None
      | on_activate, on_deactivate, after_display ->
        Some { Lifecycle.on_activate; on_deactivate; after_display }
    in
    Computation.T
      { t = Lifecycle t
      ; model = Meta.Model.unit
      ; dynamic_action = Meta.Action.nothing
      ; static_action = Meta.Action.nothing
      ; apply_static = unusable_static_apply_action
      }
  ;;

  let lifecycle ?on_activate ?on_deactivate ?after_display () =
    lifecycle'
      ?on_activate:(Option.map on_activate ~f:(Value.map ~f:Option.some))
      ?on_deactivate:(Option.map on_deactivate ~f:(Value.map ~f:Option.some))
      ?after_display:(Option.map after_display ~f:(Value.map ~f:Option.some))
      ()
  ;;

  let after_display' event_opt_value = lifecycle' ~after_display:event_opt_value ()

  let after_display event_value =
    let event_value = Value.map event_value ~f:Option.some in
    lifecycle' ~after_display:event_value ()
  ;;

  let on_change' (type a) here (module M : Model with type t = a) input ~callback =
    let%sub state, set_state = state_opt here (module M) in
    let%sub update =
      match%sub state with
      | None ->
        let%arr set_state = set_state
        and input = input
        and callback = callback in
        Some (Ui_effect.Many [ set_state (Some input); callback None input ])
      | Some state ->
        let%arr state = state
        and set_state = set_state
        and input = input
        and callback = callback in
        if phys_equal state input || M.equal state input
        then None
        else
          lazy (Ui_effect.Many [ set_state (Some input); callback (Some state) input ])
          |> Ui_effect.lazy_
          |> Some
    in
    after_display' update
  ;;

  let on_change here model input ~callback =
    let callback = Value.map callback ~f:(fun callback _prev value -> callback value) in
    on_change' here model input ~callback
  ;;

  module Poll = struct
    module Starting = struct
      type ('a, 'r) t =
        | Empty : ('a, 'a option) t
        | Initial : 'a -> ('a, 'a) t

      let empty = Empty
      let initial a = Initial a
    end

    let poll_effect_on_change_implementation
          (type i r)
          here
          (module Input : Model with type t = i)
          (module Result : Model with type t = r)
          ~initial
          ~wrap_result
          ~effect
          input
      =
      let%sub _, next_seqnum =
        actor0
          here
          (module Int)
          (module Unit)
          ~default_model:0
          ~recv:(fun ~schedule_event:_ i () -> i + 1, i)
      in
      let module State = struct
        type t =
          { last_seqnum : int
          ; last_result : Result.t
          }
        [@@deriving sexp, equal, fields]
      end
      in
      let module Action = struct
        type t = Set of int * Result.t [@@deriving sexp_of]
      end
      in
      let%sub state =
        state_machine0
          here
          (module State)
          (module Action)
          ~apply_action:
            (fun ~inject:_ ~schedule_event:_ model (Action.Set (seqnum, res)) ->
               if seqnum < model.State.last_seqnum
               then model
               else { State.last_seqnum = seqnum; last_result = res })
          ~default_model:{ State.last_seqnum = -1; last_result = initial }
      in
      let callback =
        let%map effect = effect
        and next_seqnum = next_seqnum
        and _, inject_change = state in
        fun input ->
          let%bind.Effect seqnum = next_seqnum () in
          let%bind.Effect result = effect input in
          inject_change (Action.Set (seqnum, wrap_result result))
      in
      let%sub () = on_change here (module Input) input ~callback in
      let%arr { State.last_result; _ }, _ = state in
      last_result
    ;;

    let effect_on_change
      : type a o r.
        Source_code_position.t
        -> (module Model with type t = a)
        -> (module Model with type t = o)
        -> (o, r) Starting.t
        -> a Value.t
        -> effect:(a -> o Effect.t) Value.t
        -> r Computation.packed
      =
      fun here
        (module Input : Model with type t = a)
        (module Result : Model with type t = o)
        (kind : (o, r) Starting.t)
        input
        ~effect ->
        match kind with
        | Starting.Empty ->
          poll_effect_on_change_implementation
            here
            (module Input)
            (module struct
              type t = Result.t option [@@deriving sexp, equal]
            end)
            ~effect
            ~initial:None
            ~wrap_result:Option.some
            input
        | Starting.Initial initial ->
          poll_effect_on_change_implementation
            here
            (module Input)
            (module Result)
            ~effect
            ~initial
            ~wrap_result:Fn.id
            input
    ;;
  end
end

module Incr = struct
  let value_cutoff t ~equal = read (Value.cutoff ~equal t)

  let model_cutoff
        (Computation.T { t; dynamic_action; static_action; model; apply_static })
    =
    Computation.T
      { t = Computation.Model_cutoff { t; model }
      ; dynamic_action
      ; static_action
      ; apply_static
      ; model
      }
  ;;

  let compute_with_clock t ~f =
    let dynamic_apply_action _input ~inject:_ =
      Incr.return (fun ~schedule_event:_ _model -> Nothing.unreachable_code)
    in
    let compute clock input _model ~inject:_ = f clock input in
    Computation.T
      { t =
          Computation.Leaf_incr
            { name = "incr-compute"; input = t; dynamic_apply_action; compute }
      ; dynamic_action = Meta.Action.nothing
      ; static_action = Meta.Action.nothing
      ; apply_static = unusable_static_apply_action
      ; model = Meta.Model.unit
      }
  ;;

  let compute t ~f = compute_with_clock t ~f:(fun _ input -> f input)
  let with_clock f = compute_with_clock (Value.return ()) ~f:(fun clock _ -> f clock)
  let to_value incr = { Value.value = Value.Incr incr; here = None }
end

module Dynamic_scope = struct
  type _ t =
    | Independent :
        { id : 'a Type_equal.Id.t
        ; fallback : 'a
        }
        -> 'a t
    | Derived :
        { base : 'a t
        ; get : 'a -> 'b
        ; set : 'a -> 'b -> 'a
        ; sexp_of : 'b -> Sexp.t
        }
        -> 'b t

  let rec fallback : type a. a t -> a = function
    | Independent { fallback; _ } -> fallback
    | Derived { base; get; set = _; sexp_of = _ } -> get (fallback base)
  ;;

  let create ?(sexp_of = sexp_of_opaque) ~name ~fallback () =
    Independent { id = Type_equal.Id.create ~name sexp_of; fallback }
  ;;

  let derived ?(sexp_of = sexp_of_opaque) base ~get ~set =
    Derived { base; get; set; sexp_of }
  ;;

  let rec fetch : type a b. a t -> default:b -> for_some:(a -> b) -> b Computation.packed =
    fun t ~default ~for_some ->
      match t with
      | Independent { id; _ } ->
        Computation.T
          { t = Computation.Fetch { id; default; for_some }
          ; dynamic_action = Meta.Action.nothing
          ; static_action = Meta.Action.nothing
          ; apply_static = unusable_static_apply_action
          ; model = Meta.Model.unit
          }
      | Derived { base; get; set = _; sexp_of = _ } ->
        fetch base ~default ~for_some:(fun x -> for_some (get x))
  ;;

  let lookup (type a) (var : a t) = fetch var ~default:(fallback var) ~for_some:Fn.id

  let rec store
    : type a. a t -> a Value.t -> 'r Computation.packed -> 'r Computation.packed
    =
    fun var value c ->
    match var with
    | Independent { id; _ } ->
      let (Computation.T t) = c in
      Computation.T { t with t = Computation.Store { id; value; inner = t.t } }
    | Derived { base; get = _; set; sexp_of = _ } ->
      let%sub current = lookup base in
      let%sub new_ =
        let%arr current = current
        and value = value in
        set current value
      in
      store base new_ c
  ;;

  type revert = { revert : 'a. 'a Computation.packed -> 'a Computation.packed }

  let modify var ~change ~f =
    let%sub current = lookup var in
    let revert c = store var current c in
    let value = change current in
    store var value (f { revert })
  ;;

  let set t v ~inside = store t v inside
  let set' var value ~f = modify var ~change:(fun _ -> value) ~f
end

module Clock = struct
  let approx_now ~tick_every =
    Incr.with_clock (fun clock ->
      let%map.Ui_incr () = Ui_incr.Clock.at_intervals clock tick_every in
      Ui_incr.Clock.now clock)
  ;;

  let now = Incr.with_clock Ui_incr.Clock.watch_now

  module Before_or_after = struct
    type t = Ui_incr.Before_or_after.t =
      | Before
      | After
    [@@deriving sexp, equal]
  end

  let at time =
    Incr.compute_with_clock time ~f:(fun clock ->
      Ui_incr.bind ~f:(Ui_incr.Clock.at clock))
  ;;

  module Time_ns_model = struct
    include Time_ns.Stable.Alternate_sexp.V1

    let equal = Time_ns.equal
  end

  let get_current_time =
    Incr.with_clock (fun clock ->
      Ui_incr.return (Effect.of_sync_fun (fun () -> Ui_incr.Clock.now clock) ()))
  ;;

  let every here span callback =
    let%sub input =
      Incr.with_clock (fun clock ->
        (* Even though this node has type unit (which should aggresively get cut
           off), the documentation for [at_intervals] mentions that the node has
           its cutoff manually overridden to never cut-off. *)
        let%map.Ui_incr () = Ui_incr.Clock.at_intervals clock span in
        (* The value of this node is the current time, which isn't actually used,
           but it's a nice monotonically increasing value, so we don't need to
           worry about cutoff issues. *)
        Ui_incr.Clock.now clock)
    in
    let callback =
      let%map callback = callback in
      (* Ignore the time, which we only really used to get good cutoff behavior *)
      fun (_ : Time_ns.t) -> callback
    in
    Edge.on_change here (module Time_ns_model) input ~callback
  ;;
end

module Computation = struct
  type 'a t = 'a Computation.packed

  include Applicative.Make_using_map2 (struct
      type nonrec 'a t = 'a t

      let return = const

      let map2 a b ~f =
        let%sub a = a in
        let%sub b = b in
        let%arr a = a
        and b = b in
        f a b
      ;;

      let map a ~f =
        let%sub a = a in
        let%arr a = a in
        f a
      ;;

      let map = `Custom map
    end)

  module Mapn = struct
    let map2 = map2

    let map3 t1 t2 t3 ~f =
      let%sub t1 = t1 in
      let%sub t2 = t2 in
      let%sub t3 = t3 in
      read (Value.map3 t1 t2 t3 ~f)
    ;;

    let map4 t1 t2 t3 t4 ~f =
      let%sub t1 = t1 in
      let%sub t2 = t2 in
      let%sub t3 = t3 in
      let%sub t4 = t4 in
      read (Value.map4 t1 t2 t3 t4 ~f)
    ;;

    let map5 t1 t2 t3 t4 t5 ~f =
      let%sub t1 = t1 in
      let%sub t2 = t2 in
      let%sub t3 = t3 in
      let%sub t4 = t4 in
      let%sub t5 = t5 in
      read (Value.map5 t1 t2 t3 t4 t5 ~f)
    ;;

    let map6 t1 t2 t3 t4 t5 t6 ~f =
      let%sub t1 = t1 in
      let%sub t2 = t2 in
      let%sub t3 = t3 in
      let%sub t4 = t4 in
      let%sub t5 = t5 in
      let%sub t6 = t6 in
      read (Value.map6 t1 t2 t3 t4 t5 t6 ~f)
    ;;

    let map7 t1 t2 t3 t4 t5 t6 t7 ~f =
      let%sub t1 = t1 in
      let%sub t2 = t2 in
      let%sub t3 = t3 in
      let%sub t4 = t4 in
      let%sub t5 = t5 in
      let%sub t6 = t6 in
      let%sub t7 = t7 in
      read (Value.map7 t1 t2 t3 t4 t5 t6 t7 ~f)
    ;;
  end

  include Mapn

  let rec all = function
    | [] -> return []
    | [ t1 ] -> map t1 ~f:(fun a1 -> [ a1 ])
    | [ t1; t2 ] -> map2 t1 t2 ~f:(fun a1 a2 -> [ a1; a2 ])
    | [ t1; t2; t3 ] -> map3 t1 t2 t3 ~f:(fun a1 a2 a3 -> [ a1; a2; a3 ])
    | [ t1; t2; t3; t4 ] -> map4 t1 t2 t3 t4 ~f:(fun a1 a2 a3 a4 -> [ a1; a2; a3; a4 ])
    | [ t1; t2; t3; t4; t5 ] ->
      map5 t1 t2 t3 t4 t5 ~f:(fun a1 a2 a3 a4 a5 -> [ a1; a2; a3; a4; a5 ])
    | [ t1; t2; t3; t4; t5; t6 ] ->
      map6 t1 t2 t3 t4 t5 t6 ~f:(fun a1 a2 a3 a4 a5 a6 -> [ a1; a2; a3; a4; a5; a6 ])
    | [ t1; t2; t3; t4; t5; t6; t7 ] ->
      map7 t1 t2 t3 t4 t5 t6 t7 ~f:(fun a1 a2 a3 a4 a5 a6 a7 ->
        [ a1; a2; a3; a4; a5; a6; a7 ])
    | t1 :: t2 :: t3 :: t4 :: t5 :: t6 :: t7 :: rest ->
      let left =
        map7 t1 t2 t3 t4 t5 t6 t7 ~f:(fun a1 a2 a3 a4 a5 a6 a7 ->
          [ a1; a2; a3; a4; a5; a6; a7 ])
      in
      let right = all rest in
      map2 left right ~f:(fun left right -> left @ right)
  ;;

  let reduce_balanced xs ~f =
    List.reduce_balanced xs ~f:(fun a b ->
      let%sub a = a in
      let%sub b = b in
      f a b)
  ;;

  let all_unit xs = all xs |> map ~f:(fun (_ : unit list) -> ())

  let all_map map_of_computations =
    map_of_computations
    |> Map.to_alist
    |> List.map ~f:(fun (key, data) -> map data ~f:(Tuple2.create key))
    |> all
    |> map ~f:(Map.of_alist_exn (Map.comparator_s map_of_computations))
  ;;

  module Let_syntax = struct
    let return = return

    include Applicative_infix

    module Let_syntax = struct
      let return = return
      let map = map
      let both = both

      include Mapn
    end
  end
end

module Value = Value

module Private = struct
  let conceal_value = Fn.id
  let reveal_value = Fn.id
  let conceal_computation = Fn.id
  let reveal_computation = Fn.id
end
