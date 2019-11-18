open! Core_kernel
open! Import
open Composition_infix
include Component_intf

module Make (Incr : Incremental.S) (Event : T) :
  S with module Incr := Incr with module Event := Event = struct
  module Incr_map = Incr_map.Make (Incr)
  module Snapshot = Snapshot.Make (Event)
  open Incr.Let_syntax

  type 'custom mismatch_behavior =
    [ `Ignore
    | `Raise
    | `Warn
    | `Custom of 'custom
    ]

  type ('extra, 'new_model) on_action_mismatch = ('extra -> 'new_model) mismatch_behavior

  module Full = struct
    type ('input, 'model, 'action, 'result) t =
      input:'input Incr.t
      -> old_model:'model option Incr.t
      -> model:'model Incr.t
      -> inject:('action -> Event.t)
      -> ('model, 'action, 'result) Snapshot.t Incr.t
  end

  module Projection = struct
    module Model = struct
      type ('m_up, 'm_down) t =
        { unlift : 'm_up -> 'm_down
        ; lift : 'm_up -> 'm_down -> 'm_up
        }
    end

    type ('m_up, 'm_down) t = { model : ('m_up, 'm_down) Model.t }
  end

  module Module_component = struct
    module type S = sig
      module Input : T
      module Model : T

      module Action : sig
        type t [@@deriving sexp_of]
      end

      module Result : T

      val apply_action
        :  inject:(Action.t -> Event.t)
        -> schedule_event:(Event.t -> unit)
        -> Input.t
        -> Model.t
        -> Action.t
        -> Model.t

      val compute : inject:(Action.t -> Event.t) -> Input.t -> Model.t -> Result.t
      val name : string
    end

    type ('input, 'model, 'action, 'result) component_s =
      (module S
        with type Input.t = 'input
         and type Model.t = 'model
         and type Action.t = 'action
         and type Result.t = 'result)

    module type S_incremental = sig
      module Input : T
      module Model : T

      module Action : sig
        type t [@@deriving sexp_of]
      end

      module Result : T

      val apply_action
        :  Input.t Incr.t
        -> Model.t Incr.t
        -> inject:(Action.t -> Event.t)
        -> (schedule_event:(Event.t -> unit) -> Action.t -> Model.t) Incr.t

      val compute
        :  Input.t Incr.t
        -> Model.t Incr.t
        -> inject:(Action.t -> Event.t)
        -> Result.t Incr.t

      val name : string
    end

    type ('input, 'model, 'action, 'result) component_s_incremental =
      (module S_incremental
        with type Input.t = 'input
         and type Model.t = 'model
         and type Action.t = 'action
         and type Result.t = 'result)
  end

  module Assoc = struct
    type ('data, 'action, 'result, 'k, 'cmp, 'r_by_k, 'd_by_k) t =
      { r_by_k : ('r_by_k, ('k, 'result, 'cmp) Map.t) Type_equal.t
      ; d_by_k : ('d_by_k, ('k, 'data, 'cmp) Map.t) Type_equal.t
      }
  end

  module Switch = struct
    module Case = struct
      type ('inner_input, 'inner_model, 'inner_action, 'outer_model, 'result) unpacked =
        { input : 'inner_input Incr.t
        ; model : 'inner_model Incr.t
        ; compute :
            'inner_input Incr.t
            -> 'inner_model Incr.t
            -> ('inner_model, 'inner_action, 'result) Snapshot.t Incr.t
        ; lift : 'inner_model -> 'outer_model
        }

      type ('i, 'm, 'r) t =
        | T : (_, _, 'a, 'm, 'r) unpacked * 'a Type_equal.Id.t -> ('i, 'm, 'r) t
    end

    module Case_action = struct
      type t =
        | T :
            { action : 'a
            ; type_id : 'a Type_equal.Id.t
            }
            -> t

      let sexp_of_t (T { action; type_id }) =
        let sexp_of_action = Type_equal.Id.to_sexp type_id in
        [%message "Bonsai.Switch.Case_action" (action : action)]
      ;;

      let type_id : t Type_equal.Id.t =
        Type_equal.Id.create ~name:(Source_code_position.to_string [%here]) [%sexp_of: t]
      ;;
    end
  end

  module Computation_types (T : sig
      type (_, _, _, _) t
    end) =
  struct
    open T

    type ('input, 'model, 'action, 'result) eval_type =
      input:'input Incr.t
      -> old_model:'model option Incr.t
      -> model:'model Incr.t
      -> inject:('action -> Event.t)
      -> action_type_id:'action Type_equal.Id.t
      -> ('input, 'model, 'action, 'result) t
      -> ('model, 'action, 'result) Snapshot.t Incr.t

    type ('input, 'model, 'action, 'result) optimize_type =
      ('input, 'model, 'action, 'result) t -> ('input, 'model, 'action, 'result) t

    type ('input, 'm, 'a1, 'a2, 'result, 'r1, 'r2) both_compose_type =
      eval1:('input, 'm, 'a1, 'r1) eval_type
      -> eval2:('input, 'm, 'a2, 'r2) eval_type
      -> input:'input Incr.t
      -> old_model:'m option Incr.t
      -> model:'m Incr.t
      -> inject:(('a1, 'a2) Either.t -> Event.t)
      -> f:('r1 -> 'r2 -> 'result)
      -> action_type_id1:'a1 Type_equal.Id.t
      -> action_type_id2:'a2 Type_equal.Id.t
      -> ('input, 'm, 'a1, 'r1) t
      -> ('input, 'm, 'a2, 'r2) t
      -> ('m, ('a1, 'a2) Either.t, 'result) Snapshot.t Incr.t
  end

  include Module_component

  type ('input, 'model, 'action, 'result) unpacked =
    (* Non-incremental Constructors *)
    | Constant : 'result -> (_, _, Nothing.t, 'result) unpacked
    | Pure : ('input -> 'result) -> ('input, _, Nothing.t, 'result) unpacked
    | With_readonly_model :
        ('input * 'model, unit, 'action, 'result) unpacked
        -> ('input, 'model, 'action, 'result) unpacked
    | Projection :
        ('input, 'm1, 'a, 'result) unpacked * ('m2, 'm1) Projection.t
        -> ('input, 'm2, 'a, 'result) unpacked
    | Module :
        ('input, 'model, 'action, 'result) component_s
        -> ('input, 'model, 'action, 'result) unpacked
    (* Incremental Constructors *)
    | Map_incr :
        ('input, 'm, 'a, 'r1) unpacked * ('r1 Incr.t -> 'r2 Incr.t)
        -> ('input, 'm, 'a, 'r2) unpacked
    | Switch :
        ((Switch.Case_action.t -> Event.t)
         -> 'input Incr.t
         -> 'model Incr.t
         -> ('input, 'model, 'result) Switch.Case.t Incr.t)
        -> ('input, 'model, Switch.Case_action.t, 'result) unpacked
    | Pure_incr :
        ('input Incr.t -> 'result Incr.t)
        -> ('input, _, Nothing.t, 'result) unpacked
    | Module_incr :
        ('input, 'model, 'action, 'result) component_s_incremental
        -> ('input, 'model, 'action, 'result) unpacked
    (* Composition and Combinators *)
    | Compose :
        ('i1, 'm, 'a1, 'r1) unpacked
        * 'a1 Type_equal.Id.t
        * ('r1, 'm, 'a2, 'r2) unpacked
        * 'a2 Type_equal.Id.t
        -> ('i1, 'm, ('a1, 'a2) Either.t, 'r2) unpacked
    | Map :
        ('input, 'm, 'a, 'r1) unpacked * ('r1 -> 'r2)
        -> ('input, 'm, 'a, 'r2) unpacked
    | Both :
        ('input, 'model, 'a1, 'r1) unpacked
        * 'a1 Type_equal.Id.t
        * ('input, 'model, 'a2, 'r2) unpacked
        * 'a2 Type_equal.Id.t
        * ('r1 -> 'r2 -> 'result)
        -> ('input, 'model, ('a1, 'a2) Either.t, 'result) unpacked
    | Option :
        ('input, 'm, 'a, 'r) unpacked * (unit, 'm option) on_action_mismatch
        -> ('input, 'm option, 'a, 'r option) unpacked
    | Either :
        ('input, 'm1, 'a1, 'r1) unpacked
        * 'a1 Type_equal.Id.t
        * ('input, 'm2, 'a2, 'r2) unpacked
        * 'a2 Type_equal.Id.t
        * ( [ `Action_for_first of 'm2 | `Action_for_second of 'm1 ]
          , ('m1, 'm2) Either.t )
            on_action_mismatch
        -> ( 'input
           , ('m1, 'm2) Either.t
           , ('a1, 'a2) Either.t
           , ('r1, 'r2) Either.t )
             unpacked
    | Assoc_by_model :
        (* We need the Type_equal witnesses here because the typechecker's rules aren't
           powerful enough to just have the Comparator.t here. *)
        ('k * 'input, 'model, 'action, 'result) unpacked
        * 'action Type_equal.Id.t
        * ('model, 'action, 'result, 'k, 'cmp, 'r_by_k, 'm_by_k) Assoc.t
        -> ('input, 'm_by_k, 'k * 'action, 'r_by_k) unpacked
    | Assoc_by_input :
        (* We need the Type_equal witnesses here because the typechecker's rules aren't
           powerful enough to just have the Comparator.t here. *)
        ('k * 'input, 'model, 'action, 'result) unpacked
        * 'action Type_equal.Id.t
        * ('input, 'action, 'result, 'k, 'cmp, 'r_by_k, 'i_by_k) Assoc.t
        -> ('i_by_k, 'model, 'k * 'action, 'r_by_k) unpacked
    (* For experts *)
    | Full :
        ('input, 'model, 'action, 'result) Full.t
        -> ('input, 'model, 'action, 'result) unpacked

  (* Needs to be a module so we can [include] it later in [Expert].  Otherwise, the
     compiler complains that the types (impl vs intf) are of different kinds. *)
  module T = struct
    type ('input, 'model, 'result) t =
      | T :
          ('input, 'model, 'action, 'result) unpacked * 'action Type_equal.Id.t
          -> ('input, 'model, 'result) t
  end

  include T

  module C = Computation_types (struct
      type nonrec ('i, 'm, 'a, 'r) t = ('i, 'm, 'a, 'r) unpacked
    end)

  open C

  module Both = struct
    let compose : (_, _, _, _, _, _, _) both_compose_type =
      fun ~eval1
        ~eval2
        ~input
        ~old_model
        ~model
        ~inject
        ~f
        ~action_type_id1
        ~action_type_id2
        a
        b ->
        let inject_a e = inject (First e) in
        let inject_b e = inject (Second e) in
        let%map a =
          eval1 ~input ~old_model ~model ~inject:inject_a ~action_type_id:action_type_id1 a
        and b =
          eval2 ~input ~old_model ~model ~inject:inject_b ~action_type_id:action_type_id2 b
        in
        let apply_action ~schedule_event action =
          match action with
          | First a_action -> Snapshot.apply_action a a_action ~schedule_event
          | Second b_action -> Snapshot.apply_action b b_action ~schedule_event
        in
        let result = f (Snapshot.result a) (Snapshot.result b) in
        Snapshot.create ~result ~apply_action
    ;;
  end

  let apply_nothing_action ~schedule_event:_ a = Nothing.unreachable_code a

  let rec eval : type i m a r. (i, m, a, r) eval_type =
    fun ~input ~old_model ~model ~inject ~action_type_id ->
    let sexp_of_action = Type_equal.Id.to_sexp action_type_id in
    function
    | Constant result ->
      Incr.const (Snapshot.create ~result ~apply_action:apply_nothing_action)
    | Pure f ->
      let%map input = input in
      Snapshot.create ~result:(f input) ~apply_action:apply_nothing_action
    | With_readonly_model t ->
      let old_model = old_model >>| Option.map ~f:(Fn.const ()) in
      let%map snapshot =
        eval
          t
          ~input:(Incr.both input model)
          ~model:(Incr.const ())
          ~old_model
          ~inject
          ~action_type_id
      and model = model in
      let result = Snapshot.result snapshot in
      let apply_action ~schedule_event:_ _action = model in
      Snapshot.create ~result ~apply_action
    | Pure_incr f ->
      let%map result = f input in
      Snapshot.create ~result ~apply_action:apply_nothing_action
    | Full f -> f ~input ~old_model ~model ~inject
    | Switch f ->
      let open Incr.Let_syntax in
      let%bind (T ({ input; model; compute; lift }, type_id)) = f inject input model in
      let%map snapshot = compute input model
      and model = model in
      let result = Snapshot.result snapshot in
      let apply_action
            ~schedule_event
            (T { action; type_id = type_id' } : Switch.Case_action.t)
        =
        match Type_equal.Id.same_witness type_id type_id' with
        | Some T ->
          let apply_action = Snapshot.apply_action snapshot in
          lift (apply_action ~schedule_event action)
        | None ->
          (* Drop the mismatched action. *)
          lift model
      in
      Snapshot.create ~result ~apply_action
    | Projection (t, { model = model_projection }) ->
      let model_downwards = Incr.map model ~f:model_projection.unlift in
      let old_model_downwards =
        Incr.map old_model ~f:(Option.map ~f:model_projection.unlift)
      in
      let%map snapshot =
        eval
          t
          ~input
          ~old_model:old_model_downwards
          ~model:model_downwards
          ~inject
          ~action_type_id
      and model = model in
      let result = Snapshot.result snapshot in
      let apply_action ~schedule_event a =
        let inner_model = Snapshot.apply_action snapshot ~schedule_event a in
        model_projection.lift model inner_model
      in
      Snapshot.create ~result ~apply_action
    | Compose (a, a_id, b, b_id) ->
      let inject_a e = inject (First e) in
      let inject_b e = inject (Second e) in
      let a = eval a ~input ~old_model ~model ~inject:inject_a ~action_type_id:a_id in
      let b =
        eval
          b
          ~input:(a >>| Snapshot.result)
          ~old_model
          ~model
          ~inject:inject_b
          ~action_type_id:b_id
      in
      let%map a_apply_action = a >>| Snapshot.apply_action
      and b_apply_action = b >>| Snapshot.apply_action
      and b_result = b >>| Snapshot.result in
      let apply_action ~schedule_event action =
        match action with
        | First a_action -> a_apply_action a_action ~schedule_event
        | Second b_action -> b_apply_action b_action ~schedule_event
      in
      Snapshot.create ~result:b_result ~apply_action
    | Map (t, f) ->
      let%map snapshot = eval ~input ~old_model ~model ~inject ~action_type_id t in
      Snapshot.create
        ~result:(f (Snapshot.result snapshot))
        ~apply_action:(Snapshot.apply_action snapshot)
    | Map_incr (t, f) ->
      let snapshot = eval ~input ~old_model ~model ~inject ~action_type_id t in
      let result = snapshot >>| Snapshot.result |> f in
      let apply_action = snapshot >>| Snapshot.apply_action in
      let%map result = result
      and apply_action = apply_action in
      Snapshot.create ~result ~apply_action
    | Module m ->
      let module M = (val m) in
      let%map input = input
      and model = model in
      let result = M.compute input model ~inject in
      let apply_action = M.apply_action input model ~inject in
      Snapshot.create ~result ~apply_action
    | Module_incr m ->
      let module M = (val m) in
      let%map apply_action = M.apply_action input model ~inject
      and result = M.compute input model ~inject in
      Snapshot.create ~result ~apply_action
    | Both (a, action_type_id1, b, action_type_id2, f) ->
      Both.compose
        ~eval1:eval
        ~eval2:eval
        ~input
        ~old_model
        ~model
        ~inject
        ~f
        ~action_type_id1
        ~action_type_id2
        a
        b
    | Option (t, on_action_for_none) ->
      let none_old_model = Incr.const None in
      let error_message action =
        [%message
          "Option component received an action while its model was None"
            (action : action)]
      in
      (match%pattern_bind model with
       | None ->
         let apply_action =
           match on_action_for_none with
           | `Ignore -> fun ~schedule_event:_ _ -> None
           | `Warn ->
             fun ~schedule_event:_ action ->
               eprint_s (error_message action);
               None
           | `Raise -> fun ~schedule_event:_ action -> raise_s (error_message action)
           | `Custom handler -> fun ~schedule_event:_ _ -> handler ()
         in
         return (Snapshot.create ~result:None ~apply_action)
       | Some model ->
         let%map snapshot =
           match%pattern_bind old_model with
           | Some old_model -> eval ~input ~old_model ~model ~inject ~action_type_id t
           | None ->
             eval ~input ~old_model:none_old_model ~model ~inject ~action_type_id t
         in
         let result = Some (Snapshot.result snapshot) in
         let apply_action ~schedule_event a =
           Some (Snapshot.apply_action snapshot ~schedule_event a)
         in
         Snapshot.create ~result ~apply_action)
    | Either (t1, action_type_id1, t2, action_type_id2, on_action_for_other_component) ->
      (* I couldn't figure out how to pull each case out into another function even
         though they are similar. *)
      let outer_model = model in
      (match%pattern_bind outer_model with
       | First model ->
         let old_model =
           match%pattern_map old_model with
           | Some (First om) -> Some om
           | Some (Second _) | None -> None
         in
         let inject a = inject (First a) in
         let%map snapshot =
           eval ~input ~model ~old_model ~inject ~action_type_id:action_type_id1 t1
         and outer_model = outer_model
         and model = model in
         let result = First (Snapshot.result snapshot) in
         let apply_action ~schedule_event action =
           let error_message action =
             [%message
               "Either component is in its First state, but got an action intended for \
                the Second state"
                 (action : action)]
           in
           match action, on_action_for_other_component with
           | First a, _ -> First (Snapshot.apply_action snapshot ~schedule_event a)
           | Second _, `Ignore -> outer_model
           | Second _, `Warn ->
             eprint_s (error_message action);
             outer_model
           | Second _, `Raise -> raise_s (error_message action)
           | Second _, `Custom handler -> handler (`Action_for_second model)
         in
         Snapshot.create ~result ~apply_action
       | Second model ->
         let old_model =
           match%pattern_map old_model with
           | Some (Second om) -> Some om
           | Some (First _) | None -> None
         in
         let inject a = inject (Second a) in
         let%map snapshot =
           eval ~input ~model ~old_model ~inject ~action_type_id:action_type_id2 t2
         and outer_model = outer_model
         and model = model in
         let result = Second (Snapshot.result snapshot) in
         let apply_action ~schedule_event action =
           let error_message action =
             [%message
               "Either component is in its Second state, but got an action intended for \
                the First state"
                 (action : action)]
           in
           match action, on_action_for_other_component with
           | Second a, _ -> Second (Snapshot.apply_action snapshot ~schedule_event a)
           | First _, `Ignore -> outer_model
           | First _, `Warn ->
             eprint_s (error_message action);
             outer_model
           | First _, `Raise -> raise_s (error_message action)
           | First _, `Custom handler -> handler (`Action_for_first model)
         in
         Snapshot.create ~result ~apply_action)
    | Assoc_by_model (t, action_type_id, { r_by_k = T; d_by_k = T }) ->
      (* I couldn't figure out how to pull this out into another function *)
      let%bind comparator = model >>| Map.comparator_s in
      let (module Current_comparator) = comparator in
      (* While incremental binds are avoided as much as possible, in this case,
         because the comparator for a map is stable for the type, we can bind
         here and incur no real performance penalty.  This pattern is pervasive
         throughout the Incr_map library. *)
      let old_model =
        match%map old_model with
        | Some m -> m
        | None -> Map.empty comparator
      in
      let model_and_old_model_map =
        Incr_map.merge model old_model ~f:(fun ~key:_ ->
          function
          | `Left model -> Some (model, None)
          | `Right _ -> None
          | `Both (model, old_model) -> Some (model, Some old_model))
      in
      let snapshot_map =
        Incr_map.mapi' model_and_old_model_map ~f:(fun ~key ~data:model_and_old_model ->
          let input =
            let%map input = input in
            key, input
          in
          let%pattern_bind model, old_model = model_and_old_model in
          let inject action = inject (key, action) in
          eval ~input ~model ~old_model ~inject ~action_type_id t)
      in
      let results_map =
        Incr_map.mapi snapshot_map ~f:(fun ~key:_ ~data:snapshot ->
          Snapshot.result snapshot)
      in
      let apply_action =
        let%map action_map =
          Incr_map.mapi snapshot_map ~f:(fun ~key:_ ~data:snapshot ->
            Snapshot.apply_action snapshot)
        and model = model in
        fun ~schedule_event action ->
          let id, action = action in
          match Map.find action_map id with
          | None -> model
          (* drop it on the floor *)
          | Some apply_action ->
            let data = apply_action ~schedule_event action in
            Map.set model ~key:id ~data
      in
      let%map apply_action = apply_action
      and result = results_map in
      Snapshot.create ~result ~apply_action
    | Assoc_by_input (t, action_type_id, { r_by_k = T; d_by_k = T }) ->
      (* I couldn't figure out how to pull this out into another function *)
      let%bind comparator = input >>| Map.comparator_s in
      let (module Current_comparator) = comparator in
      let snapshot_map =
        Incr_map.mapi' input ~f:(fun ~key ~data:input ->
          let input =
            let%map input = input in
            key, input
          in
          let inject action = inject (key, action) in
          eval ~input ~model ~old_model ~inject ~action_type_id t)
      in
      let results_map =
        Incr_map.mapi snapshot_map ~f:(fun ~key:_ ~data:snapshot ->
          Snapshot.result snapshot)
      in
      let apply_action =
        let%map action_map =
          Incr_map.mapi snapshot_map ~f:(fun ~key:_ ~data:snapshot ->
            Snapshot.apply_action snapshot)
        and model = model in
        fun ~schedule_event action ->
          let id, action = action in
          match Map.find action_map id with
          | None -> model
          (* drop it on the floor *)
          | Some apply_action -> apply_action ~schedule_event action
      in
      let%map apply_action = apply_action
      and result = results_map in
      Snapshot.create ~result ~apply_action

  and optimize : type i m a r. (i, m, a, r) optimize_type = function
    | Map (c, f) ->
      (match optimize c with
       | Constant r -> Constant (f r)
       | Map (c, g) -> Map (c, f << g)
       | Pure f_inner -> Pure (f << f_inner)
       | Both (l, ati1, r, ati2, f') ->
         let g a b = f (f' a b) in
         Both (l, ati1, r, ati2, g)
       | other -> Map (other, f))
    | Projection (c1, p1) ->
      (match optimize c1 with
       | Projection (c2, p2) ->
         let a =
           { Projection.model =
               { lift =
                   (fun m_up m_down ->
                      let m_mid = p1.model.unlift m_up in
                      p1.model.lift m_up (p2.model.lift m_mid m_down))
               ; unlift = (fun m -> p2.model.unlift (p1.model.unlift m))
               }
           }
         in
         Projection (c2, a)
       | other -> Projection (other, p1))
    | Both (l, ati1, r, ati2, f) ->
      let l = optimize l in
      let r = optimize r in
      Both (l, ati1, r, ati2, f)
    | Assoc_by_model (t, ati, cmp) -> Assoc_by_model (optimize t, ati, cmp)
    | other -> other
  ;;

  let nothing_type_id =
    Type_equal.Id.create
      ~name:(Source_code_position.to_string [%here])
      [%sexp_of: Nothing.t]
  ;;

  (* Constructor Functions *)

  let const r = T (Constant r, nothing_type_id)
  let pure ~f = T (Pure f, nothing_type_id)

  (* Modifier Functions *)

  let of_module (type i m a r) m =
    let module M = (val m : S
                    with type Input.t = i
                     and type Action.t = a
                     and type Model.t = m
                     and type Result.t = r)
    in
    T (Module m, Type_equal.Id.create ~name:M.name [%sexp_of: M.Action.t])
  ;;

  let compose (T (at, ati)) (T (bt, bti)) =
    T
      ( Compose (at, ati, bt, bti)
      , Type_equal.Id.create
          ~name:(Source_code_position.to_string [%here])
          (Either.sexp_of_t (Type_equal.Id.to_sexp ati) (Type_equal.Id.to_sexp bti)) )
  ;;

  let map (T (t, ati)) ~f = T (Map (t, f), ati)

  let map2 (T (a, ati)) (T (b, bti)) ~f =
    T
      ( Both (a, ati, b, bti, f)
        ,
        Type_equal.Id.create
          ~name:(Source_code_position.to_string [%here])
          (Either.sexp_of_t (Type_equal.Id.to_sexp ati) (Type_equal.Id.to_sexp bti)) )
  ;;

  let map_input t ~f = compose (pure ~f) t

  include struct
    open Applicative.Make3_using_map2 (struct
        type nonrec ('r, 'i, 'm) t = ('i, 'm, 'r) t

        let map = `Custom map
        let map2 = map2
        let return = const
      end)

    module Applicative_infix = Applicative_infix

    let all = all
    let all_unit = all_unit
    let apply = apply
    let both = both
    let map3 = map3
    let return = return
    let ( <* ) = ( <* )
    let ( *> ) = ( *> )
    let ( <*> ) = ( <*> )
    let ( >>| ) = ( >>| )
  end

  module Infix = struct
    let ( >>> ) = compose
    let ( >>| ) = ( >>| )
    let ( @>> ) f t = map_input t ~f
  end

  open Infix

  module M (Component : S) = struct
    type nonrec t = (Component.Input.t, Component.Model.t, Component.Result.t) t
  end

  module Project = struct
    module Model = struct
      let f (T (t, ati)) ~unlift ~lift =
        T (Projection (t, Projection.{ model = { unlift; lift } }), ati)
      ;;

      let field field t = f t ~unlift:(Field.get field) ~lift:(Field.fset field)
      let ignore = f ~unlift:(fun _ -> ()) ~lift:(fun m () -> m)
      let to_input_with_other (T (a, type_id)) = T (With_readonly_model a, type_id)
      let to_input t = to_input_with_other (snd @>> t)
    end
  end

  let pure_from_model ~f = compose (const ()) (pure ~f |> Project.Model.to_input)

  module List_deprecated = struct end

  module Incremental = struct
    let pure ~f = T (Pure_incr f, nothing_type_id)
    let of_incr t = pure ~f:(Fn.const t)
    let pure_from_model ~f = compose (const ()) (pure ~f |> Project.Model.to_input)
    let map (T (c, ati)) ~f = T (Map_incr (c, f), ati)
    let map_input t ~f = compose (pure ~f) t

    module type S = Module_component.S_incremental

    type ('input, 'model, 'action, 'result) component_s =
      (module S
        with type Input.t = 'input
         and type Model.t = 'model
         and type Action.t = 'action
         and type Result.t = 'result)

    let of_module (type i m a r) m =
      let module M = (val m : S
                      with type Input.t = i
                       and type Action.t = a
                       and type Model.t = m
                       and type Result.t = r)
      in
      T (Module_incr m, Type_equal.Id.create ~name:M.name [%sexp_of: M.Action.t])
    ;;

    module Case = struct
      type ('outer_input, 'outer_model, 'result) case_creator =
        { create_case :
            'inner_input 'inner_model. ('inner_input, 'inner_model, 'result) t
            -> case_input:'inner_input Incr.t -> case_model:'inner_model Incr.t
            -> lift:('inner_model -> 'outer_model)
            -> ('outer_input, 'outer_model, 'result) Switch.Case.t Incr.t
        }

      include Switch.Case

      type ('i, 'a, 'b) case = ('i, 'a, 'b) t
    end

    let switch ~f =
      let g inject =
        let create_case (T (component, action_type_id)) ~case_input ~case_model ~lift =
          let inject action =
            inject (Switch.Case_action.T { action; type_id = action_type_id })
          in
          let compute input model =
            eval
              ~input
              ~model
              ~old_model:(Incr.return None)
              ~inject
              ~action_type_id
              component
          in
          Switch.Case.T
            ({ input = case_input; model = case_model; lift; compute }, action_type_id)
          |> Incr.return
        in
        f { Case.create_case }
      in
      T (Switch g, Switch.Case_action.type_id)
    ;;
  end

  module Option = struct
    let default_on_action_for_none = `Ignore

    let wrap_model ?(on_action_for_none = default_on_action_for_none) (T (t, ati)) =
      T (Option (t, on_action_for_none), ati)
    ;;

    let wrap_model_with_default ?on_action_for_none t ~default =
      let open Incr.Let_syntax in
      wrap_model ?on_action_for_none t
      |> Incremental.map ~f:(fun o ->
        match%map o with
        | Some v -> v
        | None -> default)
    ;;
  end

  module Either = struct
    let default_on_action_for_other_component = `Ignore

    let wrap_model
          ?(on_action_for_other_component = default_on_action_for_other_component)
          (T (a, ati))
          (T (b, bti))
      =
      T
        ( Either (a, ati, b, bti, on_action_for_other_component)
        , Type_equal.Id.create
            ~name:(Source_code_position.to_string [%here])
            (Either.sexp_of_t (Type_equal.Id.to_sexp ati) (Type_equal.Id.to_sexp bti)) )
    ;;

    let wrap_model_with_same_result ?on_action_for_other_component a b =
      let open Incr.Let_syntax in
      wrap_model ?on_action_for_other_component a b
      |> Incremental.map ~f:(fun o ->
        match%map o with
        | First v -> v
        | Second v -> v)
    ;;
  end

  module Map = struct
    let associ_model
          (type k cmp)
          ?(comparator : (k, cmp) Map.comparator option)
          (T (t, ati))
      =
      let sexp_of_key =
        match comparator with
        | Some (module Comparator) -> Comparator.comparator.sexp_of_t
        | None -> sexp_of_opaque
      in
      T
        ( Assoc_by_model (t, ati, { r_by_k = T; d_by_k = T })
        , Type_equal.Id.create
            ~name:(Source_code_position.to_string [%here])
            (sexp_of_pair sexp_of_key (Type_equal.Id.to_sexp ati)) )
    ;;

    let assoc_model ?comparator t = associ_model ?comparator (snd @>> t)

    let associ_input
          (type k cmp)
          ?(comparator : (k, cmp) Map.comparator option)
          (T (t, ati))
      =
      let sexp_of_key =
        match comparator with
        | Some (module Comparator) -> Comparator.comparator.sexp_of_t
        | None -> sexp_of_opaque
      in
      T
        ( Assoc_by_input (t, ati, { r_by_k = T; d_by_k = T })
        , Type_equal.Id.create
            ~name:(Source_code_position.to_string [%here])
            (sexp_of_pair sexp_of_key (Type_equal.Id.to_sexp ati)) )
    ;;

    let assoc_input ?comparator t = associ_input ?comparator (snd @>> t)

    let merge a b ~f =
      let open Incr.Let_syntax in
      both a b
      |> Incremental.map ~f:(fun pair ->
        let%pattern_bind a, b = pair in
        Incr_map.merge a b ~f)
    ;;
  end

  module Open_on_rhs_intf = struct
    module type S = sig end
  end

  module Let_syntax = struct
    let return = return

    include Infix

    module Let_syntax = struct
      let return = return
      let both = both
      let map = map

      module Open_on_rhs = struct end
    end
  end

  module Expert = struct
    module Snapshot = Snapshot

    type nonrec ('input, 'model, 'action, 'result) unpacked =
      ('input, 'model, 'action, 'result) unpacked

    include T

    let reveal = Fn.id
    let conceal = Fn.id
    let of_full ~f ~action_type_id = T (Full f, action_type_id)
    let eval = eval
    let optimize (T (t, ati)) = T (optimize t, ati)
  end
end
