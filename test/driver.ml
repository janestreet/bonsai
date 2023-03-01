open! Core
open! Import

module Action = struct
  type ('dynamic_action, 'static_action) t =
    | Dynamic of 'dynamic_action
    | Static of 'static_action
end

type ('i, 'm, 'dynamic_action, 'static_action, 'action_input, 'r) unpacked =
  { input_var : 'i Incr.Var.t
  ; model_var : 'm Incr.Var.t
  ; default_model : 'm
  ; clock : Incr.Clock.t
  ; inject : ('dynamic_action, 'static_action) Action.t -> unit Ui_effect.t
  ; sexp_of_model : 'm -> Sexp.t
  ; action_input_incr : 'action_input Incr.t
  ; action_input : 'action_input Incr.Observer.t
  ; static_apply_action :
      schedule_event:(unit Ui_effect.t -> unit) -> 'm -> 'static_action -> 'm
  ; dynamic_apply_action :
      schedule_event:(unit Ui_effect.t -> unit)
      -> 'action_input option
      -> 'm
      -> 'dynamic_action
      -> 'm
  ; result : 'r Incr.Observer.t
  ; result_incr : 'r Incr.t
  ; lifecycle : Bonsai.Private.Lifecycle.Collection.t Incr.Observer.t
  ; lifecycle_incr : Bonsai.Private.Lifecycle.Collection.t Incr.t
  ; queue : ('dynamic_action, 'static_action) Action.t Queue.t
  ; mutable last_view : string
  ; mutable last_lifecycle : Bonsai.Private.Lifecycle.Collection.t
  }

type ('i, 'r) t = T : ('i, _, _, _, _, 'r) unpacked -> ('i, 'r) t

let assert_type_equalities
      (T a : _ Bonsai.Private.Computation.packed_info)
      (T b : _ Bonsai.Private.Computation.packed_info)
  =
  let T =
    Bonsai.Private.Meta.Model.Type_id.same_witness_exn a.model.type_id b.model.type_id
  in
  let T =
    Bonsai.Private.Meta.Action.Type_id.same_witness_exn a.dynamic_action b.dynamic_action
  in
  let T =
    Bonsai.Private.Meta.Action.Type_id.same_witness_exn a.static_action b.static_action
  in
  ()
;;

let create
      (type i r)
      ?initial_model_sexp
      ?(optimize = true)
      ~clock
      ~(initial_input : i)
      (component : (i, r) Bonsai.Arrow_deprecated.t)
  : (i, r) t
  =
  let input_var = Incr.Var.create initial_input in
  let input = Incr.Var.watch input_var in
  let fresh = Type_equal.Id.create ~name:"fresh" sexp_of_opaque in
  let var = Bonsai.Private.(Value.named App_input fresh |> conceal_value) in
  let computation = component var in
  let unoptimized_info =
    Bonsai.Private.gather (Bonsai.Private.reveal_computation computation)
  in
  let optimized_info =
    Bonsai.Private.reveal_computation computation
    |> (if optimize then Bonsai.Private.pre_process else Fn.id)
    |> Bonsai.Private.gather
  in
  let (T
         ({ model =
              { default = default_model
              ; sexp_of = sexp_of_model
              ; equal = _
              ; type_id = _
              ; of_sexp = model_of_sexp
              }
          ; input = _
          ; apply_static
          ; apply_dynamic
          ; dynamic_action = _
          ; static_action = _
          ; run = _
          ; reset = _
          } as computation_info))
    =
    optimized_info
  in
  assert_type_equalities unoptimized_info unoptimized_info;
  assert_type_equalities optimized_info optimized_info;
  let environment =
    Bonsai.Private.Environment.(empty |> add_exn ~key:fresh ~data:input)
  in
  let starting_model =
    Option.value_map initial_model_sexp ~default:default_model ~f:[%of_sexp: model]
  in
  let model_var = Incr.Var.create starting_model in
  (* Sadly the only way to give a name to the existential type that we just introduced
     into the environment is by defining a function like this. See
     https://github.com/ocaml/ocaml/issues/7074. *)
  let create_polymorphic
        (type dynamic_action static_action action_input)
        (computation_info :
           ( _
           , dynamic_action
           , static_action
           , action_input
           , r )
             Bonsai.Private.Computation.info)
        apply_static
        apply_dynamic
    : (i, r) t
    =
    let queue = Queue.create () in
    let module A =
      Ui_effect.Define (struct
        module Action = struct
          type t = (dynamic_action, static_action) Action.t
        end

        let handle = Queue.enqueue queue
      end)
    in
    let inject = A.inject in
    let inject_dynamic a = A.inject (Dynamic a) in
    let inject_static a = A.inject (Static a) in
    let snapshot =
      computation_info.run
        ~environment
        ~path:Bonsai.Private.Path.empty
        ~clock
        ~model:(Incr.Var.watch model_var)
        ~inject_dynamic
        ~inject_static
    in
    let result_incr = Bonsai.Private.Snapshot.result snapshot in
    let action_input_incr =
      Bonsai.Private.Input.to_incremental (Bonsai.Private.Snapshot.input snapshot)
    in
    let action_input = Incr.observe action_input_incr in
    let result = result_incr |> Incr.observe in
    let lifecycle_incr = Bonsai.Private.Snapshot.lifecycle_or_empty snapshot in
    let lifecycle = Incr.observe lifecycle_incr in
    Incr.stabilize ();
    T
      { input_var
      ; model_var
      ; default_model
      ; clock
      ; inject
      ; action_input
      ; action_input_incr
      ; static_apply_action = apply_static ~inject_dynamic ~inject_static
      ; dynamic_apply_action = apply_dynamic ~inject_dynamic ~inject_static
      ; result
      ; result_incr
      ; sexp_of_model
      ; lifecycle
      ; lifecycle_incr
      ; queue
      ; last_view = ""
      ; last_lifecycle = Bonsai.Private.Lifecycle.Collection.empty
      }
  in
  create_polymorphic computation_info apply_static apply_dynamic
;;

let schedule_event _ = Ui_effect.Expert.handle

let flush
      (T { model_var; static_apply_action; dynamic_apply_action; action_input; queue; _ })
  =
  let update_model ~action ~apply_action =
    (* The only difference between [Var.latest_value] and [Var.value] is that
       if [Var.set] is called _while stabilizing_, then calling [Var.value]
       will return the value that was set when stabilization started, whereas
       [latest_value] will give you the value that was just [set].  Now,
       setting a model in the middle of a stabilizaiton should never happen,
       but I think it's important to be explicit about which behavior we use,
       so I chose the one that would be least surprising if a stabilization
       does happen to occur. *)
    Incr.Var.set
      model_var
      (apply_action
         ~schedule_event:Ui_effect.Expert.handle
         (Incr.Var.latest_value model_var)
         action)
  in
  let process_event (action : _ Action.t) =
    match action with
    | Static action -> update_model ~apply_action:static_apply_action ~action
    | Dynamic action ->
      (* We need to stabilize before every action so that the [input] for the
         apply-actions are up to date. *)
      Incr.stabilize ();
      let action_input = Incr.Observer.value_exn action_input in
      let apply_action ~schedule_event model action =
        dynamic_apply_action ~schedule_event (Some action_input) model action
      in
      update_model ~action ~apply_action
  in
  while not (Queue.is_empty queue) do
    process_event (Queue.dequeue_exn queue)
  done;
  Incr.stabilize ()
;;

let set_input (T { input_var; _ }) input = Incr.Var.set input_var input
let input (T { input_var; _ }) = Incr.Var.value input_var
let result (T { result; _ }) = Incr.Observer.value_exn result
let last_view (T { last_view; _ }) = last_view
let store_view (T unpacked) s = unpacked.last_view <- s

let has_after_display_events (T t) =
  let lifecycle = t.lifecycle |> Incr.Observer.value_exn in
  Bonsai.Private.Lifecycle.Collection.has_after_display lifecycle
;;

let trigger_lifecycles (T t) =
  let old = t.last_lifecycle in
  let new_ = t.lifecycle |> Incr.Observer.value_exn in
  t.last_lifecycle <- new_;
  schedule_event () (Bonsai.Private.Lifecycle.Collection.diff old new_)
;;

let sexp_of_model (T { sexp_of_model; model_var; _ }) =
  sexp_of_model (Incr.Var.value model_var)
;;

let result_incr (T { result_incr; _ }) = result_incr
let action_input_incr (T { action_input_incr; _ }) = Ui_incr.pack action_input_incr
let lifecycle_incr (T { lifecycle_incr; _ }) = Ui_incr.pack lifecycle_incr
let clock (T { clock; _ }) = clock

let invalidate_observers (T { action_input; result; lifecycle; _ }) =
  Incr.Observer.disallow_future_use action_input;
  Incr.Observer.disallow_future_use result;
  Incr.Observer.disallow_future_use lifecycle
;;

let reset_model_to_default (T { model_var; default_model; _ }) =
  Incr.Var.set model_var default_model
;;
