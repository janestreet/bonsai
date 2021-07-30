open! Core
open! Import

type ('i, 'm, 'a, 'r) unpacked =
  { input_var : 'i Incr.Var.t
  ; model_var : 'm Incr.Var.t
  ; clock : Incr.Clock.t
  ; inject : 'a -> unit Ui_effect.t
  ; sexp_of_model : 'm -> Sexp.t
  ; apply_action : (schedule_event:(unit Ui_effect.t -> unit) -> 'a -> 'm) Incr.Observer.t
  ; result : 'r Incr.Observer.t
  ; result_incr : 'r Incr.t
  ; lifecycle : Bonsai.Private.Lifecycle.Collection.t Incr.Observer.t
  ; queue : 'a Queue.t
  ; mutable should_replace_bonsai_path_string : bool
  ; mutable last_view : string
  ; mutable last_lifecycle : Bonsai.Private.Lifecycle.Collection.t
  }

type ('i, 'r) t = T : ('i, _, _, 'r) unpacked -> ('i, 'r) t

let create
      (type i r)
      ?initial_model_sexp
      ~clock
      ~(initial_input : i)
      (component : (i, r) Bonsai.Arrow_deprecated.t)
  : (i, r) t
  =
  let input_var = Incr.Var.create initial_input in
  let input = Incr.Var.watch input_var in
  let fresh = Type_equal.Id.create ~name:"fresh" sexp_of_opaque in
  let var = Bonsai.Private.(Value.named fresh |> conceal_value) in
  let computation = component var in
  let (Bonsai.Private.Computation.T
         { t = component_unpacked
         ; action
         ; model =
             { default = default_model
             ; sexp_of = sexp_of_model
             ; equal = _
             ; type_id = _
             ; of_sexp = model_of_sexp
             }
         })
    =
    computation |> Bonsai.Private.reveal_computation
  in
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
        (type a)
        (computation : (_, a, r) Bonsai.Private.Computation.t)
        (_action : a Type_equal.Id.t)
    : (i, r) t
    =
    let queue = Queue.create () in
    let module A =
      Ui_effect.Define (struct
        module Action = struct
          type t = a
        end

        let handle = Queue.enqueue queue
      end)
    in
    let inject = A.inject in
    let snapshot =
      Bonsai.Private.eval
        ~environment
        ~path:Bonsai.Private.Path.empty
        ~clock
        ~model:(Incr.Var.watch model_var)
        ~inject
        computation
    in
    let result_incr = Bonsai.Private.Snapshot.result snapshot in
    let apply_action = Bonsai.Private.Snapshot.apply_action snapshot |> Incr.observe in
    let result = result_incr |> Incr.observe in
    let lifecycle = Bonsai.Private.Snapshot.lifecycle_or_empty snapshot |> Incr.observe in
    Incr.stabilize ();
    T
      { input_var
      ; model_var
      ; clock
      ; inject
      ; apply_action
      ; result
      ; result_incr
      ; sexp_of_model
      ; lifecycle
      ; queue
      ; should_replace_bonsai_path_string = true
      ; last_view = ""
      ; last_lifecycle = Bonsai.Private.Lifecycle.Collection.empty
      }
  in
  create_polymorphic component_unpacked action
;;

let schedule_event _ = Ui_effect.Expert.handle

let flush (T { model_var; apply_action; queue; _ }) =
  let process_event action =
    let apply_action = Incr.Observer.value_exn apply_action in
    let new_model = apply_action action ~schedule_event:Ui_effect.Expert.handle in
    Incr.Var.set model_var new_model;
    (* We need to stabilize after every action so that [Snapshot.apply_action] is closed
       over the latest model. *)
    Incr.stabilize ()
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

let should_censor_bonsai_path (T { should_replace_bonsai_path_string; _ }) =
  should_replace_bonsai_path_string
;;

let disable_bonsai_path_censoring (T unpacked) =
  unpacked.should_replace_bonsai_path_string <- false
;;

let sexp_of_model (T { sexp_of_model; model_var; _ }) =
  sexp_of_model (Incr.Var.value model_var)
;;

let result_incr (T { result_incr; _ }) = result_incr
let clock (T { clock; _ }) = clock
