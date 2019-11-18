open! Core_kernel
open! Import

type ('i, 'm, 'a, 'r) unpacked =
  { input_var : 'i Incr.Var.t
  ; model_var : 'm Incr.Var.t
  ; old_model_var : 'm option Incr.Var.t
  ; inject : 'a -> Event.t
  ; snapshot : ('m, 'a, 'r) Bonsai.Expert.Snapshot.t Incr.Observer.t
  ; event_queue : Event.t Queue.t
  ; action_type_id : 'a Type_equal.Id.t
  }

type ('i, 'm, 'r) t = T : ('i, 'm, _, 'r) unpacked -> ('i, 'm, 'r) t

let create
      (type i m r)
      ~(initial_input : i)
      ~(initial_model : m)
      (component : (i, m, r) Bonsai.t)
  : (i, m, r) t
  =
  let input_var = Incr.Var.create initial_input in
  let model_var = Incr.Var.create initial_model in
  let old_model_var = Incr.Var.create None in
  let event_queue = Queue.create () in
  let (T (component_unpacked, action_type_id)) = Bonsai.Expert.reveal component in
  let inject = Event.pack action_type_id in
  let snapshot =
    Bonsai.Expert.eval
      component_unpacked
      ~input:(Incr.Var.watch input_var)
      ~old_model:(Incr.Var.watch old_model_var)
      ~model:(Incr.Var.watch model_var)
      ~inject
      ~action_type_id
    |> Incr.observe
  in
  Incr.stabilize ();
  T
    { input_var
    ; model_var
    ; inject
    ; snapshot
    ; event_queue
    ; old_model_var
    ; action_type_id
    }
;;

let model (T t) = Incr.Var.value t.model_var
let set_model (T t) = Incr.Var.set t.model_var
let set_input (T t) = Incr.Var.set t.input_var
let schedule_event (T { event_queue; _ }) event = Queue.enqueue event_queue event
let result (T t) = Incr.Observer.value_exn t.snapshot |> Bonsai.Expert.Snapshot.result

let flush (T { event_queue; model_var; snapshot; action_type_id; old_model_var; _ }) =
  let previous_model = Incr.Var.value model_var in
  while not (Queue.is_empty event_queue) do
    match Queue.dequeue_exn event_queue with
    | External_event s -> printf "External event: %s\n" s
    | Packed (action, type_id) ->
      let action =
        Type_equal.conv (Type_equal.Id.same_witness_exn type_id action_type_id) action
      in
      let apply_action =
        snapshot |> Incr.Observer.value_exn |> Bonsai.Expert.Snapshot.apply_action
      in
      let new_model = apply_action action ~schedule_event:(Queue.enqueue event_queue) in
      Incr.Var.set model_var new_model;
      (* We need to stabilize after every action so that [Snapshot.apply_action] is closed
         over the latest model. *)
      Incr.stabilize ()
  done;
  Incr.Var.set old_model_var (Some previous_model);
  Incr.stabilize ()
;;
