open! Core_kernel
include Driver_intf

module Make
    (Bonsai : Bonsai.S)
    (Event_handler : Event_handler with module Event = Bonsai.Event) =
struct
  module Bonsai = Bonsai
  module Incr = Bonsai.Incr
  module Event = Bonsai.Event

  type ('i, 'm, 'a, 'r) unpacked =
    { input_var : 'i Incr.Var.t
    ; model_var : 'm Incr.Var.t
    ; old_model_var : 'm option Incr.Var.t
    ; inject : 'a -> Event.t
    ; snapshot : ('m, 'a, 'r) Bonsai.Expert.Snapshot.t Incr.Observer.t
    ; state : 'a Event_handler.t
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
    let (T (component_unpacked, action_type_id)) = Bonsai.Expert.reveal component in
    (* Sadly the only way to give a name to the existential type that we just introduced
       into the environment is by defining a function like this. See
       https://github.com/ocaml/ocaml/issues/7074. *)
    let create_polymorphic
          (type a)
          (component_unpacked : (i, m, a, r) Bonsai.Expert.unpacked)
          (action_type_id : a Type_equal.Id.t)
      : (i, m, r) t
      =
      let state = Event_handler.make_state ~action_type_id in
      let inject action =
        Packed_action.T (action, action_type_id) |> Event_handler.inject state
      in
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
      T { input_var; model_var; inject; snapshot; old_model_var; state }
    in
    create_polymorphic component_unpacked action_type_id
  ;;

  let flush (T { state; model_var; snapshot; old_model_var; _ }) =
    let previous_model = Incr.Var.value model_var in
    let process_event action =
      let apply_action =
        snapshot |> Incr.Observer.value_exn |> Bonsai.Expert.Snapshot.apply_action
      in
      let new_model =
        apply_action action ~schedule_event:(fun e ->
          Event_handler.schedule_event state e)
      in
      Incr.Var.set model_var new_model;
      (* We need to stabilize after every action so that [Snapshot.apply_action] is closed
         over the latest model. *)
      Incr.stabilize ()
    in
    Event_handler.iter_actions state ~f:process_event;
    Incr.Var.set old_model_var (Some previous_model);
    Incr.stabilize ()
  ;;

  let schedule_event (T t) = Event_handler.schedule_event t.state
  let model (T t) = Incr.Var.value t.model_var
  let set_model (T t) = Incr.Var.set t.model_var
  let set_input (T t) = Incr.Var.set t.input_var
  let result (T t) = Incr.Observer.value_exn t.snapshot |> Bonsai.Expert.Snapshot.result
end
