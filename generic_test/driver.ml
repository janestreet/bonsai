open! Core_kernel
include Driver_intf
module Bonsai_lib = Bonsai

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
    ; sexp_of_model : 'm -> Sexp.t
    ; snapshot :
        ('m, 'a, 'r, Event.t) Bonsai_lib.Generic.Expert.Snapshot.t Incr.Observer.t
    ; state : 'a Event_handler.t
    }

  type ('i, 'r) t = T : ('i, _, _, 'r) unpacked -> ('i, 'r) t

  let create
        (type i r)
        ?initial_model_sexp
        ~(initial_input : i)
        (component : (i, r) Bonsai.t)
    : (i, r) t
    =
    let input_var = Incr.Var.create initial_input in
    let (Bonsai_lib.Generic.Expert.T
           { unpacked = component_unpacked
           ; action_type_id
           ; model =
               { default = default_model
               ; sexp_of = sexp_of_model
               ; equal = _
               ; type_id = _
               ; of_sexp = model_of_sexp
               }
           })
      =
      component |> Bonsai.to_generic |> Bonsai_lib.Generic.Expert.reveal
    in
    let starting_model =
      Option.value_map initial_model_sexp ~default:default_model ~f:[%of_sexp: model]
    in
    let old_model_var = Incr.Var.create None in
    let model_var = Incr.Var.create starting_model in
    (* Sadly the only way to give a name to the existential type that we just introduced
       into the environment is by defining a function like this. See
       https://github.com/ocaml/ocaml/issues/7074. *)
    let create_polymorphic
          (type a)
          (component_unpacked : (i, _, a, r, _, _) Bonsai_lib.Generic.Expert.unpacked)
          (action_type_id : a Type_equal.Id.t)
      : (i, r) t
      =
      let state = Event_handler.make_state ~action_type_id in
      let inject action =
        Packed_action.T (action, action_type_id) |> Event_handler.inject state
      in
      let snapshot =
        Bonsai_lib.Generic.Expert.eval
          component_unpacked
          ~input:(Incr.Var.watch input_var)
          ~old_model:(Incr.Var.watch old_model_var)
          ~model:(Incr.Var.watch model_var)
          ~incr_state:Incr.State.t
          ~environment:Bonsai_types.Environment.empty
          ~inject
          ~action_type_id
        |> Incr.observe
      in
      Incr.stabilize ();
      T { input_var; model_var; inject; snapshot; old_model_var; state; sexp_of_model }
    in
    create_polymorphic component_unpacked action_type_id
  ;;

  let sexp_of_model (T { model_var; sexp_of_model; _ }) =
    model_var |> Incr.Var.value |> sexp_of_model
  ;;

  let flush (T { state; model_var; snapshot; old_model_var; _ }) =
    let previous_model = Incr.Var.value model_var in
    let process_event action =
      let apply_action =
        snapshot
        |> Incr.Observer.value_exn
        |> Bonsai_lib.Generic.Expert.Snapshot.apply_action
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
  let set_input (T t) = Incr.Var.set t.input_var

  let result (T t) =
    Incr.Observer.value_exn t.snapshot |> Bonsai_lib.Generic.Expert.Snapshot.result
  ;;
end
