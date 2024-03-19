open! Core
open Js_of_ocaml
open Bonsai_introspection_protocol

let get_id = Effect.of_thunk Rpc_id.create
let queue : Event.t Queue.t = Queue.create ()

let pop_events' () =
  let events = Queue.to_list queue in
  Queue.clear queue;
  events
;;

let pop_events () =
  pop_events' ()
  |> List.map ~f:Event.Stable.of_latest
  |> [%sexp_of: Event.Stable.t list]
  |> Sexp.to_string_mach
  |> Js.string
;;

class type global = object
  (* [ rpcEffectInstrospectionSupported ] is used to distinguish between
     non-bonsai apps or bonsai apps that have not yet picked up the introspection
     changes. This is read by the devtool panel to provide a nice error message
     when attempting to inspect wikipedia/a non-bonsai app/an app that is on an
     outdated version of bonsai. *)
  method rpcEffectIntrospectionSupported : bool Js.t Js.prop

  (* [ rpcEffectIs_recording ] is whether newly sent rpc events should are recorded/not
     recorded where "recorded" means that the events are put into the queue that
     [rpcEffectPopEvents] reads. *)
  method rpcEffectIsRecording : bool Js.t Js.optdef Js.prop
  method rpcEffectStartRecording : (unit -> unit) Js.callback Js.prop
  method rpcEffectStopRecording : (unit -> unit) Js.callback Js.prop
  method rpcEffectPopEvents : (unit -> Js.js_string Js.t) Js.callback Js.prop
end

let global : global Js.t = Js.Unsafe.global

let is_recording () =
  match Js.Optdef.to_option global##.rpcEffectIsRecording with
  | None -> false
  | Some x -> Js.to_bool x
;;

let should_record_effect = Effect.of_thunk is_recording

let maybe_record_event (event : Event.t lazy_t) =
  match is_recording () with
  | false -> ()
  | true ->
    let event = force event in
    Queue.enqueue queue event
;;

let start_recording () = global##.rpcEffectIsRecording := Js.Optdef.return (Js.bool true)
let stop_recording () = global##.rpcEffectIsRecording := Js.Optdef.return (Js.bool false)

let init_global () =
  global##.rpcEffectIntrospectionSupported := Js.bool true;
  (match is_recording () with
   | true ->
     (* In order to catch rpc's from the very beginning, the devtool panel api
        will set is_recording to true before any javascript in the page runs.
        If this is the case, then we do not want to set is_recording to false. *)
     ()
   | false -> global##.rpcEffectIsRecording := Js.Optdef.return (Js.bool false));
  global##.rpcEffectStartRecording := Js.wrap_callback start_recording;
  global##.rpcEffectStopRecording := Js.wrap_callback stop_recording;
  global##.rpcEffectPopEvents := Js.wrap_callback pop_events
;;

let run_top_level_side_effects () = init_global ()

let send_and_track_rpc
  ~rpc_kind
  ~get_current_time
  ~sexp_of_query
  ~sexp_of_response
  ~path
  ~send_rpc:actually_send_rpc
  ~query
  ~response_to_event
  =
  let open Effect.Let_syntax in
  let%bind.Effect id = get_id in
  let%bind start_time = get_current_time in
  let start_event =
    lazy
      (let query =
         match sexp_of_query with
         | None -> Or_no_sexp_of_provided.No_sexp_of_provided
         | Some sexp_of_query ->
           Or_no_sexp_of_provided.Sexp_of_provided (sexp_of_query query)
       in
       Event.V1.Started { id; rpc_kind; start_time; query; path })
  in
  maybe_record_event start_event;
  let%bind response = actually_send_rpc query in
  let%bind end_time = get_current_time in
  let duration = Time_ns.diff end_time start_time in
  let () =
    maybe_record_event (response_to_event response ~id ~duration ~sexp_of_response)
  in
  Effect.return response
;;

let send_and_track_rpc_from_poller
  ~rpc_kind
  ~get_current_time
  ~sexp_of_query
  ~sexp_of_response
  ~path
  ~send_rpc:actually_send_rpc
  ~query
  =
  send_and_track_rpc
    ~rpc_kind
    ~get_current_time
    ~sexp_of_query
    ~sexp_of_response
    ~path
    ~send_rpc:actually_send_rpc
    ~query
    ~response_to_event:(fun response ~id ~duration ~sexp_of_response ->
    match (response : 'response Or_error.t Bonsai.Effect_throttling.Poll_result.t) with
    | Aborted -> lazy (Event.V1.Aborted { id; duration })
    | Finished response ->
      lazy
        (let response =
           Or_error.map response ~f:(fun response ->
             match sexp_of_response with
             | None -> Or_no_sexp_of_provided.No_sexp_of_provided
             | Some sexp_of_response -> Sexp_of_provided (sexp_of_response response))
         in
         Finished { id; duration; response }))
;;

let send_and_track_rpc_from_dispatch
  ~rpc_kind
  ~get_current_time
  ~sexp_of_query
  ~sexp_of_response
  ~path
  ~send_rpc:actually_send_rpc
  ~query
  =
  send_and_track_rpc
    ~rpc_kind
    ~get_current_time
    ~sexp_of_query
    ~sexp_of_response
    ~path
    ~send_rpc:actually_send_rpc
    ~query
    ~response_to_event:(fun response ~id ~duration ~sexp_of_response ->
    lazy
      (let response =
         Or_error.map response ~f:(fun response ->
           match sexp_of_response with
           | None -> Or_no_sexp_of_provided.No_sexp_of_provided
           | Some sexp_of_response -> Sexp_of_provided (sexp_of_response response))
       in
       Event.V1.Finished { id; duration; response }))
;;

module For_testing = struct
  let get_introspection_supported () = Js.to_bool global##.rpcEffectIntrospectionSupported
  let get_is_recording = is_recording
  let pop_events = pop_events
  let pop_events' = pop_events'
  let start_recording = start_recording
  let stop_recording = stop_recording
end
