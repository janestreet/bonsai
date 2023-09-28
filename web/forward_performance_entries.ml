open Js_of_ocaml
open Async_kernel
open! Core
open Bonsai.Private
open Bonsai_protocol

type 'result t =
  { instrumented_computation : 'result Bonsai.Private.Computation.t
  ; shutdown : unit -> unit
  }

module Worker : sig
  (** Represents a web worker that you can send messages to. This type handles
      annoying details such as making sure that the web worker is ready to
      start receiving messages, serializing the messages, and batching several
      of the messages together.  *)
  type t

  (** Loads a web worker from the specified URL. [on_message] is called every
      time the web worker sends a message to the main thread. *)
  val create : url:string -> on_message:(t -> string -> unit) -> t

  (** Queues a message to be sent at the next call to [flush]. *)
  val send_message : t -> Worker_message.t -> unit

  (** Sends all the queued messages to the worker as a single message *)
  val flush : t -> unit

  val set_error_handler : t -> f:(Worker.errorEvent Js.t -> unit) -> unit
  val shutdown : t -> unit
end = struct
  (* The [acknowledged] field keeps track of whether the worker has sent back a
     message, which means that it is ready to receive messages. *)
  type t =
    { mutable acknowledged : bool
    ; mutable buffer : Worker_message.t Reversed_list.t
    ; worker : (Js.js_string Js.t, Js.js_string Js.t) Worker.worker Js.t
    }

  let create ~url ~on_message =
    (* We use a [blob] to circumvent the same-origin policy for web workers.
       Note that we aren't able to break through the browser's defenses
       totally, since the server must still configure its CSP to allow web
       workers from blobs. *)
    let worker =
      let blob =
        File.blob_from_string
          ~contentType:"application/javascript"
          [%string "importScripts('%{url}')"]
      in
      let blob_url = Dom_html.window##._URL##createObjectURL blob in
      Worker.create (Js.to_string blob_url)
    in
    let result = { worker; acknowledged = false; buffer = [] } in
    worker##.onmessage
      := Dom.handler (fun (message : Js.js_string Js.t Worker.messageEvent Js.t) ->
           result.acknowledged <- true;
           on_message result (Js.to_string message##.data);
           Js._false);
    result
  ;;

  let set_error_handler t ~f =
    t.worker##.onerror
      := Dom.handler (fun error_message ->
           f error_message;
           Js._false)
  ;;

  let send_message t message = t.buffer <- message :: t.buffer

  let flush t =
    if t.acknowledged
    then (
      let message = Versioned_message.V4 (Reversed_list.rev t.buffer) in
      let js_string =
        Js.bytestring (Bin_prot.Writer.to_string Versioned_message.bin_writer_t message)
      in
      t.worker##postMessage js_string;
      t.buffer <- [])
    else ()
  ;;

  let shutdown t =
    t.buffer <- [];
    t.worker##terminate
  ;;
end

let iter_entries performance_observer_entry_list ~f =
  performance_observer_entry_list##getEntries
  |> Js.to_array
  |> Array.iter ~f:(fun entry ->
       let label =
         let label = entry##.name |> Js.to_string in
         match Instrumentation.extract_node_path_from_entry_label label with
         | None -> `Other label
         | Some node_id -> `Bonsai node_id
       in
       let entry_type = entry##.entryType |> Js.to_bytestring in
       let start_time = entry##.startTime in
       let duration = entry##.duration in
       f { Entry.label; entry_type; start_time; duration })
;;

let uuid_to_url ~host ~port uuid = [%string "https://%{host}:%{port#Int}/%{uuid#Uuid}"]

let generate_uuid () =
  let random_state = Random.State.default in
  Uuid.create_random random_state
;;

let instrument ~host ~port ~worker_name component =
  let uuid, reused_uuid =
    let key = Js.string "bonsai-bug-session-uuid" in
    match Js.Optdef.to_option Dom_html.window##.sessionStorage with
    | None ->
      print_endline "No session storage; generating new session uuid";
      generate_uuid (), false
    | Some storage ->
      (match Js.Opt.to_option (storage##getItem key) with
       | None ->
         print_endline "No prior session uuid found; generating a new one.";
         let uuid = generate_uuid () in
         storage##setItem key (Js.string (Uuid.to_string uuid));
         uuid, false
       | Some uuid_string ->
         (match Option.try_with (fun () -> Uuid.of_string (Js.to_string uuid_string)) with
          | None ->
            print_endline
              "Found existing session uuid, but could not parse it; generating a new one.";
            let uuid = generate_uuid () in
            storage##setItem key (Js.string (Uuid.to_string uuid));
            uuid, false
          | Some uuid ->
            print_endline
              "Re-using existing session uuid. If you no longer have the debugger window \
               open, you can use the following link:";
            print_endline (uuid_to_url ~host ~port uuid);
            uuid, true))
  in
  if not reused_uuid
  then (
    let url = uuid_to_url ~host ~port uuid in
    Dom_html.window##open_
      (Js.string url)
      (Js.string "bonsai-bug")
      (Js.Opt.return (Js.string "noopener"))
    |> (ignore : Dom_html.window Js.t Js.opt -> unit));
  let graph_info_dirty = ref false in
  let graph_info = ref Graph_info.empty in
  let stop_ivar = Ivar.create () in
  let on_first_message worker =
    Worker.send_message worker (Uuid uuid);
    graph_info_dirty := true;
    let stop = Ivar.read stop_ivar in
    Async_kernel.every ~stop (Time_ns.Span.of_sec 0.2) (fun () ->
      if !graph_info_dirty
      then (
        graph_info_dirty := false;
        Worker.send_message worker (Message (Graph_info !graph_info)));
      Worker.flush worker;
      Javascript_profiling.clear_marks ();
      Javascript_profiling.clear_measures ());
    let performance_observer =
      let f new_entries observer =
        observer##takeRecords
        |> (ignore : PerformanceObserver.performanceEntry Js.t Js.js_array Js.t -> unit);
        iter_entries new_entries ~f:(fun entry ->
          Worker.send_message worker (Message (Performance_measure entry)))
      in
      PerformanceObserver.observe ~entry_types:[ "measure" ] ~f
    in
    Deferred.upon stop (fun () ->
      performance_observer##disconnect;
      Javascript_profiling.clear_marks ();
      Javascript_profiling.clear_measures ();
      Worker.shutdown worker)
  in
  let worker =
    (* We have no way of knowing when the web worker has set up its [onmessage]
       callback and is ready to receive messages. Thus, before sending any
       messages to it, we first wait until it is sends an acknowledgement
       message. *)
    let got_first_message = ref false in
    Worker.create
      ~url:[%string "https://%{host}:%{port#Int}/%{worker_name}"]
      ~on_message:(fun worker _ ->
      if not !got_first_message then got_first_message := true;
      on_first_message worker)
  in
  let component =
    Bonsai.Private.Graph_info.iter_graph_updates component ~on_update:(fun gi ->
      (* Instead of sending a message every time the graph changes, we maintain
         the current graph_info and mark it as dirty, so that the [every] loop
         send a single message per flush. *)
      graph_info := gi;
      graph_info_dirty := true)
  in
  let instrumented_computation =
    Instrumentation.instrument_computation
      component
      ~start_timer:(fun s -> Javascript_profiling.Manual.mark (s ^ "before"))
      ~stop_timer:(fun s ->
        let before = s ^ "before" in
        let after = s ^ "after" in
        Javascript_profiling.Manual.mark after;
        Javascript_profiling.Manual.measure ~name:s ~start:before ~end_:after)
  in
  let shutdown () = Ivar.fill_if_empty stop_ivar () in
  let shutdown () =
    match Or_error.try_with shutdown with
    | Ok () -> ()
    | Error e -> eprint_s [%sexp (e : Error.t)]
  in
  Worker.set_error_handler worker ~f:(fun message ->
    Firebug.console##warn message;
    shutdown ());
  { instrumented_computation; shutdown }
;;
