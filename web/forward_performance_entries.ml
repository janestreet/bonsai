open Js_of_ocaml

(* This module has been upstreamed into js_of_ocaml, so we should remove it
   once the new compiler gets released to opam. *)
module PerformanceObserver : sig
  (* Js_of_ocaml library
   * http://www.ocsigen.org/js_of_ocaml/
   * Copyright (C) 2021 Philip White
   *
   * This program is free software; you can redistribute it and/or modify
   * it under the terms of the GNU Lesser General Public License as published by
   * the Free Software Foundation, with linking exception;
   * either version 2.1 of the License, or (at your option) any later version.
   *
   * This program is distributed in the hope that it will be useful,
   * but WITHOUT ANY WARRANTY; without even the implied warranty of
   * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   * GNU Lesser General Public License for more details.
   *
   * You should have received a copy of the GNU Lesser General Public License
   * along with this program; if not, write to the Free Software
   * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
  *)

  (** PerformanceObserver API

      A code example:
      {[
        if (PerformanceObserver.is_supported()) then
          let entry_types = [ "measure" ] in
          let f entries observer =
            let entries = entries##getEntries in
            Firebug.console##debug entries ;
            Firebug.console##debug observer
          in
          PerformanceObserver.observe ~entry_types ~f
            ()
      ]}

      @see <https://developer.mozilla.org/en-US/docs/Web/API/PerformanceObserver> for API documentation.
  *)

  class type performanceObserverInit =
    object
      method entryTypes : Js.js_string Js.t Js.js_array Js.t Js.writeonly_prop
    end

  class type performanceEntry =
    object
      method name : Js.js_string Js.t Js.readonly_prop
      method entryType : Js.js_string Js.t Js.readonly_prop
      method startTime : float Js.readonly_prop
      method duration : float Js.readonly_prop
    end

  class type performanceObserverEntryList =
    object
      method getEntries : performanceEntry Js.t Js.js_array Js.t Js.meth
    end

  class type performanceObserver =
    object
      method observe : performanceObserverInit Js.t -> unit Js.meth
      method disconnect : unit Js.meth
      method takeRecords : performanceEntry Js.t Js.js_array Js.t Js.meth
    end

  val observe
    :  entry_types:string list
    -> f:(performanceObserverEntryList Js.t -> performanceObserver Js.t -> unit)
    -> performanceObserver Js.t
end = struct
  open Js_of_ocaml

  class type performanceObserverInit =
    object
      method entryTypes : Js.js_string Js.t Js.js_array Js.t Js.writeonly_prop
    end

  class type performanceEntry =
    object
      method name : Js.js_string Js.t Js.readonly_prop
      method entryType : Js.js_string Js.t Js.readonly_prop
      method startTime : float Js.readonly_prop
      method duration : float Js.readonly_prop
    end

  class type performanceObserverEntryList =
    object
      method getEntries : performanceEntry Js.t Js.js_array Js.t Js.meth
    end

  class type performanceObserver =
    object
      method observe : performanceObserverInit Js.t -> unit Js.meth
      method disconnect : unit Js.meth
      method takeRecords : performanceEntry Js.t Js.js_array Js.t Js.meth
    end

  let performanceObserver = Js.Unsafe.global##._PerformanceObserver

  let performanceObserver
    : ((performanceObserverEntryList Js.t -> performanceObserver Js.t -> unit)
         Js.callback
       -> performanceObserver Js.t)
        Js.constr
    =
    performanceObserver
  ;;

  let observe ~entry_types ~f =
    let entry_types = entry_types |> List.map Js.string |> Array.of_list |> Js.array in
    let performance_observer_init : performanceObserverInit Js.t = Js.Unsafe.obj [||] in
    let () = performance_observer_init##.entryTypes := entry_types in
    let obs = new%js performanceObserver (Js.wrap_callback f) in
    let () = obs##observe performance_observer_init in
    obs
  ;;
end

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
  val create : url:string -> on_message:(string -> unit) -> t

  (** Queues a message to be sent at the next call to [flush]. *)
  val send_message : t -> Message.t -> unit

  (** Sends all the queued messages to the worker as a single message *)
  val flush : t -> unit

  val set_error_handler : t -> f:(Worker.errorEvent Js.t -> unit) -> unit
  val shutdown : t -> unit
end = struct
  (* The [acknowledged] field keeps track of whether the worker has sent back a
     message, which means that it is ready to receive messages. *)
  type t =
    { mutable acknowledged : bool
    ; mutable buffer : Message.t Reversed_list.t
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
      on_message (Js.to_string message##.data);
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
      let message = Versioned_message.V2 (Reversed_list.rev t.buffer) in
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

let instrument ~host ~port ~worker_name component =
  let worker =
    (* Once the worker sends an acknowledgement message, we can send the graph
       info. It's possible that the worker has already received a info message,
       but we're sending one now, just to be sure.

       The reason we need to do it this way is that we have no way of knowing
       when the web worker has set up its [onmessage] callback and is ready to
       receive message. Thus, we wait until the worker notifies us explicitly
       that it is ready to receive messages.

       This onmessage callback is also a convenient place to receive the UUID
       that identifies this profiling session. Since web workers cannot open
       new windows, we must open the window from the main page. To keep the URL
       of the server decoupled from this logic, we just receive the URL from
       the worker. *)
    Worker.create
      ~url:[%string "https://%{host}:%{port#Int}/%{worker_name}"]
      ~on_message:(fun url ->
        Dom_html.window##open_
          (Js.string url)
          (Js.string "bonsai-bug")
          (Js.Opt.return (Js.string "noopener"))
        |> (ignore : Dom_html.window Js.t Js.opt -> unit))
  in
  let performance_observer =
    let f new_entries observer =
      observer##takeRecords
      |> (ignore : PerformanceObserver.performanceEntry Js.t Js.js_array Js.t -> unit);
      iter_entries new_entries ~f:(fun entry ->
        Worker.send_message worker (Performance_measure entry))
    in
    PerformanceObserver.observe ~entry_types:[ "measure" ] ~f
  in
  let graph_info_dirty = ref false in
  let graph_info = ref Graph_info.empty in
  let component =
    Bonsai.Private.Graph_info.iter_graph_updates component ~on_update:(fun gi ->
      (* Instead of sending a message every time the graph changes, we maintain
         the current graph_info and mark it as dirty, so that the loop at the
         bottom of this function can send only one single message per flush. *)
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
  let stop_ivar = Async_kernel.Ivar.create () in
  let stop = Async_kernel.Ivar.read stop_ivar in
  Async_kernel.every ~stop (Time_ns.Span.of_sec 0.2) (fun () ->
    if !graph_info_dirty
    then (
      graph_info_dirty := false;
      Worker.send_message worker (Graph_info !graph_info));
    Worker.flush worker;
    Javascript_profiling.clear_marks ();
    Javascript_profiling.clear_measures ());
  let shutdown () =
    Async_kernel.Ivar.fill_if_empty stop_ivar ();
    performance_observer##disconnect;
    Javascript_profiling.clear_marks ();
    Javascript_profiling.clear_measures ();
    Worker.shutdown worker
  in
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
