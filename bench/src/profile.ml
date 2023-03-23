open! Core
open Js_of_ocaml
module Graph_info = Bonsai.Private.Graph_info

module Id = struct
  let instance = String.Table.create ()

  let of_node_path node_path =
    let key = Bonsai.Private.Node_path.to_string node_path in
    match Hashtbl.find instance key with
    | Some id -> id
    | None ->
      let id = Hashtbl.length instance in
      Hashtbl.set instance ~key ~data:id;
      id
  ;;
end

module Measurement = struct
  module Kind = struct
    module T = struct
      type t =
        | Startup
        | Snapshot
        | Named of string
      [@@deriving sexp, equal, compare]
    end

    include T
    include Comparable.Make_plain (T)

    let time_to_first_stabilization = "Bonsai_bench profile: first stabilization"
    let time_since_snapshot_began = "Bonsai_bench profile: current snapshot"

    let to_string = function
      | Named s -> s
      | Startup -> time_to_first_stabilization
      | Snapshot -> time_since_snapshot_began
    ;;

    let of_string s =
      if String.equal time_to_first_stabilization s
      then Startup
      else if String.equal time_since_snapshot_began s
      then Snapshot
      else Named s
    ;;

    let is_bonsai_measurement = function
      | Named _ -> true
      | _ -> false
    ;;
  end

  type t =
    { kind : Kind.t
    ; duration : float
    ; id : int option
    }
  [@@deriving sexp]

  let create ~label ~duration = { kind = Kind.of_string label; duration; id = None }
  let before label = label ^ "_before"
  let after label = label ^ "_after"
  let mark_before t = Javascript_profiling.Manual.mark (before (Kind.to_string t))

  let mark_after_and_measure t =
    let name = Kind.to_string t in
    let before = before name in
    let after = after name in
    Javascript_profiling.Manual.mark after;
    Javascript_profiling.Manual.measure ~name ~start:before ~end_:after
  ;;
end

module Accumulated_measurement = struct
  type t =
    { kind : Measurement.Kind.t
    ; total_duration : float
    ; count : int
    ; id : int option
    }
  [@@deriving sexp]

  let compare
        { kind; total_duration; _ }
        { kind = kind'; total_duration = total_duration'; _ }
    =
    match kind, kind' with
    | Named _, Named _ -> Float.descending total_duration total_duration'
    | _, _ -> Measurement.Kind.compare kind kind'
  ;;

  let of_measurement { Measurement.kind; duration; id } =
    { kind; count = 1; total_duration = duration; id }
  ;;

  let add { kind; total_duration; count; id } ~measurement =
    assert (Measurement.Kind.equal kind measurement.Measurement.kind);
    { kind
    ; count = count + 1
    ; total_duration = total_duration +. measurement.duration
    ; id
    }
  ;;
end

let create_summary_table ~total_time ~incremental_time =
  let open Ascii_table_kernel in
  to_string_noattr
    [ Column.create "Statistic" fst; Column.create "Value" snd ]
    ~limit_width_to:Int.max_value
    ~bars:`Unicode
    [ "Total time (ms)", Float.to_string total_time
    ; "Incremental time (ms)", Float.to_string incremental_time
    ; "Incremental Overhead (ms)", Float.to_string (total_time -. incremental_time)
    ; ( "Incremental Overhead (%)"
      , Percent.Always_percentage.to_string
          (Percent.of_percentage ((total_time -. incremental_time) /. total_time *. 100.))
      )
    ]
;;

let create_snapshot_table data ~incremental_time =
  let open Ascii_table_kernel in
  let columns =
    [ Column.create "Id" (fun { Accumulated_measurement.id; _ } ->
        match id with
        | Some int -> Int.to_string int
        | None -> "N/A")
    ; Column.create "Name" (fun { Accumulated_measurement.kind; _ } ->
        Measurement.Kind.to_string kind)
    ; Column.create "Times fired" (fun { Accumulated_measurement.count; _ } ->
        Int.to_string count)
    ; Column.create
        "Total time (ms)"
        (fun { Accumulated_measurement.total_duration; _ } ->
           Float.to_string total_duration)
    ; Column.create
        "Percent of incremental time"
        (fun { Accumulated_measurement.total_duration; _ } ->
           Percent.Always_percentage.to_string
             (Percent.of_percentage (total_duration /. incremental_time *. 100.)))
    ]
  in
  to_string_noattr columns data ~limit_width_to:Int.max_value ~bars:`Unicode
;;

let print_statistics data =
  let sorted_data = List.sort (Map.data data) ~compare:Accumulated_measurement.compare in
  let incremental_measurements, bonsai_bench_internals =
    List.partition_tf sorted_data ~f:(fun { kind; _ } ->
      Measurement.Kind.is_bonsai_measurement kind)
  in
  let total_time =
    match bonsai_bench_internals with
    | [ { Accumulated_measurement.total_duration; _ } ] -> total_duration
    | _ ->
      raise_s
        [%message
          "An error occurred while profiling your computation. Bonsai bench expected \
           only one internal measurement. Please report this error to the bonsai team."
            ~internal_measurements:
              (bonsai_bench_internals : Accumulated_measurement.t list)]
  in
  let incremental_time =
    List.sum
      (module Float)
      incremental_measurements
      ~f:(fun { kind; total_duration; _ } ->
        if Measurement.Kind.is_bonsai_measurement kind then total_duration else 0.)
  in
  print_endline "Summary:";
  print_endline (create_summary_table ~total_time ~incremental_time);
  print_endline "Details:";
  print_endline (create_snapshot_table incremental_measurements ~incremental_time)
;;

let accumulate_measurements
      ~(source_locations : Graph_info.Node_info.t Bonsai.Private.Node_path.Map.t)
      measurements
  =
  let with_ids, without_ids =
    List.map measurements ~f:(fun measurement ->
      match measurement.Measurement.kind with
      | Snapshot | Startup -> measurement
      | Named label ->
        Option.value
          ~default:measurement
          (let%bind.Option node_path =
             Bonsai.Private.Instrumentation.extract_node_path_from_entry_label label
           in
           let%bind.Option { node_type; here } = Map.find source_locations node_path in
           let%map.Option here = here in
           { measurement with
             kind = Named [%string "%{node_type} (%{here#Source_code_position})"]
           ; id = Some (Id.of_node_path node_path)
           }))
    |> List.fold
         ~init:(Int.Map.empty, Measurement.Kind.Map.empty)
         ~f:(fun (with_ids, without_ids) measurement ->
           let accumulate_measurements = function
             | None -> Accumulated_measurement.of_measurement measurement
             | Some accumulated -> Accumulated_measurement.add accumulated ~measurement
           in
           match measurement.id with
           | None ->
             with_ids, Map.update without_ids measurement.kind ~f:accumulate_measurements
           | Some id -> Map.update with_ids id ~f:accumulate_measurements, without_ids)
  in
  Map.fold without_ids ~init:with_ids ~f:(fun ~key:_ ~data:measurement acc ->
    let id =
      match Map.max_elt acc with
      (* This could happen if the user never [let%sub]s. It's not very realistic for a
         practical app, but totally possible to write. *)
      | None -> 0
      | Some (id, _) -> id + 1
    in
    let measurement = { measurement with id = Some id } in
    Map.set acc ~key:id ~data:measurement)
;;

let take_profile_snapshot ~name graph_info performance_entries =
  match List.length !performance_entries with
  | 0 | 1 -> ()
  | _ ->
    print_endline [%string "Bonsai_bench Profile: %{name}"];
    let source_locations =
      Graph_info.pull_source_locations_from_nearest_parent graph_info
    in
    print_statistics (accumulate_measurements ~source_locations !performance_entries);
    performance_entries := []
;;

let profile (T { clock; component; get_inject; interaction; name } : Config.t) =
  print_endline [%string "Running Bonsai_bench profile of %{name}"];
  let graph_info = ref Graph_info.empty in
  let component =
    Bonsai.Debug.instrument_computation
      component
      ~start_timer:(fun s -> Measurement.mark_before (Named s))
      ~stop_timer:(fun s -> Measurement.mark_after_and_measure (Named s))
  in
  let component =
    Graph_info.iter_graph_updates
      (Bonsai.Private.reveal_computation component)
      ~on_update:(fun gi -> graph_info := gi)
    |> Bonsai.Private.conceal_computation
  in
  let performance_entries = ref [] in
  let performance_observer =
    if PerformanceObserver.is_supported ()
    then
      PerformanceObserver.observe ~entry_types:[ "measure" ] ~f:(fun entries _ ->
        Array.iter
          (Js.to_array entries##getEntries)
          ~f:(fun entry ->
            let label = Js.to_string entry##.name in
            let duration = entry##.duration in
            performance_entries
            := Measurement.create ~label ~duration :: !performance_entries))
    else
      failwith
        "PerformanceObserver could not be found. Please reach out to webdev-public on \
         symphony for assistance."
  in
  let handle_profile name =
    Measurement.mark_after_and_measure Snapshot;
    take_profile_snapshot ~name !graph_info performance_entries;
    Measurement.mark_before Snapshot
  in
  let runner =
    Runner.initialize
      ~filter_profiles:false
      ~wrap_driver_creation:
        { f =
            (fun create_driver ->
               Measurement.mark_before Startup;
               let driver = create_driver () in
               Measurement.mark_after_and_measure Startup;
               driver)
        }
      ~clock
      ~component
      ~get_inject
      ~interaction
  in
  take_profile_snapshot ~name:"startup" !graph_info performance_entries;
  Measurement.mark_before Snapshot;
  Runner.run_interactions runner ~handle_profile;
  performance_observer##disconnect;
  Runner.invalidate_observers runner
;;
