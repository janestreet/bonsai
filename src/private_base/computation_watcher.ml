open! Core
open! Import

module Source_code_positions = struct
  type pending =
    { watchers : string option Source_code_position.Map.t
    ; depended_on_at : Source_code_position.Set.t
    }
  [@@deriving sexp_of]

  type finalized =
    { watchers : string option Source_code_position.Map.t
    ; dependency_definitions : Source_code_position.Set.t
    ; depended_on_at : Source_code_position.Set.t
    }
  [@@deriving sexp_of]

  type _ t =
    | Pending : pending -> pending t
    | Finalized : finalized -> finalized t
  [@@deriving sexp_of]

  let add_watcher (type a) (positions : a t) here label : a t =
    let update_watchers watchers =
      Map.update watchers here ~f:(fun existing_label ->
        match existing_label with
        (* Top-level None, there was no value prior to this *)
        | None -> label
        | Some existing_label ->
          (match (Option.equal String.equal) existing_label label with
           | true -> existing_label
           | false ->
             eprint_s
               [%message
                 "BUG"
                   [%here]
                   "attempting to set two different labels for the same computation \
                    watcher"];
             existing_label))
    in
    match positions with
    | Finalized { watchers; dependency_definitions; depended_on_at } ->
      Finalized
        { dependency_definitions; watchers = update_watchers watchers; depended_on_at }
    | Pending { watchers; depended_on_at } ->
      Pending { watchers = update_watchers watchers; depended_on_at }
  ;;

  let get_watchers (type a) (a : a t) =
    match a with
    | Pending { watchers; _ } -> watchers
    | Finalized { watchers; _ } -> watchers
  ;;

  let merge_watchers a b =
    Map.merge a b ~f:(fun ~key:_ -> function
      | `Left left -> Some left
      | `Right right -> Some right
      (* If this watcher has a label in either map, keep that label *)
      | `Both (Some left, None) -> Some (Some left)
      (* If this watcher has a label in either map, keep that label *)
      | `Both (None, Some right) -> Some (Some right)
      | `Both (None, None) -> Some None
      (* If watcher had label in both maps, arbitrarily pick one *)
      | `Both (Some left, Some _right) -> Some (Some left))
  ;;

  let merge_watchers_of_t (a : _ t) (b : _ t) =
    let a = get_watchers a in
    let b = get_watchers b in
    merge_watchers a b
  ;;

  let empty =
    Pending
      { watchers = Source_code_position.Map.empty
      ; depended_on_at = Source_code_position.Set.empty
      }
  ;;

  let add_dependency_definition (type a) (t : a t) here =
    let watchers, dependency_definitions, depended_on_at =
      match t with
      | Pending { watchers; depended_on_at } ->
        watchers, Source_code_position.Set.empty, depended_on_at
      | Finalized { watchers; dependency_definitions; depended_on_at } ->
        watchers, dependency_definitions, depended_on_at
    in
    Finalized
      { watchers
      ; dependency_definitions = Set.add dependency_definitions here
      ; depended_on_at
      }
  ;;

  let add_depended_on_at (type a) (t : a t) here : a t =
    match t with
    | Pending { watchers; depended_on_at } ->
      Pending { watchers; depended_on_at = Set.add depended_on_at here }
    | Finalized { watchers; dependency_definitions; depended_on_at } ->
      Finalized
        { watchers; dependency_definitions; depended_on_at = Set.add depended_on_at here }
  ;;

  let merge_depended_on_at (type a) (t : a t) b_depended_on_at : a t =
    match t with
    | Pending { watchers; depended_on_at } ->
      Pending { watchers; depended_on_at = Set.union b_depended_on_at depended_on_at }
    | Finalized { watchers; dependency_definitions; depended_on_at } ->
      Finalized
        { watchers
        ; dependency_definitions
        ; depended_on_at = Set.union b_depended_on_at depended_on_at
        }
  ;;

  let extract_finalized (Finalized t) = t
end

module Config = struct
  type t =
    { log_action : bool
    ; log_model_before : bool
    ; log_model_after : bool
    ; log_watcher_positions : bool
    ; log_dependency_definition_position : bool
    ; log_incr_info : bool
    ; label : string option
    }
  [@@deriving sexp_of]

  let merge (left : t) (right : t) =
    (* If either the left or right log config for a property is set to true, we will set
       the value to true from this point forward *)
    { log_action = left.log_action || right.log_action
    ; log_model_before = left.log_model_before || right.log_model_before
    ; log_model_after = left.log_model_after || right.log_model_after
    ; log_watcher_positions = left.log_watcher_positions || right.log_watcher_positions
    ; log_dependency_definition_position =
        left.log_dependency_definition_position
        || right.log_dependency_definition_position
    ; log_incr_info = left.log_incr_info || right.log_incr_info
    ; label = (* We take the rigthmost label as that is the innermost config *)
              right.label
    }
  ;;
end

(* Mutability is required here because whenever we hit the incremental node, we cannot be
   sure of two things:

   1. We do not know if we've already wrapped the Incremental node
   2. We do not know if the current copy of [Source_code_positions.t] is the most
      up-to-date version

   Prior versions just wrapped the Incremental node multiple times, which created a new
   node in the queue for each [depended_on_at] location per update of the Incremental
   node. This is an undesired outcome, as we want to have one node per update for the
   Incremental node.

   We could either run the transformation twice (once to retrieve all the source code
   positions necessary, and then another to transform the computations), or use mutability
   to share a hashmap that allows for retrieving the most complete
   [Source_code_positions.t] as well as provides a way to know if the Incremental node has
   already been wrapped.
*)
module Id_location_hashmap = struct
  module Key = struct
    module T = struct
      type t =
        [ `Named of Type_equal.Id.Uid.t
        | `Incr of Incremental.For_analyzer.Node_id.t
        ]
      [@@deriving compare, hash, sexp_of]
    end

    include T
    include Hashable.Make_plain_and_derive_hash_fold_t (T)
  end

  include Key.Table

  let update_and_check_if_value_set ~id ~update_data:(update_data, config) table =
    let stored_value = Hashtbl.find table id in
    (* Merge the [depended_on_at], [dependency_definitions], AND [watcher] values if
       watcher is enabled. The node prior to this should have been a Map node, so it
       should have a [depended_on_at] value *)
    Hashtbl.update table id ~f:(function
      | None -> Source_code_positions.Finalized update_data, config
      | Some (Source_code_positions.Finalized existing_value_positions, old_config) ->
        ( Source_code_positions.Finalized
            { depended_on_at =
                Set.union
                  existing_value_positions.depended_on_at
                  update_data.depended_on_at
            ; watchers =
                Source_code_positions.merge_watchers
                  existing_value_positions.watchers
                  update_data.watchers
            ; dependency_definitions =
                Set.union
                  existing_value_positions.dependency_definitions
                  update_data.dependency_definitions
            }
        , Config.merge old_config config ));
    match stored_value with
    | None -> `Not_set
    | Some _ -> `Already_set
  ;;
end

module Type_id_location_map = struct
  module Data = struct
    type 'a t = Source_code_positions.finalized Source_code_positions.t
    [@@deriving sexp_of]
  end

  module T = Univ_map.Make (Univ_map.Type_id_key) (Data)
  module Merge = Univ_map.Merge (Univ_map.Type_id_key) (Data) (Data) (Data)

  type t = T.t

  type 'acc folder =
    { f :
        'a 'b.
        'acc
        -> 'a Type_equal.Id.t
        -> Source_code_positions.finalized Source_code_positions.t
        -> 'acc
    }

  type 'b mapper =
    { f :
        'a.
        'a Type_equal.Id.t
        -> Source_code_positions.finalized Source_code_positions.t
        -> 'b
    }

  let set = T.set
  let empty = T.empty
  let singleton = T.singleton
  let find = T.find
  let remove = T.remove

  let merge a b =
    Merge.merge
      b
      a
      ~f:
        { f =
            (fun ~key:_ -> function
              | `Left left -> Some left
              | `Right right -> Some right
              | `Both (Source_code_positions.Finalized a, Finalized b) ->
                Some
                  (Source_code_positions.Finalized
                     { dependency_definitions =
                         Set.union a.dependency_definitions b.dependency_definitions
                     ; depended_on_at = Set.union a.depended_on_at b.depended_on_at
                     ; watchers =
                         Source_code_positions.merge_watchers a.watchers b.watchers
                     }))
        }
  ;;

  let fold t ~init ({ f } : _ folder) =
    Map.fold
      (Type_equal.conv T.type_equal t)
      ~init
      ~f:(fun ~key:_ ~data:(T (key, data)) acc -> f acc key data)
  ;;

  let map_to_list t ({ f } : _ mapper) =
    List.map (T.to_alist t) ~f:(fun (T (key, data)) -> f key data)
  ;;
end

let format_depended_on_at_positions
  ~config:({ log_watcher_positions = _; _ } : Config.t)
  ({ depended_on_at; _ } : Source_code_positions.finalized)
  =
  let depended_on_at = Set.to_list depended_on_at in
  match true, List.length depended_on_at with
  | false, _ | true, 0 -> ""
  | true, _ ->
    List.fold
      depended_on_at
      ~init:"\n\nUpdated computation depended on at:"
      ~f:(fun acc depended_on_at ->
        let depended_on_at_string = Source_code_position.to_string depended_on_at in
        acc ^ [%string "\n - %{depended_on_at_string}"])
;;

let format_watcher_positions
  ~config:({ log_watcher_positions; _ } : Config.t)
  ({ watchers; _ } : Source_code_positions.finalized)
  =
  match log_watcher_positions, Map.length watchers with
  | false, _ | true, 0 -> ""
  | true, _ ->
    Map.fold watchers ~init:"\n\nWatchers:" ~f:(fun ~key:watcher ~data:label acc ->
      let label =
        match label with
        | None -> ""
        | Some label -> [%string " [%{label}]"]
      in
      let watcher_string = Source_code_position.to_string watcher in
      acc ^ [%string "\n -%{label} %{watcher_string}"])
;;

let format_dependency_definition_position
  ~config:({ log_dependency_definition_position; _ } : Config.t)
  ({ dependency_definitions; _ } : Source_code_positions.finalized)
  =
  match log_dependency_definition_position with
  | false -> ""
  | true ->
    let here_string =
      match Set.to_list dependency_definitions with
      | dependency_definition :: [] ->
        [%string " at [%{Source_code_position.to_string dependency_definition}]"]
      | dependency_definitions ->
        let dependency_definitions_string =
          List.fold dependency_definitions ~init:"" ~f:(fun acc dependency_definition ->
            acc ^ [%string "\n - %{dependency_definition#Source_code_position}"])
        in
        [%string "s at: %{dependency_definitions_string}"]
    in
    here_string
;;

let log_model_action_monitor
  (type model action)
  ~sexp_of_model
  ?(sexp_of_action = sexp_of_opaque)
  ?(action : action option = None)
  ?(info_string_prefix : string = "")
  ~(model_before : model)
  ~(model_after : model)
  ~(config : Config.t)
  ()
  =
  let { Config.log_model_before
      ; log_action
      ; log_model_after
      ; log_incr_info = _
      ; log_watcher_positions = _
      ; log_dependency_definition_position = _
      ; label = _
      }
    =
    config
  in
  let old_model_string =
    match log_model_before with
    | true ->
      let model_before = sexp_of_model model_before in
      Sexp.to_string_hum [%message (model_before : Sexp.t)]
    | false -> ""
  in
  let new_model_string =
    match log_model_after with
    | true ->
      let model_after = sexp_of_model model_after in
      Sexp.to_string_hum [%message (model_after : Sexp.t)]
    | false -> ""
  in
  let action_string =
    match log_action, action with
    | true, Some action ->
      let old_model_action_separator =
        if String.length old_model_string > 0 then " " else ""
      in
      let action = sexp_of_action action in
      old_model_action_separator ^ Sexp.to_string_hum [%message (action : Sexp.t)]
    | false, Some _ | false, None | true, None -> ""
  in
  let old_new_model_separator =
    if (String.length old_model_string > 0 || String.length action_string > 0)
       && String.length new_model_string > 0
    then " -> "
    else ""
  in
  let info_string =
    old_model_string ^ action_string ^ old_new_model_separator ^ new_model_string
  in
  match String.length info_string with
  | 0 -> ""
  | _ -> info_string_prefix ^ info_string
;;

module Node = struct
  type t =
    | Named_or_incr :
        { source_code_positions : Source_code_positions.finalized
        ; incr_info : Info.t option
        ; kind : [ `Named | `Incr ]
        ; config : Config.t
        }
        -> t
    | State_machine_like :
        { source_code_positions : Source_code_positions.finalized
        ; model_before : 'a
        ; model_after : 'a
        ; action : 'b option
        ; sexp_of_model : 'a -> Sexp.t
        ; sexp_of_action : ('b -> Sexp.t) option
        ; kind : [ `State_machine0 | `State_machine1 | `Wrap ]
        ; config : Config.t
        }
        -> t
    | Reset :
        { source_code_positions : Source_code_positions.finalized
        ; model_before : 'a
        ; model_after : 'a
        ; sexp_of_model : 'a -> Sexp.t
        ; kind : [ `State_machine0 | `State_machine1 | `Wrap ]
        ; config : Config.t
        }
        -> t

  let kind_to_string = function
    | `Named -> "Named node"
    | `Incr -> "Incremental node"
    | `State_machine0 -> "State_machine0"
    | `State_machine1 -> "State_machine1"
    | `Wrap -> "Wrap node"
  ;;

  let num_watchers_prefix ({ watchers; _ } : Source_code_positions.finalized) =
    match Map.length watchers with
    | 0 ->
      eprint_s [%message "BUG" [%here] "Source_code_positions has 0 watcher positions"];
      ""
    | 1 -> "Watched computation"
    | num_watchers -> [%string "%{num_watchers#Int} watched computations"]
  ;;

  let get_shared_strings node =
    let source_code_positions, kind, config =
      match node with
      | Named_or_incr { source_code_positions; kind; config; _ } ->
        source_code_positions, kind_to_string kind, config
      | State_machine_like { source_code_positions; kind; config; _ } ->
        source_code_positions, kind_to_string kind, config
      | Reset { source_code_positions; kind; config; _ } ->
        source_code_positions, kind_to_string kind, config
    in
    let watchers_prefix = num_watchers_prefix source_code_positions in
    let watcher_positions_string =
      format_watcher_positions ~config source_code_positions
    in
    let depended_on_at_string =
      format_depended_on_at_positions ~config source_code_positions
    in
    let dependency_definition_string =
      format_dependency_definition_position ~config source_code_positions
    in
    ( watchers_prefix
    , kind
    , watcher_positions_string
    , dependency_definition_string
    , depended_on_at_string )
  ;;

  let to_string node =
    let ( watchers_prefix
        , kind
        , watcher_positions_string
        , dependency_definition_string
        , depended_on_at_string )
      =
      get_shared_strings node
    in
    let model_action_info_string =
      let info_string_prefix = "\n\nDetails: " in
      match node with
      | Named_or_incr { source_code_positions = _; incr_info; kind = _; config } ->
        (match config.log_incr_info with
         | false -> ""
         | true ->
           info_string_prefix ^ Sexp.to_string_hum [%message (incr_info : Info.t option)])
      | State_machine_like
          { source_code_positions = _
          ; model_before
          ; model_after
          ; action
          ; sexp_of_model
          ; sexp_of_action
          ; kind = _
          ; config
          } ->
        let sexp_of_action = Option.value sexp_of_action ~default:sexp_of_opaque in
        log_model_action_monitor
          ~sexp_of_model
          ~sexp_of_action
          ~config
          ~model_before
          ~model_after
          ~action
          ~info_string_prefix
          ()
      | Reset
          { source_code_positions = _
          ; model_before
          ; model_after
          ; sexp_of_model
          ; kind = _
          ; config
          } ->
        log_model_action_monitor
          ~sexp_of_model
          ~model_before
          ~model_after
          ~config
          ~info_string_prefix
          ()
    in
    {%string|%{watchers_prefix} updated due to %{kind}%{dependency_definition_string}%{model_action_info_string}%{watcher_positions_string}%{depended_on_at_string}|}
  ;;

  let log node = to_string node |> print_endline
end

module Output_queue = struct
  type t = Node.t Core.Queue.t

  let process_queue ~f (q : t) =
    Queue.iter q ~f;
    Queue.clear q
  ;;

  let log_all_in_queue q = process_queue ~f:Node.log q
end

let instrument_incremental_node
  ~here
  ~id
  ~value_id_observation_definition_positions
  ~watcher_queue
  value
  =
  let incr_info = (Incr.user_info value : Info.t option) in
  let kind =
    match id with
    | `Named _ -> `Named
    | `Incr _ -> `Incr
  in
  (* Using Incr.map for better code clarity as to when the stored value should be pulled
     from the hashmap *)
  Incr.map value ~f:(fun a ->
    (match Hashtbl.find value_id_observation_definition_positions id with
     | None ->
       let kind_string =
         match kind with
         | `Named -> "named"
         | `Incr -> "incremental"
       in
       (* This value should not be None, as this function should only be called once the
          user has set the value in the hashtable *)
       eprint_s
         [%message
           "BUG"
             (here : Source_code_position.t)
             (kind_string ^ " node did not have source code positions set")]
     | Some (stored_value, config) ->
       let source_code_positions =
         stored_value |> Source_code_positions.extract_finalized
       in
       Queue.enqueue
         watcher_queue
         (Node.Named_or_incr { source_code_positions; incr_info; kind; config }));
    a)
;;

module For_testing = struct
  let log_model_action_monitor = log_model_action_monitor
end
