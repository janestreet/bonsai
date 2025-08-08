open! Core
open! Import

module Source_code_positions : sig
  (** [watchers] is a list that contains the chain of Computation_watcher nodes that
      contain this current node. [dependency_definitions] a set of
      [Source_code_position.t]s that caused the Computation_watcher to update. All of
      these [dependency_definitions] are semantically the same but some nodes happen to be
      generated more than once [depended_on_at] is the location of the Map node/let%arr
      that was called on the value *)
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

  val empty : pending t
  val add_watcher : 'a t -> Source_code_position.t -> string option -> 'a t
  val add_dependency_definition : 'a t -> Source_code_position.t -> finalized t
  val add_depended_on_at : 'a t -> Source_code_position.t -> 'a t
  val extract_finalized : finalized t -> finalized
  val merge_depended_on_at : 'a t -> Source_code_position.Set.t -> 'a t
  val merge_watchers_of_t : _ t -> _ t -> string option Source_code_position.Map.t
end

(** [dependecy_definition_position] is the first thing set when looking for free
    variables, so we know that we can use [finalized] in this map *)
module Type_id_location_map : sig
  type t

  type 'acc folder =
    { f :
        'a.
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

  val set
    :  t
    -> key:_ Type_equal.Id.t
    -> data:Source_code_positions.finalized Source_code_positions.t
    -> t

  val find
    :  t
    -> _ Type_equal.Id.t
    -> Source_code_positions.finalized Source_code_positions.t option

  (** If the key exists in both a and b, b will overwrite a *)
  val merge : t -> t -> t

  val empty : t

  val singleton
    :  _ Type_equal.Id.t
    -> Source_code_positions.finalized Source_code_positions.t
    -> t

  val remove : t -> _ Type_equal.Id.t -> t
  val fold : t -> init:'acc -> 'acc folder -> 'acc
  val map_to_list : t -> 'a mapper -> 'a list
end

module Config : sig
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

  val merge : t -> t -> t
end

module Id_location_hashmap : sig
  include
    Hashtbl.S_plain
    with type key =
      [ `Named of Type_equal.Id.Uid.t
      | `Incr of Incremental.For_analyzer.Node_id.t
      ]

  val update_and_check_if_value_set
    :  id:key
    -> update_data:Source_code_positions.finalized * Config.t
    -> (Source_code_positions.finalized Source_code_positions.t * Config.t) t
    -> [ `Already_set | `Not_set ]
end

module Node : sig
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

  val to_string : t -> string
  val log : t -> unit
end

module Output_queue : sig
  type t = Node.t Queue.t

  val process_queue : f:(Node.t -> unit) -> t -> unit
  val log_all_in_queue : t -> unit
end

val instrument_incremental_node
  :  here:Source_code_position.t
  -> id:Id_location_hashmap.key
  -> value_id_observation_definition_positions:
       (Source_code_positions.finalized Source_code_positions.t * Config.t)
         Id_location_hashmap.t
  -> watcher_queue:Output_queue.t
  -> 'a Ui_incr.Incr.t
  -> 'a Ui_incr.Incr.t

module For_testing : sig
  val log_model_action_monitor
    :  sexp_of_model:('model -> Sexp.t)
    -> ?sexp_of_action:('action -> Sexp.t)
    -> ?action:'action option
    -> ?info_string_prefix:string
    -> model_before:'model
    -> model_after:'model
    -> config:Config.t
    -> unit
    -> string
end
