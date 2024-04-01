open! Core
open! Import

module type Enum = Module_types.Enum
module type Comparator = Module_types.Comparator

type ('k, 'cmp) comparator = ('k, 'cmp) Module_types.comparator
type 'a t
type graph

include Applicative.S with type 'a t := 'a t
include Mapn with type 'a t := 'a t

val return : 'a -> 'a t
val map : 'a t -> f:('a -> 'b) -> 'b t
val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
val both : 'a t -> 'b t -> ('a * 'b) t
val cutoff : 'a t -> equal:('a -> 'a -> bool) -> 'a t
val all_map : ('k, graph -> 'v t, 'cmp) Map.t -> graph -> ('k, 'v, 'cmp) Map.t t

(** Useful for optional args that take [Bonsai.t]s.
    Note: the inverse operation is not possible. *)
val transpose_opt : 'a t option -> 'a option t

val state
  :  ?reset:('model -> 'model)
  -> ?sexp_of_model:('model -> Sexp.t)
  -> ?equal:('model -> 'model -> bool)
  -> 'model
  -> graph
  -> 'model t * ('model -> unit Effect.t) t

val state_opt
  :  ?reset:('model option -> 'model option)
  -> ?sexp_of_model:('model -> Sexp.t)
  -> ?equal:('model -> 'model -> bool)
  -> ?default_model:'model
  -> graph
  -> 'model option t * ('model option -> unit Effect.t) t

module Apply_action_context : sig
  type 'action t = 'action Apply_action_context.t

  val inject : 'action t -> 'action -> unit Effect.t
  val schedule_event : _ t -> unit Effect.t -> unit
end

module Computation_status : sig
  type 'input t =
    | Active of 'input
    | Inactive
  [@@deriving sexp_of]
end

type ('model, 'action, 'return) resetter :=
  'action Apply_action_context.t -> 'model -> 'model

module Toggle : sig
  type nonrec t =
    { state : bool t
    ; set_state : (bool -> unit Effect.t) t
    ; toggle : unit Effect.t t
    }
end

val toggle : default_model:bool -> graph -> bool t * unit Effect.t t
val toggle' : default_model:bool -> graph -> Toggle.t

val state_machine0
  :  ?reset:('model, 'action, unit) resetter
  -> ?sexp_of_model:('model -> Sexp.t)
  -> ?sexp_of_action:('action -> Sexp.t)
  -> ?equal:('model -> 'model -> bool)
  -> default_model:'model
  -> apply_action:('action Apply_action_context.t -> 'model -> 'action -> 'model)
  -> graph
  -> 'model t * ('action -> unit Effect.t) t

val state_machine1
  :  ?reset:('model, 'action, unit) resetter
  -> ?sexp_of_model:('model -> Sexp.t)
  -> ?sexp_of_action:('action -> Sexp.t)
  -> ?equal:('model -> 'model -> bool)
  -> default_model:'model
  -> apply_action:
       ('action Apply_action_context.t
        -> 'input Computation_status.t
        -> 'model
        -> 'action
        -> 'model)
  -> 'input t
  -> graph
  -> 'model t * ('action -> unit Effect.t) t

val actor0
  :  ?reset:
       (inject:('action -> 'return Effect.t)
        -> schedule_event:(unit Effect.t -> unit)
        -> 'model
        -> 'model)
  -> ?sexp_of_model:('model -> Sexp.t)
  -> ?sexp_of_action:('action -> Sexp.t)
  -> ?equal:('model -> 'model -> bool)
  -> default_model:'model
  -> recv:
       (inject:('action -> 'return Effect.t)
        -> schedule_event:(unit Effect.t -> unit)
        -> 'model
        -> 'action
        -> 'model * 'return)
  -> graph
  -> 'model t * ('action -> 'return Effect.t) t

val actor1
  :  ?sexp_of_action:('action -> Sexp.t)
  -> ?reset:
       (inject:('action -> 'return Effect.t)
        -> schedule_event:(unit Effect.t -> unit)
        -> 'model
        -> 'model)
  -> ?sexp_of_model:('model -> Sexp.t)
  -> ?equal:('model -> 'model -> bool)
  -> default_model:'model
  -> recv:
       (inject:('action -> 'return Effect.t)
        -> schedule_event:(unit Effect.t -> unit)
        -> 'input Computation_status.t
        -> 'model
        -> 'action
        -> 'model * 'return)
  -> 'input t
  -> graph
  -> 'model t * ('action -> 'return Effect.t) t

val freeze
  :  ?sexp_of_model:('a -> Sexp.t)
  -> ?equal:('a -> 'a -> bool)
  -> 'a t
  -> graph
  -> 'a t

val fix
  :  'input t
  -> f:(recurse:('input t -> graph -> 'result t) -> 'input t -> graph -> 'result t)
  -> graph
  -> 'result t

val fix2
  :  'a t
  -> 'b t
  -> f:
       (recurse:('a t -> 'b t -> graph -> 'result t)
        -> 'a t
        -> 'b t
        -> graph
        -> 'result t)
  -> graph
  -> 'result t

val scope_model : ('a, _) comparator -> on:'a t -> for_:(graph -> 'b t) -> graph -> 'b t

val most_recent_some
  :  ?sexp_of_model:('b -> Sexp.t)
  -> equal:('b -> 'b -> bool)
  -> 'a t
  -> f:('a -> 'b option)
  -> graph
  -> 'b option t

val most_recent_value_satisfying
  :  ?sexp_of_model:('a -> Sexp.t)
  -> equal:('a -> 'a -> bool)
  -> 'a t
  -> condition:('a -> bool)
  -> graph
  -> 'a option t

val previous_value
  :  ?sexp_of_model:('a -> Sexp.t)
  -> equal:('a -> 'a -> bool)
  -> 'a t
  -> graph
  -> 'a option t

val wrap
  :  ?reset:('model, 'action, unit) resetter
  -> ?sexp_of_model:('model -> Sexp.t)
  -> ?equal:('model -> 'model -> bool)
  -> default_model:'model
  -> apply_action:
       ('action Apply_action_context.t -> 'result -> 'model -> 'action -> 'model)
  -> f:('model t -> ('action -> unit Effect.t) t -> graph -> 'result t)
  -> graph
  -> 'result t

val enum
  :  (module Enum with type t = 'k)
  -> match_:'k t
  -> with_:('k -> graph -> 'a t)
  -> graph
  -> 'a t

val with_model_resetter : f:(graph -> 'a t) -> graph -> 'a t * unit Effect.t t
val with_model_resetter' : f:(reset:unit Effect.t t -> graph -> 'a t) -> graph -> 'a t

(** [peek] maps a [Bonsai.t] to an [Effect.t] with the same underlying value.
    This allows you to inspect the ['a] value from inside of a [let%bind.Effect]
    chain which might have been changed by previous effects. It is analogous to
    [peek] on other abstract data types, including [Deferred.t]s and [Mvar.t]s,
    but more constrained in that you still can only read from within an effect bind.

    The ['a Computation_state.t] returned by the effect means that if the value
    was inactive at the time it is peeked, then the effect will be unable to
    retrieve it. *)
val peek : 'a t -> graph -> 'a Computation_status.t Effect.t t

module Clock : sig
  val approx_now : tick_every:Time_ns.Span.t -> graph -> Time_ns.t t
  val now : graph -> Time_ns.t t

  module Before_or_after : sig
    type t = Ui_incr.Before_or_after.t =
      | Before
      | After
    [@@deriving sexp, equal]
  end

  val at : Time_ns.t t -> graph -> Before_or_after.t t

  val every
    :  when_to_start_next_effect:
         [< `Wait_period_after_previous_effect_starts_blocking
         | `Wait_period_after_previous_effect_finishes_blocking
         | `Every_multiple_of_period_non_blocking
         | `Every_multiple_of_period_blocking
         ]
    -> ?trigger_on_activate:bool
    -> Time_ns.Span.t
    -> unit Effect.t t
    -> graph
    -> unit

  val get_current_time : graph -> Time_ns.t Effect.t t
  val sleep : graph -> (Time_ns.Span.t -> unit Effect.t) t
  val until : graph -> (Time_ns.t -> unit Effect.t) t
end

module Edge : sig
  val on_change
    :  ?sexp_of_model:('a -> Sexp.t)
    -> equal:('a -> 'a -> bool)
    -> 'a t
    -> callback:('a -> unit Effect.t) t
    -> graph
    -> unit

  val on_change'
    :  ?sexp_of_model:('a -> Sexp.t)
    -> equal:('a -> 'a -> bool)
    -> 'a t
    -> callback:('a option -> 'a -> unit Effect.t) t
    -> graph
    -> unit

  val lifecycle
    :  ?on_activate:unit Effect.t t
    -> ?on_deactivate:unit Effect.t t
    -> ?after_display:unit Effect.t t
    -> graph
    -> unit

  val lifecycle'
    :  ?on_activate:unit Effect.t option t
    -> ?on_deactivate:unit Effect.t option t
    -> ?after_display:unit Effect.t option t
    -> graph
    -> unit

  val after_display : unit Effect.t t -> graph -> unit
  val after_display' : unit Effect.t option t -> graph -> unit
  val wait_after_display : graph -> unit Effect.t t

  module Poll : sig
    module Starting : sig
      type ('o, 'r) t

      val empty : ('o, 'o option) t
      val initial : 'o -> ('o, 'o) t
    end

    val effect_on_change
      :  ?sexp_of_input:('a -> Sexp.t)
      -> ?sexp_of_result:('o -> Sexp.t)
      -> equal_input:('a -> 'a -> bool)
      -> ?equal_result:('o -> 'o -> bool)
      -> ('o, 'r) Starting.t
      -> 'a t
      -> effect:('a -> 'o Effect.t) t
      -> graph
      -> 'r t

    val manual_refresh
      :  ?sexp_of_model:('o -> Sexp.t)
      -> ?equal:('o -> 'o -> bool)
      -> ('o, 'r) Starting.t
      -> effect:'o Effect.t t
      -> graph
      -> 'r t * unit Effect.t t
  end
end

module Memo : sig
  type 'a bonsai_t := 'a t
  type ('input, 'result) t

  val create
    :  ('input, 'cmp) comparator
    -> f:('input bonsai_t -> graph -> 'result bonsai_t)
    -> graph
    -> ('input, 'result) t bonsai_t

  val lookup
    :  ?sexp_of_model:('input -> Sexp.t)
    -> equal:('input -> 'input -> bool)
    -> ('input, 'result) t bonsai_t
    -> 'input bonsai_t
    -> graph
    -> 'result option bonsai_t
end

module Effect_throttling : sig
  module Poll_result : sig
    type 'a t =
      | Aborted
      | Finished of 'a
    [@@deriving sexp, equal]

    val collapse_to_or_error : ?tag_s:Sexp.t lazy_t -> 'a Or_error.t t -> 'a Or_error.t

    val collapse_fun_to_or_error
      :  ?sexp_of_input:('a -> Sexp.t)
      -> ('a -> 'b Or_error.t t Effect.t)
      -> 'a
      -> 'b Or_error.t Effect.t
  end

  val poll : ('a -> 'b Effect.t) t -> graph -> ('a -> 'b Poll_result.t Effect.t) t
end

module Dynamic_scope : sig
  type 'a bonsai_t := 'a t
  type 'a t

  val create : ?sexp_of:('a -> Sexp.t) -> name:string -> fallback:'a -> unit -> 'a t

  val derived
    :  ?sexp_of:('a -> Sexp.t)
    -> 'b t
    -> get:('b -> 'a)
    -> set:('b -> 'a -> 'b)
    -> 'a t

  val set : 'a t -> 'a bonsai_t -> inside:(graph -> 'r bonsai_t) -> graph -> 'r bonsai_t

  type revert = { revert : 'a. (graph -> 'a bonsai_t) -> graph -> 'a bonsai_t }

  val set'
    :  'a t
    -> 'a bonsai_t
    -> f:(revert -> graph -> 'r bonsai_t)
    -> graph
    -> 'r bonsai_t

  val lookup : 'a t -> graph -> 'a bonsai_t

  val modify
    :  'a t
    -> change:('a bonsai_t -> 'a bonsai_t)
    -> f:(revert -> graph -> 'r bonsai_t)
    -> graph
    -> 'r bonsai_t
end

module Incr : sig
  val value_cutoff : 'a t -> equal:('a -> 'a -> bool) -> graph -> 'a t
  val compute : 'a t -> f:('a Incr.t -> 'b Incr.t) -> graph -> 'b t
  val to_value : 'a Incr.t -> 'a t
  val with_clock : f:(Time_source.t -> 'a Incr.t) -> graph -> 'a t
end

val assoc
  :  ('k, 'cmp) comparator
  -> ('k, 'v, 'cmp) Map.t t
  -> f:('k t -> 'v t -> graph -> 'a t)
  -> graph
  -> ('k, 'a, 'cmp) Map.t t

val assoc_set
  :  ('key, 'cmp) comparator
  -> ('key, 'cmp) Set.t t
  -> f:('key t -> graph -> 'result t)
  -> graph
  -> ('key, 'result, 'cmp) Map.t t

val assoc_list
  :  ('key, _) comparator
  -> 'a list t
  -> get_key:('a -> 'key)
  -> f:('key t -> 'a t -> graph -> 'b t)
  -> graph
  -> [ `Duplicate_key of 'key | `Ok of 'b list ] t

module Time_source = Time_source

module Debug : sig
  val on_change : 'a t -> f:('a -> unit) -> graph -> unit
  val on_change_print_s : 'a t -> ('a -> Sexp.t) -> graph -> unit

  val instrument_computation
    :  (graph -> 'a t)
    -> start_timer:(string -> unit)
    -> stop_timer:(string -> unit)
    -> graph
    -> 'a t

  val to_dot : ?pre_process:bool -> (graph -> 'a t) -> string
  val enable_incremental_annotations : unit -> unit
  val disable_incremental_annotations : unit -> unit
end

module Path : sig
  type t = Path.t [@@deriving compare, sexp_of]

  include Comparable.S_plain with type t := t

  (** Converts the path to a "unique" string that contains only
      lowercase letters and underscores.  This makes it viable for e.g. HTML ids.

      The uniqueness of this string depends on the uniqueness of the sexp
      function for any modules that are being used in "assoc".  The
      invariant that must be upheld by those modules is the following:

      [a != b] implies [sexp_of a != sexp_of b] *)
  val to_unique_identifier_string : t -> string
end

val path_id : graph -> string t
val path : graph -> Path.t t
val arr1 : graph -> 'a t -> f:('a -> 'b) -> 'b t
val arr2 : graph -> 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

module Conv : sig
  val handle : f:(graph -> 'a t) -> graph -> 'a Computation.t
  val top_level_handle : (graph -> 'a t) -> 'a Computation.t
  val perform : ?here:Source_code_position.t -> graph -> 'a Computation.t -> 'a t
  val reveal_value : 'a t -> 'a Value.t
  val conceal_value : 'a Value.t -> 'a t
  val isolated : graph -> f:(unit -> 'a Value.t) -> 'a Computation.t
end

module Effect = Ui_effect

module Let_syntax : sig
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val return : 'a -> 'a t

  module Let_syntax : sig
    val map : ?here:Source_code_position.t -> 'a t -> f:('a -> 'b) -> 'b t
    val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
    val both : 'a t -> 'b t -> ('a * 'b) t
    val arr : ?here:Source_code_position.t -> 'a t -> f:('a -> 'b) -> 'b t
    val return : 'a t -> 'a t
    val cutoff : 'a t -> equal:('a -> 'a -> bool) -> 'a t

    val switch
      :  here:Source_code_position.t
      -> match_:int t
      -> branches:int
      -> with_:(int -> 'a t)
      -> 'a t

    val sub : ?here:_ -> 'a -> f:('a -> 'b) -> 'b

    include Mapn with type 'a t := 'a t
  end
end

module Map :
  Map0_intf.Output
    with type 'a Value.t := 'a t
     and type 'a Computation.t := graph -> 'a t
     and module Value := Value
     and module Computation := Computation

module Expert : sig
  val thunk : f:(unit -> 'a) -> graph -> 'a t

  val assoc_on
    :  ('io_key, 'io_cmp) comparator
    -> ('model_key, 'model_cmp) comparator
    -> ('io_key, 'data, 'io_cmp) Core.Map.t t
    -> get_model_key:('io_key -> 'data -> 'model_key)
    -> f:('io_key t -> 'data t -> graph -> 'result t)
    -> graph
    -> ('io_key, 'result, 'io_cmp) Core.Map.t t

  val delay : f:(graph -> 'a t) -> graph -> 'a t
    [@@deprecated "[since 2023-07] Use Bonsai.fix "]

  module Var : sig
    type 'a bonsai := 'a t

    (** A [Var.t] represents a ref to some global state, which can be used as an input to
        an incremental Bonsai computation.

        The most common use case of [Var.t]s is in tests, so that test inputs can be set
        outside of a Bonsai context. *)
    type 'a t

    (** Creates a new [Var.t] with an initial value. *)
    val create : 'a -> 'a t

    (** Provides incremental, read-only access to [t] by producing a {!Bonsai.t}. *)
    val value : 'a t -> 'a bonsai

    (** Updates the value inside of [t].  [f] is given the previous value of [t] so that you
        can reuse parts of the value if applicable. *)
    val update : 'a t -> f:('a -> 'a) -> unit

    (** Sets the value inside of [t]. *)
    val set : 'a t -> 'a -> unit

    (** Gets the value inside of [t]. *)
    val get : 'a t -> 'a

    (** Retrieves the underlying ['a t] Ui_incr.t var. *)
    val incr_var : 'a t -> 'a Ui_incr.Var.t
  end

  module For_bonsai_internal : sig
    val set_perform_on_exception : (exn -> unit) -> unit
  end
end

(** Just in subfeature to reimplement proc on top of cont *)
module For_proc2 : sig
  val arr1_with_location
    :  ?here:Source_code_position.t
    -> graph
    -> 'a t
    -> f:('a -> 'b)
    -> 'b t

  val value_cutoff : 'a t -> equal:('a -> 'a -> bool) -> 'a t
  val conceal_value : 'a Value.t -> 'a t

  val state
    :  ?reset:('model -> 'model)
    -> ?sexp_of_model:('model -> Sexp.t)
    -> ?equal:('model -> 'model -> bool)
    -> 'model
    -> graph
    -> ('model * ('model -> unit Effect.t)) t

  val state_opt
    :  ?reset:('model option -> 'model option)
    -> ?default_model:'model
    -> ?sexp_of_model:('model -> Sexp.t)
    -> ?equal:('model -> 'model -> bool)
    -> unit
    -> graph
    -> ('model option * ('model option -> unit Effect.t)) t

  val toggle : default_model:bool -> graph -> (bool * unit Effect.t) t

  module Toggle : sig
    type t =
      { state : bool
      ; set_state : bool -> unit Effect.t
      ; toggle : unit Effect.t
      }
  end

  val toggle' : default_model:bool -> graph -> Toggle.t t

  val state_machine0
    :  ?reset:('model, 'action, unit) resetter
    -> ?sexp_of_model:('model -> Sexp.t)
    -> ?sexp_of_action:('action -> Sexp.t)
    -> ?equal:('model -> 'model -> bool)
    -> default_model:'model
    -> apply_action:('action Apply_action_context.t -> 'model -> 'action -> 'model)
    -> unit
    -> graph
    -> ('model * ('action -> unit Effect.t)) t

  val state_machine1
    :  ?sexp_of_action:('action -> Sexp.t)
    -> ?reset:('model, 'action, unit) resetter
    -> ?sexp_of_model:('model -> Sexp.t)
    -> ?equal:('model -> 'model -> bool)
    -> default_model:'model
    -> apply_action:
         ('action Apply_action_context.t
          -> 'input Computation_status.t
          -> 'model
          -> 'action
          -> 'model)
    -> 'input t
    -> graph
    -> ('model * ('action -> unit Effect.t)) t

  val actor0
    :  ?reset:
         (inject:('action -> 'return Effect.t)
          -> schedule_event:(unit Effect.t -> unit)
          -> 'model
          -> 'model)
    -> ?sexp_of_model:('model -> Sexp.t)
    -> ?sexp_of_action:('action -> Sexp.t)
    -> ?equal:('model -> 'model -> bool)
    -> default_model:'model
    -> recv:
         (inject:('action -> 'return Effect.t)
          -> schedule_event:(unit Effect.t -> unit)
          -> 'model
          -> 'action
          -> 'model * 'return)
    -> unit
    -> graph
    -> ('model * ('action -> 'return Effect.t)) t

  val actor1
    :  ?sexp_of_action:('action -> Sexp.t)
    -> ?reset:
         (inject:('action -> 'return Effect.t)
          -> schedule_event:(unit Effect.t -> unit)
          -> 'model
          -> 'model)
         (** to learn more about [reset], read the docs on [with_model_resetter] *)
    -> ?sexp_of_model:('model -> Sexp.t)
    -> ?equal:('model -> 'model -> bool)
    -> default_model:'model
    -> recv:
         (inject:('action -> 'return Effect.t)
          -> schedule_event:(unit Effect.t -> unit)
          -> 'input Computation_status.t
          -> 'model
          -> 'action
          -> 'model * 'return)
    -> 'input t
    -> graph
    -> ('model * ('action -> 'return Effect.t)) t

  val wrap
    :  ?reset:('action Apply_action_context.t -> 'model -> 'model)
    -> ?sexp_of_model:('model -> Sexp.t)
    -> ?equal:('model -> 'model -> bool)
    -> default_model:'model
    -> apply_action:
         ('action Apply_action_context.t -> 'result -> 'model -> 'action -> 'model)
    -> f:('model t -> ('action -> unit Effect.t) t -> graph -> 'result t)
    -> unit
    -> graph
    -> 'result t

  val switch
    :  match_:int t
    -> branches:int
    -> with_:(int -> graph -> 'a t)
    -> graph
    -> 'a t

  val with_model_resetter : (graph -> 'a t) -> graph -> ('a * unit Effect.t) t
  val with_model_resetter' : (reset:unit Effect.t t -> graph -> 'a t) -> graph -> 'a t

  val on_change
    :  ?sexp_of_model:('a -> Sexp.t)
    -> equal:('a -> 'a -> bool)
    -> 'a t
    -> callback:('a -> unit Effect.t) t
    -> graph
    -> unit t

  val on_change'
    :  ?sexp_of_model:('a -> Sexp.t)
    -> equal:('a -> 'a -> bool)
    -> 'a t
    -> callback:('a option -> 'a -> unit Effect.t) t
    -> graph
    -> unit t

  val lifecycle
    :  ?on_activate:unit Effect.t t
    -> ?on_deactivate:unit Effect.t t
    -> ?after_display:unit Effect.t t
    -> unit
    -> graph
    -> unit t

  val lifecycle'
    :  ?on_activate:unit Effect.t option t
    -> ?on_deactivate:unit Effect.t option t
    -> ?after_display:unit Effect.t option t
    -> unit
    -> graph
    -> unit t

  val after_display : unit Effect.t t -> graph -> unit t
  val after_display' : unit Effect.t option t -> graph -> unit t

  val manual_refresh
    :  ?sexp_of_model:('o -> Sexp.t)
    -> ?equal:('o -> 'o -> bool)
    -> ('o, 'r) Edge.Poll.Starting.t
    -> effect:'o Effect.t t
    -> graph
    -> ('r * unit Effect.t) t

  val debug_on_change : 'a t -> f:('a -> unit) -> graph -> unit t
  val debug_on_change_print_s : 'a t -> ('a -> Sexp.t) -> graph -> unit t
  val lazy_ : (graph -> 'a t) lazy_t -> graph -> 'a t

  val narrow
    :  ('a * ('b -> unit Effect.t)) t
    -> get:('a -> 'c)
    -> set:('a -> 'd -> 'b)
    -> graph
    -> ('c * ('d -> unit Effect.t)) t

  val narrow_via_field
    :  ('a * ('a -> unit Effect.t)) t
    -> ('a, 'b) Field.t
    -> graph
    -> ('b * ('b -> unit Effect.t)) t
end
