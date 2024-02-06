open! Core
open! Bonsai_web
open! Incr_map_collate
open! Bonsai_web_ui_partial_render_table
open! Bonsai_bench

(** An [Action.t] represents the possible actions that can be performed on a partial render
    table. *)
module Action : sig
  type ('key, 'column_id) t =
    | Unfocus
    | Focus_up
    | Focus_down
    | Focus_left
    | Focus_right
    | Page_up
    | Page_down
    | Focus of ('key * 'column_id)
  [@@deriving sexp, equal]
end

(** An [Input.t] packages up all of the inputs to the partial render table and provides
    facilities for modifying individual components. *)
module Input : sig
  type ('key, 'column_id, 'data, 'cmp) t

  (** [create] produces a [t], with defaults for most components of the input. *)
  val create
    :  ?filter:(* default: None *) (key:'key -> data:'data -> bool) option
    -> ?order:
         (* default: Compare.Unchanged *)
         ('key, 'data, 'cmp) Incr_map_collate.Compare.t
    -> ?rank_range:(* default: Which_range.To 100 *) int Collate.Which_range.t
    -> ?key_range:(* default: Which_range.All_rows *) 'key Collate.Which_range.t
    -> ?on_change:
         ((* default: Fn.const Effect.Ignore *) ('key * 'column_id) option
          -> unit Effect.t)
    -> ('key, 'data, 'cmp) Map.t
    -> ('key, 'column_id, 'data, 'cmp) t

  (** [apply_filter] produces an interaction to change the current filter. *)
  val apply_filter
    :  ('key, _, 'data, 'cmp) t
    -> (key:'key -> data:'data -> bool)
    -> 'action Bonsai_bench.Interaction.t

  (** [clear_filter] produces an interaction to remove the current filter. *)
  val clear_filter : _ t -> 'action Bonsai_bench.Interaction.t

  (** [set_map] produces an interaction to change the map whose data is being rendered in
      the table. *)
  val set_map
    :  ('key, _, 'data, 'cmp) t
    -> ('key, 'data, 'cmp) Map.t
    -> 'action Bonsai_bench.Interaction.t

  (** [set_order] produces an interaction to change the current ordering. *)
  val set_order
    :  ('key, _, 'data, 'cmp) t
    -> ('key, 'data, 'cmp) Incr_map_collate.Compare.t
    -> 'action Bonsai_bench.Interaction.t

  (** [set_rank_range] produces an interaction to change the currently visible rank range. *)
  val set_rank_range
    :  _ t
    -> int Collate.Which_range.t
    -> 'action Bonsai_bench.Interaction.t

  (** [set_on_change] produces an interaction to change the current [on_change] function. *)
  val set_on_change
    :  ('key, 'column_id, _, _) t
    -> (('key * 'column_id) option -> unit Effect.t)
    -> 'action Bonsai_bench.Interaction.t

  (** [scroll] generates an interaction with abs(start-stop) [change_input]s, which set the
      [rank_range]'s low end to the values between [start] (inclusive) and [stop]
      (exclusive), keeping [window_size] elements in the range. *)
  val scroll
    :  _ t
    -> start:int
    -> stop:int
    -> window_size:int
    -> 'action Bonsai_bench.Interaction.t
end

(** [create_bench] produces a [Bonsai_bench.t] which can be used to either benchmark or
    profile the partial render table, with the initial input set to [initial_vars], by
    running the interaction produced by calling [interaction] on [initial_vars]. *)
val create_bench
  :  ?preload_rows:int
  -> ('key, 'cmp) Bonsai.comparator
  -> initial_vars:('key, 'column_id, 'data, 'cmp) Input.t
  -> columns:('key, 'data, 'column_id) Expert.Columns.t
  -> interaction:
       (('key, 'column_id, 'data, 'cmp) Input.t
        -> ('key, 'column_id) Action.t Bonsai_bench.Interaction.t)
  -> test_name:string
  -> Bonsai_bench.t
