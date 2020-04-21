open! Core_kernel
module Snapshot = Snapshot
module Environment = Environment

type on_action_mismatch =
  [ `Ignore
  | `Raise
  | `Warn
  ]

module type Model = sig
  type t [@@deriving sexp, equal]
end

module type Action = sig
  type t [@@deriving sexp_of]
end

(** We need keys to have [t_of_sexp] and [sexp_of_t] *)
module type Comparator = sig
  type t [@@deriving sexp]

  include Comparator.S with type t := t
end

type ('k, 'cmp) comparator =
  (module Comparator with type t = 'k and type comparator_witness = 'cmp)

type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked = ..

val nothing_type_id : Nothing.t Type_equal.Id.t
val unit_type_id : unit Type_equal.Id.t

type ('input, 'model, 'action, 'result, 'incr, 'event) eval_type =
  input:('input, 'incr) Incremental.t
  -> old_model:('model option, 'incr) Incremental.t
  -> model:('model, 'incr) Incremental.t
  -> inject:('action -> 'event)
  -> action_type_id:'action Type_equal.Id.t
  -> environment:'incr Environment.t
  -> incr_state:'incr Incremental.State.t
  -> ('input, 'model, 'action, 'result, 'incr, 'event) unpacked
  -> (('model, 'action, 'result, 'event) Snapshot.t, 'incr) Incremental.t

module Packed : sig
  type 'model model_info =
    { default : 'model
    ; equal : 'model -> 'model -> bool
    ; type_id : 'model Type_equal.Id.t
    ; sexp_of : 'model -> Sexp.t
    ; of_sexp : Sexp.t -> 'model
    }

  val unit_model_info : unit model_info
  val both_model_infos : 'a model_info -> 'b model_info -> ('a * 'b) model_info

  type ('input, 'result, 'incr, 'event) t =
    | T :
        { unpacked : ('input, 'model, 'action, 'result, 'incr, 'event) unpacked
        ; action_type_id : 'action Type_equal.Id.t
        ; model : 'model model_info
        }
        -> ('input, 'result, 'incr, 'event) t
end

module Visitor : sig
  type t =
    { visit :
        'input 'result 'incr 'event. ('input, 'result, 'incr, 'event) Packed.t
        -> ('input, 'result, 'incr, 'event) Packed.t
    }
end

module type Definition = sig
  (** The easiest way to define [extension_constructor] is to use the built-in extension
      node:

      {[
        type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
          | C : foo -> (_, _, _, _, _, _) unpacked

        let extension_constructor = [%extension_constructor C]
      ]} *)
  val extension_constructor : Obj.Extension_constructor.t

  (** Only needs to handle the case(s) defined in this module. *)
  val sexp_of_unpacked : ('a, 'b, 'c, 'd, 'e, 'f) unpacked -> Sexp.t

  (** Only needs to handle the case(s) defined in this module. *)
  val eval : ('input, 'model, 'action, 'result, 'incr, 'event) eval_type

  (** An implementation of [visit] only needs to handle the cases defined in this module.

      The implementation of [visit] should call [visit_ext] on any subcomponents contained
      in the constructor.  With the new subcomponents returned by [visit_ext], the
      implementation should re-construct itself, and call [visit] on this new value. *)
  val visit
    :  ('input, 'result, 'incr, 'event) Packed.t
    -> Visitor.t
    -> ('input, 'result, 'incr, 'event) Packed.t
end

val define : (module Definition) -> unit

(** {1 Dynamic dispatch}

    The functions below dispatch on the extension constructor of the input component,
    which must have been registered with [define]. *)

val eval_ext
  :  input:('input, 'incr) Incremental.t
  -> old_model:('model option, 'incr) Incremental.t
  -> model:('model, 'incr) Incremental.t
  -> inject:('action -> 'event)
  -> action_type_id:'action Type_equal.Id.t
  -> environment:'incr Environment.t
  -> incr_state:'incr Incremental.State.t
  -> ('input, 'model, 'action, 'result, 'incr, 'event) unpacked
  -> (('model, 'action, 'result, 'event) Snapshot.t, 'incr) Incremental.t

(** Depth-first mapping transformation over a component tree.

    The reason that visit_ext takes (and returns) Packed.t values instead
    of unpacked values is so that an optimization stage can change the type
    of an action parameter during optimization *)
val visit_ext
  :  ('input, 'result, 'incr, 'event) Packed.t
  -> Visitor.t
  -> ('input, 'result, 'incr, 'event) Packed.t

val sexp_of_unpacked : (_, _, _, _, _, _) unpacked -> Sexp.t
