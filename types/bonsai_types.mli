open! Core_kernel
module Snapshot = Snapshot

type ('extra, 'new_model) on_action_mismatch =
  [ `Ignore
  | `Raise
  | `Warn
  | `Custom of 'extra -> 'new_model
  ]

type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked = ..

val nothing_type_id : Nothing.t Type_equal.Id.t

type ('input, 'model, 'action, 'result, 'incr, 'event) eval_type =
  input:('input, 'incr) Incremental.t
  -> old_model:('model option, 'incr) Incremental.t
  -> model:('model, 'incr) Incremental.t
  -> inject:('action -> 'event)
  -> action_type_id:'action Type_equal.Id.t
  -> incr_state:'incr Incremental.State.t
  -> ('input, 'model, 'action, 'result, 'incr, 'event) unpacked
  -> (('model, 'action, 'result, 'event) Snapshot.t, 'incr) Incremental.t

module Packed : sig
  type ('input, 'model, 'result, 'incr, 'event) t =
    | T :
        ('input, 'model, 'action, 'result, 'incr, 'event) unpacked
        * 'action Type_equal.Id.t
        -> ('input, 'model, 'result, 'incr, 'event) t
end

module Visitor : sig
  type t =
    { visit :
        'input 'model 'result 'incr 'event. ( 'input
                                            , 'model
                                            , 'result
                                            , 'incr
                                            , 'event )
          Packed.t
        -> ('input, 'model, 'result, 'incr, 'event) Packed.t
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
    :  ('input, 'model, 'result, 'incr, 'event) Packed.t
    -> Visitor.t
    -> ('input, 'model, 'result, 'incr, 'event) Packed.t
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
  -> incr_state:'incr Incremental.State.t
  -> ('input, 'model, 'action, 'result, 'incr, 'event) unpacked
  -> (('model, 'action, 'result, 'event) Snapshot.t, 'incr) Incremental.t

(** Depth-first mapping transformation over a component tree.

    The reason that visit_ext takes (and returns) Packed.t values instead
    of unpacked values is so that an optimization stage can change the type
    of an action parameter during optimization *)
val visit_ext
  :  ('input, 'model, 'result, 'incr, 'event) Packed.t
  -> Visitor.t
  -> ('input, 'model, 'result, 'incr, 'event) Packed.t

val sexp_of_unpacked : (_, _, _, _, _, _) unpacked -> Sexp.t
