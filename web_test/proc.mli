open! Core
open! Import

module Result_spec : sig
  include module type of struct
    include Bonsai_test.Result_spec
  end

  (** [filter_printed_attributes] controls which attributes on a Node will get
      printed analyzing the string name of the attribute. Style properties
      correspond to their property name prefixed with "style.". For example, to
      filter out the "display" CSS property, you should return false for
      "style.display"; to filter out all CSS styles, return false when the
      string begins with "style.". A Node's key corresponds to the string
      "@key" *)
  val vdom
    :  ?filter_printed_attributes:(string -> string -> bool)
    -> ?censor_paths:bool
    -> ?censor_hash:bool
    -> ?path_censoring_message:string
    -> ?hash_censoring_message:string
    -> ('a -> Vdom.Node.t)
    -> ('a, Nothing.t) t
end

module Handle : sig
  include module type of struct
    include Bonsai_test.Handle
  end

  val create
    :  ('a, 'b) Result_spec.t
    -> ?rpc_implementations:
         Async_rpc_kernel.Rpc.Connection.t Async_rpc_kernel.Rpc.Implementation.t list
    -> ?connectors:(Rpc_effect.Where_to_connect.t -> Rpc_effect.Connector.t)
    (** By default [connectors] always returns
        [Bonsai_web.Rpc_effect.Connector.test_fallback], which uses any provided
        [rpc_implementations] to handle any dispatched RPCs. *)
    -> ?start_time:Time_ns.t
    -> ?optimize:bool
    -> 'a Computation.t
    -> ('a, 'b) t

  (** Runs [recompute_view] and [Async_kernel_scheduler.yield_until_no_jobs_remain]
      in a loop until nothing remains to be done. This is a good sledgehammer
      function to use if you want to wait until all the effects of a user-action
      have completed.

      By default, this function prints "------ between bonsai frame ------" in between
      each iteration to demonstrate when side-effects occur, and how long it took for a
      stable state to be reached. This line is just extra documentation; it is not
      necessarily a sign that something is wrong (unless, of course, the behavior of the
      thing you're trying to test shouldn't result in an extra frame). These lines can be
      removed by passing [~silence_between_frames:true] in case your tests take a
      non-deterministic number of iterations to stabilize.

      [max_iterations] controls how many loop iterations are allowed before the
      function aborts with an exception, in case the default of 100 is too low.
      However, you should rarely, if ever need this parameter.
  *)
  val flush_async_and_bonsai
    :  ?max_iterations:int
    -> ?silence_between_frames:bool
    -> ('a, 'b) t
    -> unit Async_kernel.Deferred.t

  val click_on
    :  ?extra_event_fields:(string * Js_of_ocaml.Js.Unsafe.any) list
    -> ?shift_key_down:bool
    -> ?alt_key_down:bool
    -> ?ctrl_key_down:bool
    -> ('a, 'b) t
    -> get_vdom:('a -> Vdom.Node.t)
    -> selector:string
    -> unit

  val submit_form
    :  ?extra_event_fields:(string * Js_of_ocaml.Js.Unsafe.any) list
    -> ('a, 'b) t
    -> get_vdom:('a -> Vdom.Node.t)
    -> selector:string
    -> unit

  val set_checkbox
    :  ?extra_event_fields:(string * Js_of_ocaml.Js.Unsafe.any) list
    -> ?shift_key_down:bool
    -> ?alt_key_down:bool
    -> ?ctrl_key_down:bool
    -> ('a, 'b) t
    -> get_vdom:('a -> Vdom.Node.t)
    -> selector:string
    -> checked:bool
    -> unit

  val input_text
    :  ?extra_event_fields:(string * Js_of_ocaml.Js.Unsafe.any) list
    -> ('a, 'b) t
    -> get_vdom:('a -> Vdom.Node.t)
    -> selector:string
    -> text:string
    -> unit

  val keydown
    :  ?extra_event_fields:(string * Js_of_ocaml.Js.Unsafe.any) list
    -> ?shift_key_down:bool
    -> ?alt_key_down:bool
    -> ?ctrl_key_down:bool
    -> ('a, 'b) t
    -> get_vdom:('a -> Vdom.Node.t)
    -> selector:string
    -> key:Js_of_ocaml.Dom_html.Keyboard_code.t
    -> unit

  val change
    :  ?extra_event_fields:(string * Js_of_ocaml.Js.Unsafe.any) list
    -> ('a, 'b) t
    -> get_vdom:('a -> Vdom.Node.t)
    -> selector:string
    -> value:string
    -> unit

  val focus
    :  ?extra_event_fields:(string * Js_of_ocaml.Js.Unsafe.any) list
    -> ('a, 'b) t
    -> get_vdom:('a -> Vdom.Node.t)
    -> selector:string
    -> unit

  val blur
    :  ?extra_event_fields:(string * Js_of_ocaml.Js.Unsafe.any) list
    -> ?related_target:string
    -> ('a, 'b) t
    -> get_vdom:('a -> Vdom.Node.t)
    -> selector:string
    -> unit

  val mousemove
    :  ?extra_event_fields:(string * Js_of_ocaml.Js.Unsafe.any) list
    -> ('a, 'b) t
    -> get_vdom:('a -> Vdom.Node.t)
    -> selector:string
    -> unit

  val mouseenter
    :  ?extra_event_fields:(string * Js_of_ocaml.Js.Unsafe.any) list
    -> ('a, 'b) t
    -> get_vdom:('a -> Vdom.Node.t)
    -> selector:string
    -> unit

  val wheel
    :  ?extra_event_fields:(string * Js_of_ocaml.Js.Unsafe.any) list
    -> ('a, 'b) t
    -> get_vdom:('a -> Vdom.Node.t)
    -> selector:string
    -> delta_y:float
    -> unit

  val trigger_hook
    :  ('a, 'b) t
    -> get_vdom:('a -> Vdom.Node.t)
    -> selector:string
    -> name:string
    -> ('c -> unit Vdom.Effect.t) Type_equal.Id.t
    -> 'c
    -> unit

  val trigger_hook_via
    :  ('a, 'b) t
    -> get_vdom:('a -> Vdom.Node.t)
    -> selector:string
    -> name:string
    -> 't Type_equal.Id.t
    -> f:('t -> 'c -> unit Vdom.Effect.t)
    -> 'c
    -> unit

  val get_hook_value
    :  ('a, 'b) t
    -> get_vdom:('a -> Vdom.Node.t)
    -> selector:string
    -> name:string
    -> 'c Type_equal.Id.t
    -> 'c

  module Bulk_size_tracker : sig
    type change =
      { selector : string
      ; width : float
      ; height : float
      }

    val change_sizes : ('a, 'b) t -> get_vdom:('a -> Vdom.Node.t) -> change list -> unit
  end

  module Position_tracker : sig
    type change =
      { selector : string
      ; top : int
      ; left : int
      ; width : int
      ; height : int
      }

    val change_positions
      :  ('a, 'b) t
      -> get_vdom:('a -> Vdom.Node.t)
      -> change list
      -> unit
  end

  module Drag_and_drop : sig
    val run
      :  ('a, 'b) t
      -> get_vdom:('a -> Vdom.Node.t)
      -> name:string
      -> Bonsai_web_ui_drag_and_drop.For_testing.Action.t
      -> unit
  end


end
