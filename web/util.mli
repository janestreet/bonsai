open! Core
open! Js_of_ocaml

(** [am_running_how] provides information on how the code is currently being run:
    - [`Node_test] means that the code is being run using node as part of an expect_test
    - [`Node_benchmark] means that the code is being run using node as part of a benchmark
    - [`Node] means that the code is being run using node, but not as part of an
      expect_test or a benchmark
    - [`Browser_benchmark] means that the code is being run in the browser as part of a
      benchmark
    - [`Browser] means that the code is being run in a browser but not as part of a
      benchmark
*)
val am_running_how
  : [ `Browser | `Browser_benchmark | `Node | `Node_benchmark | `Node_test ]

(** [am_within_disabled_fieldset] traverses up the DOM to see whether an event occurred
    within a fieldset element with the disabled attribute. As this function requires DOM
    interaction, it will return [false] if the code is not running in the browser.

    Note: because this function bubbles up from the target of the event, it's possible
    that the event occurs within a disabled fieldset, but the form element which performs
    this check is not within a disabled fieldset (or vice versa).
    For example, mousemove events will originate from the element under the mouse, so if
    the mouse is over a different disabled form, [am_within_disabled_fieldset] will be
    [true], even if the component which performs this check is not.
*)
val am_within_disabled_fieldset : #Dom_html.event Js.t -> bool

module For_bonsai_internal : sig
  val set_stack_overflow_exception_check : unit -> unit
end
