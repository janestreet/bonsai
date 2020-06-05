open! Core_kernel
open! Import


module type S = sig
  type input
  type action
  type extra

  val show : unit -> unit
  val show_model : unit -> unit
  val set_input : input -> unit
  val do_actions : action list -> unit
  val get_extra : unit -> extra
end

module type S_vdom = sig
  include S

  val click_on : selector:string -> unit
  val input_text : selector:string -> text:string -> unit
end

module type Helpers = sig
  module type S = S

  val make_generic
    :  driver:('input, 's) Driver.t
    -> string_of_result:('result -> string)
    -> get_result:('s -> 'result)
    -> get_extra:('s -> 'extra)
    -> schedule_action:('s -> 'action -> unit)
    -> (module S
         with type action = 'action
          and type input = 'input
          and type extra = 'extra)

  val make
    :  driver:('input, 'result) Driver.t
    -> sexp_of_result:('result -> Sexp.t)
    -> (module S
         with type input = 'input
          and type action = Nothing.t
          and type extra = unit)

  val make_with_inject
    :  driver:('input, 'result * ('action -> Vdom.Event.t)) Driver.t
    -> sexp_of_result:('result -> Sexp.t)
    -> (module S with type input = 'input and type action = 'action and type extra = unit)

  val make_string
    :  driver:('input, string) Driver.t
    -> (module S
         with type input = 'input
          and type action = Nothing.t
          and type extra = unit)

  val make_string_with_inject
    :  driver:('input, string * ('action -> Vdom.Event.t)) Driver.t
    -> (module S with type input = 'input and type action = 'action and type extra = unit)

  val make_vdom
    :  ?vdom_to_string:(Vdom.Node.t -> string)
    -> driver:('input, Vdom.Node.t) Driver.t
    -> (module S_vdom
         with type input = 'input
          and type action = Nothing.t
          and type extra = unit)

  val make_vdom_with_extra
    :  ?vdom_to_string:(Vdom.Node.t -> string)
    -> driver:('input, Vdom.Node.t * 'extra) Driver.t
    -> (module S_vdom
         with type input = 'input
          and type action = Nothing.t
          and type extra = 'extra)

  val make_vdom_with_inject
    :  ?vdom_to_string:(Vdom.Node.t -> string)
    -> driver:('input, Vdom.Node.t * ('action -> Vdom.Event.t)) Driver.t
    -> (module S_vdom
         with type input = 'input
          and type action = 'action
          and type extra = unit)
end
