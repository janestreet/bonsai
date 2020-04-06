open! Core_kernel
open! Import

module type S = sig
  type input
  type action

  val show : unit -> unit
  val set_input : input -> unit
  val do_actions : action list -> unit
end

module type Helpers = sig
  module type S = S

  val make
    :  driver:('input, 'result) Driver.t
    -> sexp_of_result:('result -> Sexp.t)
    -> (module S with type input = 'input and type action = Nothing.t)

  val make_with_inject
    :  driver:('input, 'result * ('action -> Event.t)) Driver.t
    -> sexp_of_result:('result -> Sexp.t)
    -> (module S with type input = 'input and type action = 'action)

  val make_string
    :  driver:('input, string) Driver.t
    -> (module S with type input = 'input and type action = Nothing.t)

  val make_string_with_inject
    :  driver:('input, string * ('action -> Event.t)) Driver.t
    -> (module S with type input = 'input and type action = 'action)
end
