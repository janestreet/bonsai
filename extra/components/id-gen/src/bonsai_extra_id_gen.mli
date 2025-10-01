open! Core
module Bonsai := Bonsai.Cont

(** Id_gen builds a component which generates unique identifiers by starting at 0 and
    incrementing by one every time that the effect is called.

    The functor is parameteraized on the size of integer (int vs int63 vs int64), and it's
    also a generative functor, so invoking the functor multiple times times will mint new
    types.

    I explicitly disassociate the input T from the output T because otherwise the benefits
    of the generative functor would be gone.

    The [reset] parameter (default: [`Reset]) can be used to configure the behavior of the
    component when inside of a [with_model_resetter]:
    - [`Reset] fully resets the component, and will start generating ids starting at 0
      again
    - [`Do_nothing] makes the component ignore the reset and keep its state as-is
    - [`Bump] moves the counter up one, fully disassociating it from the previous
      generation of ids. This variant is only useful in [component'] when you'd want no
      previously generated ids to match the current id. *)
module Id_gen (T : Int_intf.S) () : sig
  include Int_intf.S

  val component
    :  ?reset:[ `Reset | `Do_nothing ]
    -> local_ Bonsai.graph
    -> t Bonsai.Effect.t Bonsai.t

  (** [component'] also gives you access to the most recently generated id *)
  val component'
    :  ?reset:[ `Reset | `Do_nothing | `Bump ]
    -> local_ Bonsai.graph
    -> t Bonsai.Effect.t Bonsai.t * t Bonsai.t
end
