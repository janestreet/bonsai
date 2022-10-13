open! Core
open! Bonsai_web

type t =
  { state : Color_list.t
  ; view : Vdom.Node.t
  ; step : unit Effect.t
  ; is_done : bool
  ; is_automating : bool
  }

(* This component is responsible for the [step] UI column, and is given the "before" and
   "after" maps that it should step through. It returns the view, the current state of the
   map, and a "step" effect along with its status for the automator component. *)
val component
  :  before_state:Color_list.t Value.t
  -> after_state:Color_list.t Value.t
  -> t Computation.t
