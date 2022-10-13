open! Core
open Bonsai_web

(* Create a circular SVG gauge. *)
val create
  :  ?percent_to_color:(Percent.t -> [< Css_gen.Color.t ])
  -> radius:float
  -> Percent.t
  -> Vdom.Node.t
