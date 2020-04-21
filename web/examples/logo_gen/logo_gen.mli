open! Core_kernel
open Bonsai_web
open Virtual_dom_svg

val circle
  :  r1:float
  -> r2:float
  -> gap1:float
  -> gap2:float
  -> rounding:float
  -> Vdom.Node.t * Attr.path_op list
