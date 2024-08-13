module Applicative = Applicative
open! Core
include Module_types
include Annotate_incr
module Effect = Ui_effect
module Time_source = Ui_time_source

let unreachable_action : Nothing.t Action.leaf Action.t -> 'a = function
  | Leaf_dynamic _ -> .
  | Leaf_static _ -> .
;;

let unusable_apply_action ~inject:_ ~schedule_event:_ _input _model = unreachable_action
let reset_unit_model ~inject:_ ~schedule_event:_ () = ()

(* incremental nodes are pretty big; allocate these just once *)
let unit_model = Incr.return ()
let ignore_effect = Incr.return Effect.Ignore
let ( >>> ) f inject b = inject (f b)
