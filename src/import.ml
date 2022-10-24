open! Core
include Module_types
include Annotate_incr
module Effect = Ui_effect

let reset_unit_model ~inject_dynamic:_ ~inject_static:_ ~schedule_event:_ () = ()
