open! Core
include Module_types
include Annotate_incr
module Effect = Ui_effect
module Time_source = Ui_time_source

let reset_unit_model ~inject_dynamic:_ ~inject_static:_ ~schedule_event:_ () = ()
