open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax
module Lib = Bonsai_guide_code_lib.Time_examples

let () = Util.run Lib.current_time ~id:"clock_now"
let () = Util.run Lib.approx_current_time ~id:"clock_approx_now"
let () = Util.run Lib.measure_time ~id:"current_time_effect"
let () = Util.run Lib.clock_sleep_demo ~id:"clock_sleep"
let () = Util.run Lib.clock_every_demo ~id:"clock_every"
