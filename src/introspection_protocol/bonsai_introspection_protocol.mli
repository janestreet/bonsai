open! Core

(** Custom event type used for communicating between the devtool panel and a tab running a
    bonsai application. Manually triggered whenever a value is set in session storage. *)
val storage_event_name : string

module Bonsai_bug_protocol = Bonsai_bug_protocol
module Incr_node_protocol = Incr_node_protocol
module Rpc_effect_protocol = Rpc_effect_protocol
module Startup_timing_protocol = Startup_timing_protocol
