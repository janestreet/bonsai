open! Core
open! Import
include module type of Ui_effect with type 'a t = 'a Ui_effect.t

val of_event : unit Ui_effect.t -> unit t
