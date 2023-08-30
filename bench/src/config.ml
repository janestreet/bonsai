open! Core
open! Bonsai

type ('a, 'r) unpacked =
  { clock : Bonsai.Time_source.t
  ; name : string
  ; component : 'r Computation.t
  ; get_inject : 'r -> 'a -> unit Effect.t
  ; interaction : 'a Interaction.t
  }

type t = T : (_, _) unpacked -> t

let create
  ?(clock = Bonsai.Time_source.create ~start:Time_ns.epoch)
  ~name
  ~component
  ~get_inject
  interaction
  =
  T { clock; name; component; get_inject; interaction }
;;

let create_with_resetter
  ?(clock = Bonsai.Time_source.create ~start:Time_ns.epoch)
  ~name
  ~component
  ~get_inject
  interaction
  =
  T
    { clock
    ; name
    ; component
    ; get_inject
    ; interaction =
        [ interaction; Interaction.Reset_model; Interaction.Stabilize ]
        |> Interaction.many
    }
;;
