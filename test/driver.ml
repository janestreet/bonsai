open! Core_kernel
open! Import
open Bonsai_generic_test

module T = struct
  module Event = Event

  type 'a t =
    { action_type_id : 'a Type_equal.Id.t
    ; queue : Event.t Queue.t
    }

  let make_state ~action_type_id = { queue = Queue.create (); action_type_id }
  let schedule_event t = Queue.enqueue t.queue

  let iter_actions (type a) (t : a t) ~(f : a -> unit) =
    let rec process_event = function
      | Event.Packed (a, a_typ_id) ->
        let T = Type_equal.Id.same_witness_exn a_typ_id t.action_type_id in
        f a
      | External_event s -> printf "External event: %s\n" s
      | No_op -> ()
      | Sequence list -> List.iter list ~f:process_event
    in
    while not (Queue.is_empty t.queue) do
      process_event (Queue.dequeue_exn t.queue)
    done
  ;;

  let inject _state (Packed_action.T (a, type_id)) = Event.pack type_id a
end

include Driver.Make (Bonsai) (T)
