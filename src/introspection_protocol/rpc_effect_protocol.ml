open! Core

module Rpc_id = struct
  include Int63

  let id = ref Int63.zero

  let create () =
    let out = !id in
    Int63.incr id;
    out
  ;;

  module For_testing = struct
    let reset_counter () = id := Int63.zero
  end
end

module Source_code_position = struct
  type t = Source_code_position.t =
    { pos_fname : string
    ; pos_lnum : int
    ; pos_bol : int
    ; pos_cnum : int
    }
  [@@deriving sexp, quickcheck, equal]
end

module Rpc_kind = struct
  module Interval = struct
    type t =
      | Poll_until_ok of { retry_interval : Time_ns.Span.t }
      | Poll_until_condition_met of { every : Time_ns.Span.t }
      | Poll of { every : Time_ns.Span.t }
      | Dispatch
    [@@deriving sexp, equal, quickcheck]
  end

  module Polling_state_rpc_interval = struct
    type t =
      | Poll of { every : Time_ns.Span.t }
      | Dispatch
    [@@deriving sexp, equal, quickcheck]
  end

  module Async_rpc_kernel_rpc_descriptions = struct
    type t = Async_rpc_kernel.Rpc.Description.t =
      { global_ name : string
      ; version : int
      }
    [@@deriving sexp, equal, quickcheck]
  end

  type t =
    | Normal of
        { name : string
        ; version : int
        ; interval : Interval.t
        }
    | Babel of
        { descriptions : Async_rpc_kernel_rpc_descriptions.t Nonempty_list.t
        ; interval : Interval.t
        }
    | Streamable of
        { name : string
        ; version : int
        ; interval : Interval.t
        }
    | Polling_state_rpc of
        { name : string
        ; version : int
        ; interval : Polling_state_rpc_interval.t
        }
    | Babel_polling_state_rpc of
        { descriptions : Async_rpc_kernel_rpc_descriptions.t Nonempty_list.t
        ; interval : Polling_state_rpc_interval.t
        }
  [@@deriving sexp, equal, quickcheck]
end

module Or_no_sexp_of_provided = struct
  type 'a t =
    | No_sexp_of_provided
    | Sexp_of_provided of 'a
  [@@deriving sexp, equal, quickcheck]
end

module Rpc_status = struct
  module Response = struct
    type t = Sexp.t Or_no_sexp_of_provided.t Or_error.t [@@deriving sexp, equal]

    let quickcheck_generator =
      let open Quickcheck.Generator in
      Quickcheck.Generator.union
        [ ([%quickcheck.generator: Sexp.t Or_no_sexp_of_provided.t] >>| fun x -> Ok x)
        ; Quickcheck.Generator.return (Error (Error.of_string "failure"))
        ]
    ;;

    let quickcheck_shrinker = Quickcheck.Shrinker.empty ()
    let quickcheck_observer = Quickcheck.Observer.singleton ()
  end

  type t =
    | Running
    | Finished of
        { duration : Time_ns.Span.t
        ; response : Response.t
        }
    | Aborted of { duration : Time_ns.Span.t }
  [@@deriving sexp, equal, quickcheck]
end

module Event = struct
  module Time = struct
    type t = Time_ns.Alternate_sexp.t [@@deriving sexp, equal]

    let quickcheck_generator = [%quickcheck.generator: Time_ns.t]
    let quickcheck_shrinker = [%quickcheck.shrinker: Time_ns.t]
    let quickcheck_observer = [%quickcheck.observer: Time_ns.t]
  end

  module V1 = struct
    type t =
      | Started of
          { id : Rpc_id.t
          ; rpc_kind : Rpc_kind.t
          ; start_time : Time.t
          ; query : Sexp.t Or_no_sexp_of_provided.t
               [@default No_sexp_of_provided] [@sexp_drop_default.equal]
          ; path : string
          ; here : Source_code_position.t option [@sexp.option]
          } [@sexp.allow_extra_fields]
      | Finished of
          { id : Rpc_id.t
          ; duration : Time_ns.Span.t
          ; response : Rpc_status.Response.t
               [@default Ok No_sexp_of_provided] [@sexp_drop_default.equal]
          } [@sexp.allow_extra_fields]
      | Aborted of
          { id : Rpc_id.t
          ; duration : Time_ns.Span.t
          } [@sexp.allow_extra_fields]
      | Response_size of
          { id : Rpc_id.t
          ; payload_bytes : int
          }
    [@@deriving sexp, quickcheck]
  end

  module Stable = struct
    (* NOTE: We are mostly relying on version upgrading being safe to up-convert via
       sexp's. For last-resort protocol upgrades we can use this variant mechanism for the
       upgrades. *)
    type nonrec t = V1 of V1.t [@@deriving sexp, quickcheck]

    let to_latest (V1 v1) = v1
    let of_latest v1 = V1 v1
  end

  type t = V1.t
end

module Rpc_state = struct
  type t =
    { rpc_kind : Rpc_kind.t
    ; start_time : Time_ns.Alternate_sexp.t
    ; query : Sexp.t Or_no_sexp_of_provided.t
    ; status : Rpc_status.t
    ; path : string
    ; response_size : int option [@sexp.option]
    ; here : Source_code_position.t option [@sexp.option]
    }
  [@@deriving sexp, equal]
end

module State = struct
  type t = Rpc_state.t Rpc_id.Map.t [@@deriving sexp, equal]

  let empty = Rpc_id.Map.empty

  let assert_is_running
    (rpc_state : Rpc_state.t)
    ~rpc_id
    ~event_kind
    ~on_is_running
    ~original
    =
    let on_failure ~adjective =
      print_endline
        [%string
          {|Saw %{event_kind} event for already %{adjective} event. Ignoring new event. (id %{rpc_id#Rpc_id})|}]
    in
    match rpc_state.status with
    | Running -> on_is_running ()
    | Finished _ ->
      on_failure ~adjective:"finished";
      original
    | Aborted _ ->
      on_failure ~adjective:"aborted";
      original
  ;;

  let apply_event (state : t) (event : Event.t) : t =
    match event with
    | Started { id; rpc_kind; start_time; query; path; here } ->
      let rpc_state =
        { Rpc_state.status = Running
        ; rpc_kind
        ; start_time
        ; query
        ; path
        ; response_size = None
        ; here
        }
      in
      (match Map.add state ~key:id ~data:rpc_state with
       | `Ok state -> state
       | `Duplicate ->
         print_s
           [%message
             "Saw started message with duplicate ids, ignoring message."
               (id : Rpc_id.t)
               (rpc_state : Rpc_state.t)];
         state)
    | Finished { id; duration; response } ->
      (match Map.find state id with
       | None ->
         print_s
           [%message
             "Saw finished message for unknown rpc id, ignoring finish message."
               (id : Rpc_id.t)
               (duration : Time_ns.Span.t)
               (response : Rpc_status.Response.t)];
         state
       | Some rpc_state ->
         assert_is_running
           rpc_state
           ~event_kind:"finish"
           ~rpc_id:id
           ~original:state
           ~on_is_running:(fun () ->
             Map.set
               state
               ~key:id
               ~data:{ rpc_state with status = Finished { duration; response } }))
    | Aborted { id; duration } ->
      (match Map.find state id with
       | None ->
         print_s
           [%message
             "Saw abort message for unknown rpc id, ignoring abort message."
               (id : Rpc_id.t)
               (duration : Time_ns.Span.t)];
         state
       | Some rpc_state ->
         assert_is_running
           rpc_state
           ~event_kind:"abort"
           ~rpc_id:id
           ~original:state
           ~on_is_running:(fun () ->
             Map.set state ~key:id ~data:{ rpc_state with status = Aborted { duration } }))
    | Response_size { id; payload_bytes } ->
      (match Map.find state id with
       | None ->
         print_s
           [%message
             "Saw tracing_event message for unknown rpc id, ignoring message."
               (id : Rpc_id.t)
               (payload_bytes : int)];
         state
       | Some rpc_state ->
         let current_response_size = Option.value rpc_state.response_size ~default:0 in
         Map.set
           state
           ~key:id
           ~data:
             { rpc_state with
               response_size = Some (current_response_size + payload_bytes)
             })
  ;;
end

module For_testing = struct
  module Rpc_id = struct
    include Rpc_id

    let of_int = Int63.of_int
  end

  module Event = struct
    include Event.V1

    let conceal = Fn.id
    let reveal = Fn.id

    module Unstable = Event.V1
  end

  let reset_ids_for_testing = Rpc_id.For_testing.reset_counter
end
