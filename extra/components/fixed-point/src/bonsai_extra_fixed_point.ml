open! Core
module Bonsai = Bonsai.Cont
module Effect = Bonsai.Effect
open Bonsai.Let_syntax

let with_inject_fixed_point f (local_ graph) =
  let r, _ =
    Bonsai.wrap_n
      ~n:Two
      graph
      ~sexp_of_model:[%sexp_of: Unit.t]
      ~default_model:()
      ~equal:[%equal: Unit.t]
      ~apply_action:(fun context result () action ->
        match result with
        | Inactive ->
          let action = sexp_of_opaque action in
          eprint_s
            [%message
              "An action sent to a [wrap] has been dropped because its input was not \
               present. This happens when the [wrap] is inactive when it receives a \
               message."
                (action : Sexp.t)
                [%here]];
          ()
        | Active (_result, inject) ->
          (* speedy thing go in, speedy thing come out *)
          Bonsai.Apply_action_context.schedule_event context (inject action))
      ~f:(fun _model inject (local_ graph) -> f inject graph)
  in
  r
;;

let with_self_effect
  (type a)
  ?sexp_of_model
  ?equal
  ~(f :
      a Bonsai.Computation_status.t Effect.t Bonsai.t -> local_ Bonsai.graph -> a Bonsai.t)
  : local_ Bonsai.graph -> a Bonsai.t
  =
  fun (local_ graph) ->
  Bonsai.wrap
    graph
    ?sexp_of_model:(Option.map ~f:Option.sexp_of_t sexp_of_model)
    ?equal:(Option.map ~f:Option.equal equal)
    ~default_model:None
    ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) result model () ->
      match result with
      | Inactive ->
        let action = sexp_of_opaque () in
        eprint_s
          [%message
            "An action sent to a [wrap] has been dropped because its input was not \
             present. This happens when the [wrap] is inactive when it receives a \
             message."
              (action : Sexp.t)
              [%here]];
        model
      | Active result -> Some result)
    ~f:(fun model inject (local_ graph) ->
      let current_model =
        let get_model = Bonsai.peek model graph in
        let%arr inject and get_model in
        let%bind.Effect () = inject () in
        let%map.Effect model = get_model in
        match model with
        | Inactive
        (* Active None could happen if the model were reset in between the injection and
           get_model effects *)
        | Active None -> Bonsai.Computation_status.Inactive
        | Active (Some v) -> Active v
      in
      f current_model graph)
;;
