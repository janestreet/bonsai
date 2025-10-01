open! Core
module Bonsai = Bonsai.Cont
open Bonsai.Let_syntax

let with_inject_fixed_point f (local_ graph) =
  let f inject (local_ graph) =
    let%sub a, b = f inject graph in
    a, b
  in
  let result = Bonsai_extra.Fixed_point.with_inject_fixed_point f graph in
  result
;;

let with_self_effect ?sexp_of_model ?equal ~f () (local_ graph) =
  Bonsai_extra.Fixed_point.with_self_effect ?sexp_of_model ?equal ~f graph
;;

let pipe ?sexp_of (local_ graph) =
  let enqueue, dequeue = Bonsai_extra.Pipe.pipe ?sexp_of graph in
  Bonsai.both enqueue dequeue
;;

let exactly_once effect (local_ graph) =
  Bonsai_extra.Effects.exactly_once effect graph;
  Bonsai.return ()
;;

let exactly_once_with_value = Bonsai_extra.Effects.exactly_once_with_value

let value_with_override ?sexp_of_model ?equal value (local_ graph) =
  let value, override =
    Bonsai_extra.Value_utilities.value_with_override ?sexp_of_model ?equal value graph
  in
  Bonsai.both value override
;;

let state_machine0_dynamic_model
  ?sexp_of_action
  ?sexp_of_model
  ?equal
  ~model
  ~apply_action
  ()
  (local_ graph)
  =
  let result, inject =
    Bonsai_extra.State_machine.state_machine0_dynamic_model
      ?sexp_of_action
      ?sexp_of_model
      ?equal
      ~model
      ~apply_action
      graph
  in
  Bonsai.both result inject
;;

let state_machine1_dynamic_model
  ?sexp_of_action
  ?sexp_of_model
  ?equal
  ~model
  ~apply_action
  input
  (local_ graph)
  =
  let result, inject =
    Bonsai_extra.State_machine.state_machine1_dynamic_model
      ?sexp_of_action
      ?sexp_of_model
      ?equal
      ~model
      ~apply_action
      input
      graph
  in
  Bonsai.both result inject
;;

let state_dynamic_model ?sexp_of_model ?equal ~model () (local_ graph) =
  let result, inject =
    Bonsai_extra.State_machine.state_dynamic_model ?sexp_of_model ?equal ~model graph
  in
  Bonsai.both result inject
;;

module Id_gen (T : Int_intf.S) () = struct
  include Bonsai_extra.Id_gen (T) ()

  let component' ?reset (local_ graph) =
    let generate, newest = component' ?reset graph in
    Bonsai.both generate newest
  ;;
end

let mirror'
  ?sexp_of_model
  ~equal
  ~store_set
  ~store_value
  ~interactive_set
  ~interactive_value
  ()
  (local_ graph)
  =
  Bonsai_extra.Mirror.mirror'
    ?sexp_of_model
    ~equal
    ~store_set
    ~store_value
    ~interactive_set
    ~interactive_value
    graph;
  Bonsai.return ()
;;

let mirror
  ?sexp_of_model
  ~equal
  ~store_set
  ~store_value
  ~interactive_set
  ~interactive_value
  ()
  (local_ graph)
  =
  Bonsai_extra.Mirror.mirror
    ?sexp_of_model
    ~equal
    ~store_set
    ~store_value
    ~interactive_set
    ~interactive_value
    graph;
  Bonsai.return ()
;;

let with_last_modified_time ~equal input (local_ graph) =
  let value, time =
    Bonsai_extra.Value_stability.with_last_modified_time ~equal input graph
  in
  Bonsai.both value time
;;

let is_stable = Bonsai_extra.Value_stability.is_stable

module Stability = Bonsai_extra.Value_stability.Stability

let value_stability = Bonsai_extra.Value_stability.value_stability

module One_at_a_time = struct
  include Bonsai_extra.One_at_a_time

  let effect send_effect (local_ graph) =
    let send, status = effect send_effect graph in
    Bonsai.both send status
  ;;
end

let bonk = Bonsai_extra.Effects.bonk
let chain_incr_effects = Bonsai_extra.Effects.chain_incr_effects
