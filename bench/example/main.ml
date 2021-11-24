open! Core
open! Bonsai
open! Bonsai.Let_syntax
open! Bonsai_bench

let const =
  Test.create
    ~name:"Bonsai.const"
    ~component:(Bonsai.const 4)
    ~get_inject:(fun _ -> Nothing.unreachable_code)
    (Interaction.many [])
;;

let state =
  Test.create
    ~name:"Bonsai.state"
    ~component:(Bonsai.state [%here] (module Int) ~default_model:0)
    ~get_inject:(fun (_, inject) -> inject)
    Interaction.(many_with_stabilizations [ inject 1; reset_model ])
;;

module State_machine = struct
  module Action = struct
    type t =
      | Incr
      | Decr
    [@@deriving sexp, equal]
  end

  let component =
    Bonsai.state_machine0
      [%here]
      (module Int)
      (module Action)
      ~default_model:0
      ~apply_action:(fun ~inject:_ ~schedule_event:_ model action ->
        match action with
        | Incr -> model + 1
        | Decr -> model - 1)
  ;;

  let incr = Interaction.inject Action.Incr
  let decr = Interaction.inject Action.Decr
end

(* The state does not get reset by default between benchmark runs. We choose
   an interaction which is idempotent so each test run will be identical. *)
let state_machine_idempotent =
  [ State_machine.incr; State_machine.decr; State_machine.incr; State_machine.decr ]
  |> Interaction.many_with_stabilizations
  |> Test.create
       ~name:"Bonsai.state_machine0: idempotent"
       ~component:State_machine.component
       ~get_inject:(fun (_, inject) -> inject)
;;

(* This state machine benchmark does not have an idempotent interaction. As a result, this
   test will cause the model to grow by 2 every time the test is run during benchmarking.
   Since (number of test runs * 2) won't exceed the range of int and the performance of
   incrementing/decrementing by 1 is ~identical between int values, this won't cause
   skewed benchmark results. *)
let state_machine_without_reset =
  [ State_machine.incr; State_machine.decr; State_machine.incr; State_machine.incr ]
  |> Interaction.many_with_stabilizations
  |> Test.create
       ~name:"Bonsai.state_machine0: not idempotent; no model reset"
       ~component:State_machine.component
       ~get_inject:(fun (_, inject) -> inject)
;;

(* If it was important that the state machine restarted at 0 every time, then we could use
   [create_with_resetter] to explicitly reset the model. This comes with an overhead cost,
   though, as we must reset the model and perform a stabilization. *)
let state_machine_with_reset =
  [ State_machine.incr; State_machine.decr; State_machine.incr; State_machine.incr ]
  |> Interaction.many_with_stabilizations
  |> Test.create_with_resetter
       ~name:
         "Bonsai.state_machine0: not idempotent; model reset using create_with_resetter"
       ~component:State_machine.component
       ~get_inject:(fun (_, inject) -> inject)
;;

(* We can also manually reset the component's model. The benchmark above is equivalent to
   this one. *)
let state_machine_with_manual_reset =
  [ State_machine.incr
  ; State_machine.decr
  ; State_machine.incr
  ; State_machine.incr
  ; Interaction.reset_model
  ; Interaction.stabilize
  ]
  |> Interaction.many_with_stabilizations
  |> Test.create_with_resetter
       ~name:"Bonsai.state_machine0: not idempotent; model reset manually"
       ~component:State_machine.component
       ~get_inject:(fun (_, inject) -> inject)
;;

module My_triple = struct
  let component first second third =
    let%arr first = first
    and second = second
    and third = third in
    first, second, third
  ;;
end

(* This benchmark calls stabilize in between setting each of the components in the
   [My_triple] component. *)
let piecewise_triple_stabilize_between_each =
  let first = Var.create 0 in
  let second = Var.create "" in
  let third = Var.create 0. in
  Interaction.
    [ change_input first 1
    ; change_input second "second"
    ; change_input third 3.
    ; change_input first 0
    ; change_input second ""
    ; change_input third 0.
    ]
  |> Interaction.many_with_stabilizations
  |> Test.create
       ~name:"My_triple setting components and stabilizing between each one"
       ~component:
         (My_triple.component (Var.value first) (Var.value second) (Var.value third))
       ~get_inject:(fun _ -> Nothing.unreachable_code)
;;

(* If we wanted to ensure stabilization only happened after all of the inputs were set,
   we could do the following. Since [many_with_stabilizations] just intersperses
   [stabilize]s in the list of interactions, stabilization is only inserted between the
   two [many] groups below. *)
let piecewise_triple_stabilize_after_all =
  let first = Var.create 0 in
  let second = Var.create "" in
  let third = Var.create 0. in
  Interaction.
    [ many [ change_input first 1; change_input second "second"; change_input third 3. ]
    ; many [ change_input first 0; change_input second ""; change_input third 0. ]
    ]
  |> Interaction.many_with_stabilizations
  |> Test.create
       ~name:"My_triple setting components and stabilizing after all three"
       ~component:
         (My_triple.component (Var.value first) (Var.value second) (Var.value third))
       ~get_inject:(fun _ -> Nothing.unreachable_code)
;;

module Do_work_every_second = struct
  let component =
    Bonsai.Incr.with_clock (fun clock ->
      let%map.Ui_incr () = Ui_incr.Clock.at_intervals clock (Time_ns.Span.of_sec 1.0) in
      for _ = 1 to 1000 do
        ()
      done)
  ;;

  let advance_by_zero = Interaction.advance_clock_by (Time_ns.Span.of_sec 0.)
  let advance_by_second = Interaction.advance_clock_by (Time_ns.Span.of_sec 1.0)
end

let do_work_every_second_advance_by_zero =
  Test.create
    ~name:"Component that does work every second: advance clock by zero each run"
    ~component:Do_work_every_second.component
    ~get_inject:(fun _ -> Nothing.unreachable_code)
    Do_work_every_second.advance_by_zero
;;

let do_work_every_second_advance_by_second =
  Test.create
    ~name:"Component that does work every second: advance clock by a second each run"
    ~component:Do_work_every_second.component
    ~get_inject:(fun _ -> Nothing.unreachable_code)
    Do_work_every_second.advance_by_second
;;

let two_state_machines_that_alternate =
  let component =
    let%map.Computation which, set_which =
      Bonsai.state [%here] (module Bool) ~default_model:true
    and state_1, inject_1 = State_machine.component
    and state_2, inject_2 = State_machine.component in
    let inject action =
      match which with
      | true -> Effect.Many [ set_which false; inject_1 action ]
      | false -> Effect.Many [ set_which true; inject_2 action ]
    in
    (state_1, state_2), inject
  in
  [ State_machine.incr; State_machine.incr; State_machine.decr; State_machine.decr ]
  |> Interaction.many_with_stabilizations
  |> Test.create
       ~name:"Alternating state machines"
       ~component
       ~get_inject:(fun (_, inject) -> inject)
;;

let () =
  let quota = Core_bench_js.Quota.Span (Time.Span.of_sec 1.0) in
  Bonsai_bench.bench
    ~run_config:(Core_bench_js.Run_config.create () ~quota)
    [ const
    ; state
    ; state_machine_idempotent
    ; state_machine_without_reset
    ; state_machine_with_reset
    ; state_machine_with_manual_reset
    ; piecewise_triple_stabilize_after_all
    ; piecewise_triple_stabilize_between_each
    ; do_work_every_second_advance_by_zero
    ; do_work_every_second_advance_by_second
    ; two_state_machines_that_alternate
    ]
;;
