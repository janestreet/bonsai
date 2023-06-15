open! Core
open! Import

(* We need to fake the source-code position because this test is run in
   two files with different names

   In particular, we can't just use [print_s ~hide_positions:true] because that
   only hides line and column numbers, but includes the file name. *)
let dummy_source_code_position =
  Source_code_position.
    { pos_fname = "file_name.ml"; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 }
;;

let run_test ~(component : _ Bonsai.Arrow_deprecated.t) ~initial_input ~f =
  let clock = Bonsai.Time_source.create ~start:(Time_ns.now ()) in
  let driver component = Driver.create component ~initial_input ~clock in
  f (driver component)
;;

module Helpers = struct
  include Helpers

  let make ~driver = Helpers.make ~driver
  let make_string ~driver = Helpers.make_string ~driver
  let make_string_with_inject ~driver = Helpers.make_string_with_inject ~driver
  let make_with_inject ~driver = Helpers.make_with_inject ~driver
end

module Counter_component = struct
  module Input = Unit
  module Model = Int

  module Action = struct
    type t =
      | Increment
      | Decrement
    [@@deriving sexp_of]
  end

  module Result = struct
    type t = string * (Action.t -> unit Effect.t)
  end

  let apply_action _ () model : Action.t -> Model.t = function
    | Increment -> model + 1
    | Decrement -> model - 1
  ;;

  let compute ~inject () m = Int.to_string m, inject
  let name = "counter-component"
end

let%expect_test "enum" =
  let open Bonsai.Arrow_deprecated.Infix in
  let component =
    Bonsai.Arrow_deprecated.enum
      (module Bool)
      ~which:Tuple2.get1
      ~handle:(function
        | true -> Tuple2.get2 @>> Bonsai.Arrow_deprecated.pure ~f:(sprintf "true %d")
        | false -> Tuple2.get2 @>> Bonsai.Arrow_deprecated.pure ~f:(sprintf "false %d"))
  in
  run_test ~component ~initial_input:(true, 5) ~f:(fun driver ->
    [%expect {| |}];
    let (module H) = Helpers.make_string ~driver in
    H.show ();
    [%expect {| true 5 |}];
    H.set_input (true, 10);
    [%expect {| true 10 |}];
    H.set_input (false, 10);
    [%expect {| false 10 |}];
    H.set_input (false, 5);
    [%expect {| false 5 |}])
;;

let%expect_test "enum with action handling `Warn" =
  let open Bonsai.Arrow_deprecated.Infix in
  let module Action = struct
    type t =
      | Outer of Counter_component.Action.t
      | Inner of Counter_component.Action.t
  end
  in
  let component =
    let%map.Bonsai.Arrow_deprecated (result, inject_inner), inject_outer =
      Bonsai.Arrow_deprecated.of_module
        (module Counter_component)
        ~sexp_of_model:[%sexp_of: Counter_component.Model.t]
        ~equal:[%equal: Int.t]
        ~default_model:1
      >>> Bonsai.Arrow_deprecated.first
            (Bonsai.Arrow_deprecated.enum
               (module Bool)
               ~which:(fun digit -> Int.of_string digit mod 3 = 0)
               ~handle:(function
                 | false ->
                   Fn.ignore
                   @>> Bonsai.Arrow_deprecated.of_module
                         (module Counter_component)
                         ~sexp_of_model:[%sexp_of: Counter_component.Model.t]
                         ~equal:[%equal: Counter_component.Model.t]
                         ~default_model:0
                   >>| Tuple2.map_fst ~f:(sprintf "counter %s")
                 | true ->
                   Bonsai.Arrow_deprecated.pure ~f:(fun s ->
                     let view = sprintf "pure %s" s in
                     let inj _ = failwith "can't raise actions out of this one" in
                     view, inj)))
    in
    ( result
    , function
      | Action.Outer a -> inject_outer a
      | Inner a -> inject_inner a )
  in
  run_test ~component ~initial_input:() ~f:(fun driver ->
    [%expect {| |}];
    let (module H) = Helpers.make_string_with_inject ~driver in
    H.show ();
    [%expect "counter 0"];
    H.do_actions [ Inner Increment ];
    [%expect "counter 1"];
    H.do_actions [ Inner Increment ];
    [%expect "counter 2"];
    H.do_actions [ Outer Increment ];
    [%expect "counter 2"];
    H.do_actions
      [ Outer Increment
      (* The inner action is ignored.  You can see this because it prints "counter 2"
         when it gets focus again. *)
      ; Inner Increment
      ];
    [%expect
      {|
               (lib/bonsai/src/proc.ml:92:14
                "An action sent to an [of_module1] has been dropped because its input was not present. This happens when the [of_module1] is inactive when it receives a message."
                (action Increment))
               pure 3|}];
    H.do_actions [ Outer Increment ];
    [%expect "counter 2"])
;;

let%expect_test "constant component" =
  run_test
    ~component:(Bonsai.Arrow_deprecated.const "some constant value")
    ~initial_input:()
    ~f:(fun driver ->
      [%expect {| |}];
      let (module H) = Helpers.make_string ~driver in
      H.show ();
      [%expect {| some constant value |}])
;;

let%expect_test "module component" =
  run_test
    ~component:
      (Bonsai.Arrow_deprecated.of_module
         (module Counter_component)
         ~sexp_of_model:[%sexp_of: Counter_component.Model.t]
         ~default_model:0
         ~equal:[%equal: Counter_component.Model.t])
    ~initial_input:()
    ~f:(fun driver ->
      [%expect {| |}];
      let (module H) = Helpers.make_string_with_inject ~driver in
      H.show ();
      [%expect "0"];
      H.do_actions [ Increment ];
      [%expect "1"];
      H.do_actions [ Decrement ];
      [%expect "0"];
      (* Increment and decrement in the same cycle should cancel out *)
      H.do_actions [ Increment; Decrement ];
      [%expect "0"])
;;

let%expect_test "state-machine counter-component" =
  let component =
    let%map.Bonsai.Arrow_deprecated model, inject =
      Bonsai.Arrow_deprecated.state_machine
        ~sexp_of_model:[%sexp_of: Counter_component.Model.t]
        ~sexp_of_action:[%sexp_of: Counter_component.Action.t]
        ~equal:[%equal: Counter_component.Model.t]
        dummy_source_code_position
        ~default_model:0
        ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) () model -> function
          | Increment -> model + 1
          | Decrement -> model - 1)
    in
    Int.to_string model, inject
  in
  run_test ~component ~initial_input:() ~f:(fun driver ->
    [%expect {| |}];
    let (module H) = Helpers.make_string_with_inject ~driver in
    H.show ();
    [%expect "0"];
    H.do_actions [ Increment ];
    [%expect "1"];
    H.do_actions [ Decrement ];
    [%expect "0"];
    (* Increment and decrement in the same cycle should cancel out *)
    H.do_actions [ Increment; Decrement ];
    [%expect "0"])
;;

let%expect_test "basic Same_model let syntax" =
  let open Bonsai.Arrow_deprecated.Let_syntax in
  let counter_component =
    Bonsai.Arrow_deprecated.of_module
      (module Counter_component)
      ~sexp_of_model:[%sexp_of: Counter_component.Model.t]
      ~default_model:0
      ~equal:[%equal: Counter_component.Model.t]
  in
  let component =
    let%map a_side = Bonsai.Arrow_deprecated.const 5
    and b_side, inject_b = counter_component in
    sprintf "%d | %s" a_side b_side, inject_b
  in
  run_test ~component ~initial_input:() ~f:(fun driver ->
    [%expect {| |}];
    let (module H) = Helpers.make_string_with_inject ~driver in
    H.show ();
    [%expect {| 5 | 0 |}];
    H.do_actions [ Increment ];
    [%expect {| 5 | 1 |}];
    H.do_actions [ Decrement ];
    [%expect {| 5 | 0 |}])
;;

let%expect_test "module project field" =
  let open Bonsai.Arrow_deprecated.Let_syntax in
  let module _ = struct
    type t =
      { a : int
      ; b : int
      }
    [@@deriving fields]
  end
  in
  let counter_component =
    Bonsai.Arrow_deprecated.of_module
      (module Counter_component)
      ~sexp_of_model:[%sexp_of: Counter_component.Model.t]
      ~default_model:0
      ~equal:[%equal: Counter_component.Model.t]
  in
  let component =
    let%map a_side, inject_a = counter_component
    and b_side, inject_b = counter_component in
    ( sprintf "%s | %s" a_side b_side
    , function
      | First a -> inject_a a
      | Second b -> inject_b b )
  in
  run_test ~component ~initial_input:() ~f:(fun driver ->
    [%expect {| |}];
    let (module H) = Helpers.make_string_with_inject ~driver in
    H.show ();
    [%expect {| 0 | 0 |}];
    H.do_actions [ First Increment ];
    [%expect {| 1 | 0 |}];
    H.do_actions [ Second Decrement ];
    [%expect {| 1 | -1 |}])
;;

let%expect_test "incremental fn constructor" =
  let component =
    Bonsai.Arrow_deprecated.With_incr.pure
      ~f:
        (Incr_map.mapi ~f:(fun ~key:_ ~data ->
           print_endline "doing math";
           data + 1))
  in
  let initial_input = [ 0, 0; 1, 1; 2, 2 ] |> Int.Map.of_alist_exn in
  run_test ~component ~initial_input ~f:(fun driver ->
    [%expect {|
      doing math
      doing math
      doing math |}];
    let (module H) = Helpers.make ~driver ~sexp_of_result:[%sexp_of: int Int.Map.t] in
    H.show ();
    [%expect {|
      ((0 1)
       (1 2)
       (2 3)) |}];
    H.set_input (Map.add_exn (initial_input : _ Int.Map.t) ~key:3 ~data:3);
    [%expect {|
      doing math
      ((0 1)
       (1 2)
       (2 3)
       (3 4)) |}])
;;

let%expect_test "schedule event from outside of the component" =
  let module Raises_something_from_without = struct
    module Input = Unit
    module Model = Unit

    module Action = struct
      type t = Trigger [@@deriving sexp_of]
    end

    module Result = struct
      type t = unit * (Action.t -> unit Effect.t)
    end

    let apply_action context () () Action.Trigger =
      Bonsai.Apply_action_context.schedule_event context (Effect.external_ "hello world")
    ;;

    let compute ~inject () () = (), inject
    let name = "raises-something-from-without"
  end
  in
  let component =
    Bonsai.Arrow_deprecated.of_module
      (module Raises_something_from_without)
      ~sexp_of_model:[%sexp_of: Raises_something_from_without.Model.t]
      ~equal:[%equal: Raises_something_from_without.Model.t]
      ~default_model:()
  in
  run_test ~component ~initial_input:() ~f:(fun driver ->
    [%expect {| |}];
    [%expect {||}];
    let (module H) = Helpers.make_with_inject ~driver ~sexp_of_result:[%sexp_of: unit] in
    H.do_actions [ Trigger ];
    [%expect {|
      External event: hello world
      () |}])
;;

let%expect_test "schedule many events from outside of the component" =
  let module Raises_something_from_without = struct
    module Input = Unit

    module Action = struct
      type t = Trigger [@@deriving sexp_of]
    end

    module Model = Unit

    module Result = struct
      type t = unit * (Action.t -> unit Effect.t)
    end

    let apply_action context () () Action.Trigger =
      Bonsai.Apply_action_context.schedule_event
        context
        (Effect.sequence
           [ Effect.external_ "hello world"
           ; Effect.no_op
           ; Effect.external_ "goodbye world"
           ])
    ;;

    let compute ~inject () () = (), inject
    let name = "raises-something-from-without"
  end
  in
  let component =
    Bonsai.Arrow_deprecated.of_module
      (module Raises_something_from_without)
      ~sexp_of_model:[%sexp_of: Raises_something_from_without.Model.t]
      ~equal:[%equal: Raises_something_from_without.Model.t]
      ~default_model:()
  in
  run_test ~component ~initial_input:() ~f:(fun driver ->
    [%expect {| |}];
    let (module H) = Helpers.make_with_inject ~driver ~sexp_of_result:[%sexp_of: unit] in
    H.do_actions [ Trigger ];
    [%expect
      {|
      External event: hello world
      External event: goodbye world
      () |}])
;;

let%expect_test "value cutoff" =
  let open Bonsai.Arrow_deprecated.Infix in
  let cutoff =
    Incr.Cutoff.create (fun ~old_value ~new_value -> old_value % 2 = new_value % 2)
  in
  let component =
    Bonsai.Arrow_deprecated.With_incr.value_cutoff ~cutoff
    >>> Bonsai.Arrow_deprecated.pure ~f:Int.to_string
  in
  run_test ~component ~initial_input:1 ~f:(fun driver ->
    [%expect {| |}];
    let (module H) = Helpers.make_string ~driver in
    H.show ();
    [%expect "1"];
    H.set_input 5;
    [%expect "1"];
    H.set_input 6;
    [%expect "6"];
    H.set_input 2;
    [%expect "6"])
;;

let%expect_test "input" =
  let module Words_counter_component = struct
    module Input = struct
      type t = string list
    end

    module Model = Int

    module Action = struct
      type t = Increment [@@deriving sexp_of]
    end

    module Result = struct
      type t = (int * string list) * (Action.t -> unit Effect.t)
    end

    let apply_action (_ : _ Bonsai.Apply_action_context.t) _words model
      : Action.t -> Model.t
      = function
        | Increment -> model + 1
    ;;

    let compute ~inject words m =
      let res = m, List.filter words ~f:(fun s -> String.length s = m) in
      res, inject
    ;;

    let name = "words-counter-component"
  end
  in
  let component =
    Bonsai.Arrow_deprecated.of_module
      (module Words_counter_component)
      ~sexp_of_model:[%sexp_of: Words_counter_component.Model.t]
      ~default_model:0
      ~equal:[%equal: Words_counter_component.Model.t]
  in
  let initial_input = [] in
  run_test ~component ~initial_input ~f:(fun driver ->
    [%expect {| |}];
    let (module H) =
      Helpers.make_with_inject ~driver ~sexp_of_result:[%sexp_of: int * string list]
    in
    H.show ();
    [%expect {| (0 ()) |}];
    H.set_input [ "a"; "b"; "c"; "aa"; "bbb"; "cccc" ];
    [%expect {| (0 ()) |}];
    H.do_actions [ Words_counter_component.Action.Increment ];
    [%expect {| (1 (a b c)) |}];
    H.do_actions [ Words_counter_component.Action.Increment ];
    [%expect {| (2 (aa)) |}];
    H.do_actions [ Words_counter_component.Action.Increment ];
    [%expect {| (3 (bbb)) |}];
    H.do_actions [ Words_counter_component.Action.Increment ];
    [%expect {| (4 (cccc)) |}];
    H.set_input [ "aaaa"; "bbbb" ];
    [%expect {| (4 (aaaa bbbb)) |}])
;;

let%expect_test "compose, pure" =
  let open Bonsai.Arrow_deprecated.Infix in
  let component_a = Bonsai.Arrow_deprecated.pure ~f:(fun model -> model mod 5) in
  let component_b = Bonsai.Arrow_deprecated.pure ~f:(fun input -> input + 2) in
  let component = component_a >>> component_b in
  run_test ~component ~initial_input:0 ~f:(fun driver ->
    [%expect {| |}];
    let (module H) = Helpers.make ~driver ~sexp_of_result:[%sexp_of: int] in
    H.show ();
    [%expect "2"];
    H.set_input 11;
    [%expect "3"])
;;

let%expect_test "pure_incr" =
  let open Bonsai.Arrow_deprecated.Infix in
  let component_a = Bonsai.Arrow_deprecated.pure ~f:(fun model -> model mod 5) in
  let component_b =
    Bonsai.Arrow_deprecated.With_incr.pure ~f:(fun input ->
      Incr.map input ~f:(fun i -> i + 2))
  in
  let component = component_a >>> component_b in
  run_test ~component ~initial_input:0 ~f:(fun driver ->
    [%expect {| |}];
    let (module H) = Helpers.make ~driver ~sexp_of_result:[%sexp_of: int] in
    H.show ();
    [%expect "2"];
    H.set_input 11;
    [%expect "3"])
;;

let%expect_test "input projection" =
  let open Bonsai.Arrow_deprecated.Infix in
  let component =
    String.length @>> Bonsai.Arrow_deprecated.pure ~f:(fun input -> input + 1)
  in
  run_test ~component ~initial_input:"hi" ~f:(fun driver ->
    [%expect {| |}];
    let (module H) = Helpers.make ~driver ~sexp_of_result:[%sexp_of: int] in
    H.show ();
    [%expect "3"];
    H.set_input "hello";
    [%expect "6"])
;;

let%expect_test "assoc on input" =
  let component =
    Bonsai.Arrow_deprecated.pure ~f:(fun x -> x + 1)
    |> Bonsai.Arrow_deprecated.Map.assoc_input (module String)
  in
  run_test
    ~component
    ~initial_input:(String.Map.of_alist_exn [ "a", 0; "b", 2 ])
    ~f:(fun driver ->
      let (module H) =
        Helpers.make ~driver ~sexp_of_result:[%sexp_of: int String.Map.t]
      in
      H.show ();
      [%expect {|
        ((a 1)
         (b 3)) |}];
      H.set_input (String.Map.of_alist_exn [ "a", 1; "b", 2 ]);
      [%expect {|
        ((a 2)
         (b 3)) |}])
;;

let%expect_test "Incremental.of_incr" =
  let var = Incr.Var.create "hello" in
  let incr = Incr.Var.watch var in
  let component = Bonsai.Arrow_deprecated.With_incr.of_incr incr in
  run_test
    ~component
    ~initial_input:(String.Map.of_alist_exn [ "a", 0; "b", 2 ])
    ~f:(fun driver ->
      [%expect {| |}];
      let (module H) = Helpers.make_string ~driver in
      H.show ();
      [%expect {| hello |}];
      Incr.Var.set var "world";
      Driver.flush driver;
      H.show ();
      [%expect {| world |}];
      (* reset for next test *)
      Incr.Var.set var "hello")
;;

module _ = struct
  open Bonsai.Arrow_deprecated.Let_syntax

  let dummy
        (type t)
        (module M : Bonsai.Arrow_deprecated.Model with type t = t)
        ~default
        ~equal
    =
    Bonsai.Arrow_deprecated.state_machine
      ~sexp_of_model:[%sexp_of: M.t]
      ~equal
      ~sexp_of_action:[%sexp_of: M.t]
      [%here]
      ~default_model:default
      ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) () _model -> Fn.id)
    >>| Tuple2.map_fst ~f:M.sexp_of_t
  ;;

  let%expect_test "normal operation" =
    let driver =
      Driver.create
        ~initial_input:()
        ~clock:(Bonsai.Time_source.create ~start:(Time_ns.now ()))
        (dummy (module Int) ~equal:[%equal: Int.t] ~default:5)
    in
    let (module H) = Helpers.make_with_inject ~driver ~sexp_of_result:Fn.id in
    H.show ();
    [%expect {| 5 |}]
  ;;
end
