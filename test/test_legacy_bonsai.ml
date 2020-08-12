open! Core_kernel
open! Import

(* We need to fake the source-code position because this test is run in
   two files with different names

   In particular, we can't just use [print_s ~hide_positions:true] because that
   only hides line and column numbers, but includes the file name. *)
let dummy_source_code_position =
  Source_code_position.
    { pos_fname = "file_name.ml"; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 }
;;

let run_test ~(component : _ Bonsai.Arrow.t) ~initial_input ~f =
  let driver component = Driver.create component ~initial_input in
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
    type t = string * (Action.t -> Event.t)
  end

  let apply_action ~inject:_ ~schedule_event:_ () model : Action.t -> Model.t = function
    | Increment -> model + 1
    | Decrement -> model - 1
  ;;

  let compute ~inject () m = Int.to_string m, inject
  let name = "counter-component"
end

let%expect_test "enum" =
  let open Bonsai.Arrow.Infix in
  let component =
    Bonsai.Arrow.enum
      (module Bool)
      ~which:Tuple2.get1
      ~handle:(function
        | true -> Tuple2.get2 @>> Bonsai.Arrow.pure ~f:(sprintf "true %d")
        | false -> Tuple2.get2 @>> Bonsai.Arrow.pure ~f:(sprintf "false %d"))
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
  let open Bonsai.Arrow.Infix in
  let module Action = struct
    type t =
      | Outer of Counter_component.Action.t
      | Inner of Counter_component.Action.t
  end
  in
  let component =
    let%map.Bonsai.Arrow (result, inject_inner), inject_outer =
      Bonsai.Arrow.of_module (module Counter_component) ~default_model:1
      >>> Bonsai.Arrow.first
            (Bonsai.Arrow.enum
               (module Bool)
               ~which:(fun digit -> Int.of_string digit mod 3 = 0)
               ~handle:(function
                 | false ->
                   Fn.ignore
                   @>> Bonsai.Arrow.of_module (module Counter_component) ~default_model:0
                   >>| Tuple2.map_fst ~f:(sprintf "counter %s")
                 | true ->
                   Bonsai.Arrow.pure ~f:(fun s ->
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
      ("an action inside of Bonsai.enum as been dropped because the computation is no longer active"
       (key true) (action (First Increment)))
      pure 3|}];
    H.do_actions [ Outer Increment ];
    [%expect "counter 2"])
;;

let%expect_test "constant component" =
  run_test
    ~component:(Bonsai.Arrow.const "some constant value")
    ~initial_input:()
    ~f:(fun driver ->
      [%expect {| |}];
      let (module H) = Helpers.make_string ~driver in
      H.show ();
      [%expect {| some constant value |}])
;;

let%expect_test "module component" =
  run_test
    ~component:(Bonsai.Arrow.of_module (module Counter_component) ~default_model:0)
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
    let%map.Bonsai.Arrow model, inject =
      Bonsai.Arrow.state_machine
        (module Counter_component.Model)
        (module Counter_component.Action)
        dummy_source_code_position
        ~default_model:0
        ~apply_action:(fun ~inject:_ ~schedule_event:_ () model -> function
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
  let open Bonsai.Arrow.Let_syntax in
  let counter_component =
    Bonsai.Arrow.of_module (module Counter_component) ~default_model:0
  in
  let component =
    let%map a_side = Bonsai.Arrow.const 5
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
  let open Bonsai.Arrow.Let_syntax in
  let module Model = struct
    type t =
      { a : int
      ; b : int
      }
    [@@deriving fields]
  end
  in
  let counter_component =
    Bonsai.Arrow.of_module (module Counter_component) ~default_model:0
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
    Bonsai.Arrow.With_incr.pure
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
    H.set_input (Int.Map.add_exn initial_input ~key:3 ~data:3);
    [%expect
      {|
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
      type t = unit * (Action.t -> Event.t)
    end

    let apply_action ~inject:_ ~schedule_event () () Action.Trigger =
      schedule_event (Event.external_ "hello world")
    ;;

    let compute ~inject () () = (), inject
    let name = "raises-something-from-without"
  end
  in
  let component =
    Bonsai.Arrow.of_module (module Raises_something_from_without) ~default_model:()
  in
  run_test ~component ~initial_input:() ~f:(fun driver ->
    [%expect {| |}];
    [%expect {||}];
    let (module H) =
      Helpers.make_with_inject ~driver ~sexp_of_result:[%sexp_of: unit]
    in
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
      type t = unit * (Action.t -> Event.t)
    end

    let apply_action ~inject:_ ~schedule_event () () Action.Trigger =
      schedule_event
        (Event.sequence
           [ Event.external_ "hello world"; Event.no_op; Event.external_ "goodbye world" ])
    ;;

    let compute ~inject () () = (), inject
    let name = "raises-something-from-without"
  end
  in
  let component =
    Bonsai.Arrow.of_module (module Raises_something_from_without) ~default_model:()
  in
  run_test ~component ~initial_input:() ~f:(fun driver ->
    [%expect {| |}];
    let (module H) =
      Helpers.make_with_inject ~driver ~sexp_of_result:[%sexp_of: unit]
    in
    H.do_actions [ Trigger ];
    [%expect
      {|
      External event: hello world
      External event: goodbye world
      () |}])
;;

let%expect_test "model cutoff" =
  let module T = struct
    let name = "incremental of_module"

    module Input = Unit

    module Model = struct
      include Int

      let equal a b = if a > 2 then true else Int.equal a b
    end

    module Action = Unit

    module Result = struct
      type t = string * (unit -> Ui_event.t)
    end

    let apply_action _input model ~inject:_ =
      let%map.Incr model = model in
      fun ~schedule_event:_ () -> model + 1
    ;;

    let compute _input model ~inject =
      let%map.Incr model = model in
      Int.to_string model, inject
    ;;
  end
  in
  let component =
    Bonsai.Arrow.With_incr.of_module (module T) ~default_model:0
    |> Bonsai.Arrow.With_incr.model_cutoff
  in
  run_test ~component ~initial_input:() ~f:(fun driver ->
    [%expect {| |}];
    let (module H) = Helpers.make_string_with_inject ~driver in
    H.show ();
    [%expect "0"];
    H.do_actions [ () ];
    [%expect "1"];
    H.do_actions [ () ];
    [%expect "2"];
    H.do_actions [ () ];
    [%expect "3"];
    H.do_actions [ () ];
    [%expect "3"])
;;

let%expect_test "value cutoff" =
  let open Bonsai.Arrow.Infix in
  let cutoff =
    Incr.Cutoff.create (fun ~old_value ~new_value -> old_value % 2 = new_value % 2)
  in
  let component =
    Bonsai.Arrow.With_incr.value_cutoff ~cutoff >>> Bonsai.Arrow.pure ~f:Int.to_string
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
      type t = (int * string list) * (Action.t -> Event.t)
    end

    let apply_action ~inject:_ ~schedule_event:_ _words model : Action.t -> Model.t
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
    Bonsai.Arrow.of_module (module Words_counter_component) ~default_model:0
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
  let open Bonsai.Arrow.Infix in
  let component_a = Bonsai.Arrow.pure ~f:(fun model -> model mod 5) in
  let component_b = Bonsai.Arrow.pure ~f:(fun input -> input + 2) in
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
  let open Bonsai.Arrow.Infix in
  let component_a = Bonsai.Arrow.pure ~f:(fun model -> model mod 5) in
  let component_b =
    Bonsai.Arrow.With_incr.pure ~f:(fun input -> Incr.map input ~f:(fun i -> i + 2))
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
  let open Bonsai.Arrow.Infix in
  let component = String.length @>> Bonsai.Arrow.pure ~f:(fun input -> input + 1) in
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
    Bonsai.Arrow.pure ~f:(fun x -> x + 1) |> Bonsai.Arrow.Map.assoc_input (module String)
  in
  run_test
    ~component
    ~initial_input:(String.Map.of_alist_exn [ "a", 0; "b", 2 ])
    ~f:(fun driver ->
      [%expect {| |}];
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
  let component = Bonsai.Arrow.With_incr.of_incr incr in
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

module Model_sexpification = struct
  open Bonsai.Arrow.Let_syntax

  let dummy (type t) (module M : Bonsai.Arrow.Model with type t = t) ~default =
    Bonsai.Arrow.state_machine
      (module M)
      (module M)
      [%here]
      ~default_model:default
      ~apply_action:(fun ~inject:_ ~schedule_event:_ () _model -> Fn.id)
    >>| Tuple2.map_fst ~f:M.sexp_of_t
  ;;

  let%expect_test "normal operation" =
    let driver = Driver.create ~initial_input:() (dummy (module Int) ~default:5) in
    let (module H) = Helpers.make_with_inject ~driver ~sexp_of_result:Fn.id in
    H.show ();
    [%expect {| 5 |}]
  ;;

  let%expect_test "with of_sexp" =
    let driver =
      Driver.create
        ~initial_model_sexp:[%sexp [ 2; () ]]
        ~initial_input:()
        (dummy (module Int) ~default:5)
    in
    let (module H) = Helpers.make_with_inject ~driver ~sexp_of_result:Fn.id in
    H.show ();
    [%expect {| 2 |}]
  ;;

  let%expect_test "multiple components" =
    let component =
      let%map (a, _), (b, _) =
        Bonsai.Arrow.both (dummy (module Int) ~default:5) (dummy (module Int) ~default:5)
      in
      Sexp.List [ a; b ], Nothing.unreachable_code
    in
    let driver =
      Driver.create
        ~initial_model_sexp:[%sexp [ [ [ "2"; () ]; [ [ "3"; () ]; () ] ]; () ]]
        ~initial_input:()
        component
    in
    let (module H) = Helpers.make_with_inject ~driver ~sexp_of_result:Fn.id in
    H.show ();
    [%expect {| (2 3) |}]
  ;;

  let%expect_test "enum" =
    let module Action = struct
      type t =
        | Outer of bool
        | Inner of string
    end
    in
    let component =
      let%map.Bonsai.Arrow (inner, change_inner), change_outer =
        dummy (module Bool) ~default:true
        >>> Bonsai.Arrow.first
              (Bonsai.Arrow.if_
                 [%of_sexp: bool]
                 ~then_:
                   (Fn.ignore @>> dummy (module Int) ~default:0
                    >>| Tuple2.map_snd ~f:(fun inject s -> s |> int_of_string |> inject))
                 ~else_:(Fn.ignore @>> dummy (module String) ~default:"world"))
      in
      let inject = function
        | Action.Outer b -> change_outer b
        | Inner s -> change_inner s
      in
      inner, inject
    in
    let driver = Driver.create ~initial_input:() component in
    let (module H) = Helpers.make_with_inject ~driver ~sexp_of_result:Fn.id in
    H.show ();
    [%expect {| 0 |}];
    H.do_actions [ Action.Inner "23" ];
    [%expect {| 23 |}];
    H.do_actions [ Action.Outer false ];
    [%expect {| world |}];
    H.do_actions [ Action.Inner "bonsai!" ];
    [%expect {| bonsai! |}];
    let initial_model_sexp = Driver.sexp_of_model driver in
    print_s initial_model_sexp;
    [%expect
      {|
      (((false ()) (((false (bonsai! ())) (true ((23 ()) ()))) ())) ()) |}];
    let driver = Driver.create ~initial_model_sexp ~initial_input:() component in
    let (module H) = Helpers.make_with_inject ~driver ~sexp_of_result:Fn.id in
    H.show ();
    [%expect {| bonsai! |}];
    H.do_actions [ Action.Outer true ];
    [%expect {| 23 |}]
  ;;
end
