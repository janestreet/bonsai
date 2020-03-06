open! Core_kernel
open! Import

let optimize c =
  let open Bonsai_lib.Generic.Expert in
  c |> Bonsai.to_generic |> reveal |> optimize |> conceal |> Bonsai.of_generic
;;

let run_test ~(component : _ Bonsai.t) ~initial_input ~initial_model ~f =
  let optimized = optimize component in
  let optimized' = optimize optimized in
  require_equal
    ~message:"Optimization did not reach a fixed point"
    [%here]
    (module Sexp)
    [%sexp (optimized : Bonsai.t)]
    [%sexp (optimized' : Bonsai.t)];
  let print_components () =
    print_s [%message (component : Bonsai.t)];
    print_s [%message (optimized : Bonsai.t)]
  in
  let driver component = Driver.create component ~initial_input ~initial_model in
  print_components ();
  f (driver component);
  print_components ();
  f (driver optimized);
  print_components ();
  f (driver optimized')
;;

module Helpers = struct
  include Helpers

  let make ~driver = Helpers.make ~sexp_of_model:sexp_of_opaque ~driver
  let make_string ~driver = Helpers.make_string ~sexp_of_model:sexp_of_opaque ~driver

  let make_string_with_inject ~driver =
    Helpers.make_string_with_inject ~sexp_of_model:sexp_of_opaque ~driver
  ;;

  let make_with_inject ~driver =
    Helpers.make_with_inject ~sexp_of_model:sexp_of_opaque ~driver
  ;;
end

let%expect_test "constant component" =
  run_test
    ~component:(Bonsai.const "some constant value")
    ~initial_input:()
    ~initial_model:()
    ~f:(fun driver ->
      [%expect {|
        (component Const)
        (optimized Const) |}];
      let (module H) = Helpers.make_string ~driver in
      H.show ();
      [%expect {| some constant value |}])
;;

let%expect_test "function component" =
  run_test
    ~component:(Bonsai.pure_from_model ~f:Int.to_string)
    ~initial_input:()
    ~initial_model:1
    ~f:(fun driver ->
      [%expect {|
        (component Pure_model)
        (optimized Pure_model) |}];
      let (module H) = Helpers.make_string ~driver in
      H.show ();
      [%expect "1"];
      H.set_model 5;
      [%expect "5"])
;;

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

let%expect_test "module component" =
  run_test
    ~component:(Bonsai.of_module (module Counter_component))
    ~initial_input:()
    ~initial_model:0
    ~f:(fun driver ->
      [%expect
        {|
        (component (Leaf counter-component))
        (optimized (Leaf counter-component)) |}];
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
  let module Action = Counter_component.Action in
  let component =
    let%map.Bonsai inject =
      Bonsai.Model.state_machine
        (* We need to fake the source-code position because this test is run in
           two files with different names

           In particular, we can't just use [print_s ~hide_positions:true] because that
           only hides line and column numbers, but includes the file name. *)
        Source_code_position.
          { pos_fname = "file_name.ml"; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 }
        ~sexp_of_action:[%sexp_of: Action.t]
        ~apply_action:(fun ~inject:_ ~schedule_event:_ () model -> function
          | Increment -> model + 1
          | Decrement -> model - 1)
    and model = Bonsai.model in
    Int.to_string model, inject
  in
  run_test ~component ~initial_input:() ~initial_model:0 ~f:(fun driver ->
    [%expect
      {|
        (component (
          Map (Map2 (Leaf "state-machine defined at file_name.ml:0:0") Return_model)))
        (optimized (
          Map2 (Leaf "state-machine defined at file_name.ml:0:0") Return_model)) |}];
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
  let open Bonsai.Let_syntax in
  let counter_component = Bonsai.of_module (module Counter_component) in
  let component =
    let%map a_side = Bonsai.const 5
    and b_side, inject_b = counter_component in
    sprintf "%d | %s" a_side b_side, inject_b
  in
  run_test ~component ~initial_input:() ~initial_model:0 ~f:(fun driver ->
    [%expect
      {|
        (component (Map (Map2 Const (Leaf counter-component))))
        (optimized (Map2 Const (Leaf counter-component))) |}];
    let (module H) = Helpers.make_string_with_inject ~driver in
    H.show ();
    [%expect {| 5 | 0 |}];
    H.do_actions [ Increment ];
    [%expect {| 5 | 1 |}];
    H.do_actions [ Decrement ];
    [%expect {| 5 | 0 |}])
;;

let%expect_test "module project field" =
  let open Bonsai.Let_syntax in
  let module Model = struct
    type t =
      { a : int
      ; b : int
      }
    [@@deriving fields]
  end
  in
  let counter_component = Bonsai.of_module (module Counter_component) in
  let component =
    let%map a_side, inject_a = counter_component |> Bonsai.Model.field Model.Fields.a
    and b_side, inject_b = counter_component |> Bonsai.Model.field Model.Fields.b in
    ( sprintf "%s | %s" a_side b_side
    , function
      | First a -> inject_a a
      | Second b -> inject_b b )
  in
  run_test
    ~component
    ~initial_input:()
    ~initial_model:{ Model.a = 0; b = 0 }
    ~f:(fun driver ->
      [%expect
        {|
        (component (
          Map (
            Map2
            (Projection (Leaf counter-component))
            (Projection (Leaf counter-component)))))
        (optimized (
          Map2
          (Projection (Leaf counter-component))
          (Projection (Leaf counter-component)))) |}];
      let (module H) = Helpers.make_string_with_inject ~driver in
      H.show ();
      [%expect {| 0 | 0 |}];
      H.do_actions [ First Increment ];
      [%expect {| 1 | 0 |}];
      H.do_actions [ Second Decrement ];
      [%expect {| 1 | -1 |}])
;;

let%expect_test "module project with [Nothing.t] action" =
  let open Bonsai.Let_syntax in
  let display_component =
    Bonsai.pure_from_model ~f:Int64.to_string_hum
    |> Bonsai.Model.f ~get:Int64.of_int ~set:(fun _ _ ->
      failwith "component modified its model")
  in
  let counter_component = Bonsai.of_module (module Counter_component) in
  let component =
    let%map a_side = display_component
    and b_side, inject_b = counter_component in
    sprintf "%s | %s" a_side b_side, inject_b
  in
  run_test ~component ~initial_input:() ~initial_model:0 ~f:(fun driver ->
    [%expect
      {|
        (component (
          Map (
            Map2
            (Projection Pure_model)
            (Leaf       counter-component))))
        (optimized (
          Map2
          (Projection Pure_model)
          (Leaf       counter-component))) |}];
    let (module H) = Helpers.make_string_with_inject ~driver in
    H.show ();
    [%expect {| 0 | 0 |}];
    H.do_actions [ Increment ];
    [%expect {| 1 | 1 |}];
    H.do_actions [ Increment ];
    [%expect {| 2 | 2 |}];
    H.do_actions [ Decrement ];
    [%expect {| 1 | 1 |}])
;;

let%expect_test "incremental fn constructor" =
  let component =
    Bonsai.With_incr.pure_from_model
      ~f:
        (Incr_map.mapi ~f:(fun ~key:_ ~data ->
           print_endline "doing math";
           data + 1))
  in
  let initial_model = [ 0, 0; 1, 1; 2, 2 ] |> Int.Map.of_alist_exn in
  run_test ~component ~initial_input:() ~initial_model ~f:(fun driver ->
    [%expect
      {|
        (component (Compose Const (With_readonly_model (Map_input Pure_incr))))
        (optimized (Compose Const (With_readonly_model (Map_input Pure_incr))))
        doing math
        doing math
        doing math |}];
    let (module H) = Helpers.make ~driver ~sexp_of_result:[%sexp_of: int Int.Map.t] in
    H.show ();
    [%expect {|
      ((0 1)
       (1 2)
       (2 3)) |}];
    H.set_model (Int.Map.add_exn initial_model ~key:3 ~data:3);
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

    module Action = struct
      type t = Trigger [@@deriving sexp_of]
    end

    module Model = Unit

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
  let component = Bonsai.of_module (module Raises_something_from_without) in
  run_test ~component ~initial_input:() ~initial_model:() ~f:(fun driver ->
    [%expect
      {|
        (component (Leaf raises-something-from-without))
        (optimized (Leaf raises-something-from-without)) |}];
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
  let component = Bonsai.of_module (module Raises_something_from_without) in
  run_test ~component ~initial_input:() ~initial_model:() ~f:(fun driver ->
    [%expect
      {|
        (component (Leaf raises-something-from-without))
        (optimized (Leaf raises-something-from-without)) |}];
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

let%expect_test "map merge combinator" =
  let component_a = Bonsai.pure_from_model ~f:(fun a -> a + 1) in
  let component_b = Bonsai.pure_from_model ~f:Int.of_string in
  let a_map = Bonsai.Map.assoc_model component_a ~comparator:(module Int) in
  let b_map = Bonsai.Map.assoc_model component_b ~comparator:(module Int) in
  let module Model = struct
    type t =
      { a : int Int.Map.t
      ; b : string Int.Map.t
      }
    [@@deriving fields]
  end
  in
  let component =
    let a = a_map |> Bonsai.Model.field Model.Fields.a in
    let b = b_map |> Bonsai.Model.field Model.Fields.b in
    Bonsai.Map.merge a b ~f:(fun ~key:_ ->
      function
      | `Both (a, b) -> Some (a + b)
      | _ -> None)
  in
  let initial_model = { Model.a = Int.Map.empty; b = Int.Map.empty } in
  run_test ~component ~initial_input:() ~initial_model ~f:(fun driver ->
    [%expect
      {|
        (component (
          Map_incr (
            Map2
            (Projection (Assoc_by_model (Map_input Pure_model)))
            (Projection (Assoc_by_model (Map_input Pure_model))))))
        (optimized (
          Map_incr (
            Map2
            (Projection (Assoc_by_model (Map_input Pure_model)))
            (Projection (Assoc_by_model (Map_input Pure_model)))))) |}];
    let (module H) = Helpers.make ~driver ~sexp_of_result:[%sexp_of: int Int.Map.t] in
    H.show ();
    [%expect "()"];
    H.set_model { initial_model with a = Int.Map.singleton 1 5 };
    [%expect "()"];
    H.set_model { Model.a = Int.Map.singleton 1 5; b = Int.Map.singleton 1 "10" };
    [%expect "((1 16))"])
;;

let%expect_test "incremental module constructor" =
  let open Incr.Let_syntax in
  let char_compute =
    Bonsai.With_incr.pure_from_model ~f:(fun model ->
      let model = Incr_map.of_set model in
      Incr_map.unordered_fold
        model
        ~init:Int.Map.empty
        ~add:(fun ~key ~data:_ acc ->
          Int.Map.update acc (String.length key) ~f:(function
            | Some set -> Set.add set key
            | None -> String.Set.singleton key))
        ~remove:(fun ~key ~data:_ acc ->
          Int.Map.change acc (String.length key) ~f:(function
            | Some set ->
              let set = Set.remove set key in
              if Set.is_empty set then None else Some set
            | None -> None)))
  in
  let module Char_count_component = struct
    module Input = struct
      type t = String.Set.t Int.Map.t
    end

    module Action = struct
      type t = Remove_strings_of_length_5 [@@deriving sexp_of]
    end

    module Result = struct
      type t = String.Set.t Int.Map.t * (Action.t -> Event.t)
    end

    module Model = String.Set

    let apply_action (input : Input.t Incr.t) (model : Model.t Incr.t) ~inject:_ =
      let lookup = Incr_map.Lookup.create input ~comparator:Int.comparator in
      let%map strings_of_length_5 = Incr_map.Lookup.find lookup 5
      and model = model in
      fun ~schedule_event:_ -> function
        | Action.Remove_strings_of_length_5 ->
          (match strings_of_length_5 with
           | Some strings_of_length_5 ->
             Set.fold strings_of_length_5 ~init:model ~f:Set.remove
           | None -> model)
    ;;

    let compute input _model ~inject =
      let%map input = input in
      input, inject
    ;;

    let name = "raises-something-from-without"
  end
  in
  let char_count = Bonsai.With_incr.of_module (module Char_count_component) in
  let component = Bonsai.compose char_compute char_count in
  let initial_model = String.Set.empty in
  run_test ~component ~initial_input:() ~initial_model ~f:(fun driver ->
    [%expect
      {|
        (component (
          Compose
          (Compose Const (With_readonly_model (Map_input Pure_incr)))
          (Leaf_incr raises-something-from-without)))
        (optimized (
          Compose
          (Compose Const (With_readonly_model (Map_input Pure_incr)))
          (Leaf_incr raises-something-from-without))) |}];
    let (module H) =
      Helpers.make_with_inject
        ~driver
        ~sexp_of_result:[%sexp_of: String.Set.t Int.Map.t]
    in
    H.show ();
    [%expect {| () |}];
    let model = Set.add initial_model "hello" in
    H.set_model model;
    [%expect {| ((5 (hello))) |}];
    let model = Set.add model "there" in
    H.set_model model;
    [%expect {| ((5 (hello there))) |}];
    let model = Set.add model "hi" in
    H.set_model model;
    [%expect {| ((2 (hi)) (5 (hello there))) |}];
    H.do_actions [ Char_count_component.Action.Remove_strings_of_length_5 ];
    [%expect {| ((2 (hi))) |}])
;;

let%expect_test "model cutoff" =
  let cutoff =
    Incr.Cutoff.create (fun ~old_value ~new_value -> old_value % 2 = new_value % 2)
  in
  let component =
    Bonsai.pure_from_model ~f:Int.to_string |> Bonsai.With_incr.model_cutoff ~cutoff
  in
  run_test ~component ~initial_input:() ~initial_model:1 ~f:(fun driver ->
    [%expect
      {|
        (component (Model_cutoff ((t Pure_model) (cutoff (F <fun>)))))
        (optimized (Model_cutoff ((t Pure_model) (cutoff (F <fun>))))) |}];
    let (module H) = Helpers.make_string ~driver in
    H.show ();
    [%expect "1"];
    H.set_model 5;
    [%expect "1"];
    H.set_model 6;
    [%expect "6"];
    H.set_model 2;
    [%expect "6"])
;;

let%expect_test "value cutoff" =
  let open Bonsai.Infix in
  let cutoff =
    Incr.Cutoff.create (fun ~old_value ~new_value -> old_value % 2 = new_value % 2)
  in
  let component =
    Bonsai.With_incr.value_cutoff ~cutoff >>> Bonsai.pure ~f:Int.to_string
  in
  run_test ~component ~initial_input:1 ~initial_model:() ~f:(fun driver ->
    [%expect
      {|
        (component (Compose (Value_cutoff ((cutoff (F <fun>)))) Pure_input))
        (optimized (Map (Value_cutoff ((cutoff (F <fun>)))))) |}];
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
  let component = Bonsai.of_module (module Words_counter_component) in
  let initial_input, initial_model = [], 0 in
  run_test ~component ~initial_input ~initial_model ~f:(fun driver ->
    [%expect
      {|
        (component (Leaf words-counter-component))
        (optimized (Leaf words-counter-component)) |}];
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
  let open Bonsai.Infix in
  let component_a = Bonsai.pure_from_model ~f:(fun model -> model mod 5) in
  let component_b = Bonsai.pure ~f:(fun input -> input + 2) in
  let component = component_a >>> component_b in
  run_test ~component ~initial_input:() ~initial_model:0 ~f:(fun driver ->
    [%expect
      {|
        (component (Compose Pure_model Pure_input))
        (optimized (Map Pure_model)) |}];
    let (module H) = Helpers.make ~driver ~sexp_of_result:[%sexp_of: int] in
    H.show ();
    [%expect "2"];
    H.set_model 11;
    [%expect "3"])
;;

let%expect_test "pure_incr" =
  let open Bonsai.Infix in
  let component_a = Bonsai.pure_from_model ~f:(fun model -> model mod 5) in
  let component_b =
    Bonsai.With_incr.pure ~f:(fun input -> Incr.map input ~f:(fun i -> i + 2))
  in
  let component = component_a >>> component_b in
  run_test ~component ~initial_input:() ~initial_model:0 ~f:(fun driver ->
    [%expect
      {|
        (component (Compose Pure_model Pure_incr))
        (optimized (Compose Pure_model Pure_incr)) |}];
    let (module H) = Helpers.make ~driver ~sexp_of_result:[%sexp_of: int] in
    H.show ();
    [%expect "2"];
    H.set_model 11;
    [%expect "3"])
;;

let%expect_test "input projection" =
  let open Bonsai.Infix in
  let component = String.length @>> Bonsai.pure ~f:(fun input -> input + 1) in
  run_test ~component ~initial_input:"hi" ~initial_model:() ~f:(fun driver ->
    [%expect
      {|
        (component (Map_input Pure_input))
        (optimized (Map_input Pure_input)) |}];
    let (module H) = Helpers.make ~driver ~sexp_of_result:[%sexp_of: int] in
    H.show ();
    [%expect "3"];
    H.set_input "hello";
    [%expect "6"])
;;

let%expect_test "Model.to_input_with_other" =
  let open Bonsai.Infix in
  let component =
    Bonsai.pure ~f:(fun (input, model) -> String.length input + model)
    |> Bonsai.Model.to_input_with_other
    >>> Bonsai.pure ~f:(fun input -> input + 1)
  in
  run_test ~component ~initial_input:"hi" ~initial_model:0 ~f:(fun driver ->
    [%expect
      {|
        (component (Compose (With_readonly_model Pure_input) Pure_input))
        (optimized (Map (With_readonly_model Pure_input))) |}];
    let (module H) = Helpers.make ~driver ~sexp_of_result:[%sexp_of: int] in
    H.show ();
    [%expect "3"];
    H.set_input "hello";
    [%expect "6"];
    H.set_model 10;
    [%expect "16"];
    H.set_input "hi";
    [%expect "13"])
;;

let%expect_test "project model to input" =
  let component = Bonsai.pure ~f:(fun input -> input + 1) |> Bonsai.Model.to_input in
  run_test ~component ~initial_input:() ~initial_model:0 ~f:(fun driver ->
    [%expect
      {|
        (component (With_readonly_model (Map_input Pure_input)))
        (optimized (With_readonly_model (Map_input Pure_input))) |}];
    let (module H) = Helpers.make ~driver ~sexp_of_result:[%sexp_of: int] in
    H.show ();
    [%expect "1"];
    H.set_model 3;
    [%expect "4"])
;;

let%expect_test "assoc on model" =
  let component =
    Bonsai.of_module (module Counter_component)
    |> Bonsai.Map.assoc_model
    |> Bonsai.map ~f:(fun pair_map ->
      let string_output =
        pair_map
        |> Map.map ~f:Tuple2.get1
        |> [%sexp_of: string String.Map.t]
        |> Sexp.to_string_hum
      in
      let inject (key, action) =
        let _, inject = Map.find_exn pair_map key in
        inject action
      in
      string_output, inject)
  in
  run_test
    ~component
    ~initial_input:()
    ~initial_model:(String.Map.of_alist_exn [ "a", 0; "b", 2 ])
    ~f:(fun driver ->
      [%expect
        {|
        (component (Map (Assoc_by_model (Map_input (Leaf counter-component)))))
        (optimized (Map (Assoc_by_model (Map_input (Leaf counter-component))))) |}];
      let (module H) = Helpers.make_string_with_inject ~driver in
      H.show ();
      [%expect {| ((a 0) (b 2)) |}];
      H.do_actions [ "a", Counter_component.Action.Increment ];
      [%expect {| ((a 1) (b 2)) |}])
;;

let%expect_test "assoc on input" =
  let component = Bonsai.pure ~f:(fun x -> x + 1) |> Bonsai.Map.assoc_input in
  run_test
    ~component
    ~initial_input:(String.Map.of_alist_exn [ "a", 0; "b", 2 ])
    ~initial_model:()
    ~f:(fun driver ->
      [%expect
        {|
        (component (Assoc_by_input (Map_input Pure_input)))
        (optimized (Assoc_by_input (Map_input Pure_input))) |}];
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
  let component = Bonsai.With_incr.of_incr incr in
  let optimized = optimize component in
  print_s [%message (component : Bonsai.t) (optimized : Bonsai.t)];
  [%expect {|
    ((component Pure_incr)
     (optimized Pure_incr)) |}];
  run_test
    ~component
    ~initial_input:(String.Map.of_alist_exn [ "a", 0; "b", 2 ])
    ~initial_model:()
    ~f:(fun driver ->
      [%expect {|
        (component Pure_incr)
        (optimized Pure_incr) |}];
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

let%expect_test "id" =
  let component = Bonsai.both Bonsai.input Bonsai.model in
  let optimized = optimize component in
  print_s [%message (component : Bonsai.t) (optimized : Bonsai.t)];
  [%expect
    {|
    ((component (Map2 (Map Return_input) Return_model))
     (optimized (Map2 (Map Return_input) Return_model))) |}];
  run_test ~component ~initial_input:5 ~initial_model:"hello" ~f:(fun driver ->
    [%expect
      {|
        (component (Map2 (Map Return_input) Return_model))
        (optimized (Map2 (Map Return_input) Return_model)) |}];
    let (module H) = Helpers.make ~driver ~sexp_of_result:[%sexp_of: int * string] in
    H.show ();
    [%expect {| (5 hello) |}];
    H.set_input 20;
    [%expect {| (20 hello) |}];
    H.set_model "world";
    [%expect {| (20 world) |}])
;;

let%expect_test "enum" =
  let component =
    Bonsai.enum
      (module Bool)
      ~which:(fun _ model -> model)
      ~handle:(function
        | true -> Bonsai.pure ~f:(sprintf "true %d")
        | false -> Bonsai.pure ~f:(sprintf "false %d"))
  in
  run_test ~component ~initial_input:5 ~initial_model:true ~f:(fun driver ->
    [%expect
      {|
        (component (
          Enum
          (false (Erase_action false Pure_input))
          (true  (Erase_action true  Pure_input))))
        (optimized (
          Enum
          (false (Erase_action false Pure_input))
          (true  (Erase_action true  Pure_input)))) |}];
    let (module H) = Helpers.make_string ~driver in
    H.show ();
    [%expect {| true 5 |}];
    H.set_input 10;
    [%expect {| true 10 |}];
    H.set_model false;
    [%expect {| false 10 |}];
    H.set_input 5;
    [%expect {| false 5 |}])
;;

let%expect_test "enum with action handling `Ignore" =
  let component =
    Bonsai.enum
      (module Bool)
      ~which:(fun _ model -> model mod 3 = 0)
      ~handle:(function
        | false -> Bonsai.of_module (module Counter_component)
        | true ->
          Bonsai.pure_from_model ~f:(fun model ->
            Int.to_string model, fun _ -> failwith "cant raise actions out of this one"))
  in
  run_test ~component ~initial_model:1 ~initial_input:() ~f:(fun driver ->
    [%expect
      {|
        (component (
          Enum
          (false (Erase_action false (Leaf counter-component)))
          (true (Erase_action true Pure_model))))
        (optimized (
          Enum
          (false (Erase_action false (Leaf counter-component)))
          (true (Erase_action true Pure_model)))) |}];
    let (module H) = Helpers.make_string_with_inject ~driver in
    H.show ();
    [%expect "1"];
    H.do_actions [ Counter_component.Action.Increment ];
    [%expect "2"];
    H.do_actions
      (* The second of these actions is ignored. *)
      [ Counter_component.Action.Increment; Counter_component.Action.Increment ];
    [%expect "3"])
;;

let%expect_test "enum with action handling `Ignore" =
  let component =
    Bonsai.enum
      ~on_action_mismatch:`Raise
      (module Bool)
      ~which:(fun _ model -> model mod 3 = 0)
      ~handle:(function
        | false -> Bonsai.of_module (module Counter_component)
        | true ->
          Bonsai.pure_from_model ~f:(fun model ->
            Int.to_string model, fun _ -> failwith "cant raise actions out of this one"))
  in
  run_test ~component ~initial_model:1 ~initial_input:() ~f:(fun driver ->
    [%expect
      {|
        (component (
          Enum
          (false (Erase_action false (Leaf counter-component)))
          (true (Erase_action true Pure_model))))
        (optimized (
          Enum
          (false (Erase_action false (Leaf counter-component)))
          (true (Erase_action true Pure_model)))) |}];
    let (module H) = Helpers.make_string_with_inject ~driver in
    H.show ();
    [%expect "1"];
    H.do_actions [ Counter_component.Action.Increment ];
    [%expect "2"];
    Expect_test_helpers_kernel.require_does_raise [%here] (fun () ->
      H.do_actions
        (* The second of these actions throws the exception that follows. *)
        [ Counter_component.Action.Increment; Counter_component.Action.Increment ]);
    [%expect
      {|
        ("Component received an action for key"
          (action_key false)
          "while in the key"
          (current_key true)) |}])
;;

module Optimize = struct
  open Bonsai.Let_syntax
  open Bonsai.Infix

  let unoptimizable = Bonsai.With_incr.of_incr (Incr.return 5)
  let leaf = Bonsai.of_module (module Counter_component)

  let run component =
    let optimized = optimize component in
    print_s [%message (component : Bonsai.t) (optimized : Bonsai.t)]
  ;;

  let%expect_test "map_over_constant" =
    run (Bonsai.const 5 >>| Int.succ);
    [%expect {| ((component (Map Const)) (optimized Const)) |}]
  ;;

  let%expect_test "map_over_map" =
    run (unoptimizable >>| Int.succ >>| Int.succ >>| Int.succ);
    [%expect {| ((component (Map (Map (Map Pure_incr)))) (optimized (Map Pure_incr))) |}]
  ;;

  let%expect_test "compose_over_const_and_pure" =
    run (Bonsai.const 5 >>> Bonsai.pure ~f:(fun x -> x * 2));
    [%expect {| ((component (Compose Const Pure_input)) (optimized Const)) |}]
  ;;

  let%expect_test "map_over_map2" =
    run
      ((let%map a = unoptimizable
        and b = unoptimizable in
        a + b)
       >>| Int.succ);
    [%expect
      {|
      ((component (Map (Map (Map2 Pure_incr Pure_incr))))
       (optimized (Map2 Pure_incr Pure_incr))) |}]
  ;;

  let%expect_test "map_over_leaf" =
    run (leaf >>| Tuple2.get1);
    [%expect
      {|
      ((component (Map (Leaf counter-component)))
       (optimized (Leaf counter-component))) |}]
  ;;

  (* Optimize deliberately leaves map_input-over-leaf alone. *)
  let%expect_test "map_input_over_leaf" =
    run (ignore @>> leaf);
    [%expect
      {|
      ((component (Map_input (Leaf counter-component)))
       (optimized (Map_input (Leaf counter-component)))) |}]
  ;;

  let%expect_test "project_over_project" =
    run
      (unoptimizable
       |> Bonsai.Model.f ~get:Int.of_string ~set:(fun _ -> Int.to_string)
       |> Bonsai.Model.f ~get:Int.to_string ~set:(fun _ -> Int.of_string));
    [%expect
      {|
      ((component (Projection (Projection Pure_incr)))
       (optimized (Projection Pure_incr))) |}]
  ;;

  (* Optimize deliberately leaves project-over-leaf alone. *)
  let%expect_test "project_over_leaf" =
    run (leaf |> Bonsai.Model.f ~get:Int.of_string ~set:(fun _ -> Int.to_string));
    [%expect
      {|
      ((component (Projection (Leaf counter-component)))
       (optimized (Projection (Leaf counter-component)))) |}]
  ;;
end
