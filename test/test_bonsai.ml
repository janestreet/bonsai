open! Core_kernel
open! Import

let%expect_test "constant component" =
  let constant_component = Bonsai.const "some constant value" in
  let driver = Driver.create ~initial_input:() ~initial_model:() constant_component in
  let (module H) = Helpers.make_string ~driver in
  H.show ();
  [%expect {| "some constant value" |}]
;;

let%expect_test "function component" =
  let fn_component = Bonsai.pure_from_model ~f:string_of_int in
  let driver = Driver.create ~initial_input:() ~initial_model:1 fn_component in
  let (module H) = Helpers.make_string ~driver in
  H.show ();
  [%expect "1"];
  H.set_model 5;
  [%expect "5"]
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
  let name = Source_code_position.to_string [%here]
end

let%expect_test "module component" =
  let component = Bonsai.of_module (module Counter_component) in
  let driver = Driver.create ~initial_input:() ~initial_model:0 component in
  let (module H) = Helpers.make_string_with_inject ~driver in
  H.show ();
  [%expect "0"];
  H.do_actions [ Increment ];
  [%expect "1"];
  H.do_actions [ Decrement ];
  [%expect "0"];
  (* Increment and decrement in the same cycle should cancel out *)
  H.do_actions [ Increment; Decrement ];
  [%expect "0"]
;;

let%expect_test "switch-case" =
  let module Model = struct
    type t =
      | Show_counter of int
      | Hide
  end
  in
  let counter_component = Bonsai.of_module (module Counter_component) in
  let component =
    Bonsai.Incremental.switch ~f:(fun { create_case } input model ->
      let open Incr.Let_syntax in
      match%pattern_bind model with
      | Model.Show_counter i ->
        print_endline "case switched to show-counter";
        let case_model =
          let%map i = i
          and input = input in
          i + input
        in
        create_case
          counter_component
          ~case_input:(Incr.return ())
          ~case_model
          ~lift:(fun i -> Show_counter i)
      | Model.Hide ->
        print_endline "case switched to hide";
        create_case
          (Bonsai.const
             ("hidden", Fn.const (Event.External_event "action triggered on hidden")))
          ~case_input:(Incr.return ())
          ~case_model:(Incr.return ())
          ~lift:(fun () -> Model.Hide))
  in
  let driver = Driver.create ~initial_input:0 ~initial_model:Model.Hide component in
  let (module H) = Helpers.make_string_with_inject ~driver in
  H.show ();
  [%expect {|
    case switched to hide
    hidden |}];
  H.set_model (Model.Show_counter 5);
  [%expect {|
    case switched to show-counter
    5 |}];
  H.do_actions [ Increment ];
  [%expect "6"];
  H.set_model Model.Hide;
  [%expect {|
    case switched to hide
    hidden |}];
  H.do_actions [ Increment ];
  [%expect {|
    External event: action triggered on hidden
    hidden |}];
  H.set_input 100;
  [%expect {|
    hidden |}];
  H.set_model (Model.Show_counter 5);
  [%expect {|
    case switched to show-counter
    105 |}]
;;

let%expect_test "basic Same_model let syntax" =
  let open Bonsai.Let_syntax in
  let counter_component = Bonsai.of_module (module Counter_component) in
  let component =
    let%map a_side = Bonsai.const 5
    and b_side, inject_b = counter_component in
    sprintf "%d | %s" a_side b_side, inject_b
  in
  let driver = Driver.create ~initial_input:() ~initial_model:0 component in
  let (module H) = Helpers.make_string_with_inject ~driver in
  H.show ();
  [%expect {| "5 | 0" |}];
  H.do_actions [ Increment ];
  [%expect {| "5 | 1" |}];
  H.do_actions [ Decrement ];
  [%expect {| "5 | 0" |}]
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
    let%map a_side, inject_a =
      counter_component |> Bonsai.Project.Model.field Model.Fields.a
    and b_side, inject_b =
      counter_component |> Bonsai.Project.Model.field Model.Fields.b
    in
    ( sprintf "%s | %s" a_side b_side
    , function
      | First a -> inject_a a
      | Second b -> inject_b b )
  in
  let driver =
    Driver.create ~initial_input:() ~initial_model:{ Model.a = 0; b = 0 } component
  in
  let (module H) = Helpers.make_string_with_inject ~driver in
  H.show ();
  [%expect {| "0 | 0" |}];
  H.do_actions [ First Increment ];
  [%expect {| "1 | 0" |}];
  H.do_actions [ Second Decrement ];
  [%expect {| "1 | -1" |}]
;;

let%expect_test "module project with [Nothing.t] action" =
  let open Bonsai.Let_syntax in
  let display_component =
    Bonsai.pure_from_model ~f:Int64.to_string_hum
    |> Bonsai.Project.Model.f ~unlift:Int64.of_int ~lift:(fun _ _ ->
      failwith "component modified its model")
  in
  let counter_component = Bonsai.of_module (module Counter_component) in
  let component =
    let%map a_side = display_component
    and b_side, inject_b = counter_component in
    sprintf "%s | %s" a_side b_side, inject_b
  in
  let driver = Driver.create ~initial_input:() ~initial_model:0 component in
  let (module H) = Helpers.make_string_with_inject ~driver in
  H.show ();
  [%expect {| "0 | 0" |}];
  H.do_actions [ Increment ];
  [%expect {| "1 | 1" |}];
  H.do_actions [ Increment ];
  [%expect {| "2 | 2" |}];
  H.do_actions [ Decrement ];
  [%expect {| "1 | 1" |}]
;;

let%expect_test "incremental fn constructor" =
  let incr_map_component =
    Bonsai.Incremental.pure_from_model
      ~f:
        (Incr_map.mapi ~f:(fun ~key:_ ~data ->
           print_endline "doing math";
           data + 1))
  in
  let initial_model = [ 0, 0; 1, 1; 2, 2 ] |> Int.Map.of_alist_exn in
  let driver = Driver.create ~initial_input:() ~initial_model incr_map_component in
  let (module H) = Helpers.make ~driver ~sexp_of_result:[%sexp_of: int Int.Map.t] in
  H.show ();
  [%expect {|
    doing math
    doing math
    doing math
    ((0 1) (1 2) (2 3)) |}];
  H.set_model (Int.Map.add_exn initial_model ~key:3 ~data:3);
  [%expect {|
    doing math
    ((0 1) (1 2) (2 3) (3 4)) |}]
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
      schedule_event (Event.External_event "hello world")
    ;;

    let compute ~inject () () = (), inject
    let name = Source_code_position.to_string [%here]
  end
  in
  let component = Bonsai.of_module (module Raises_something_from_without) in
  let driver = Driver.create ~initial_input:() ~initial_model:() component in
  let (module H) = Helpers.make_with_inject ~driver ~sexp_of_result:[%sexp_of: unit] in
  H.do_actions [ Trigger ];
  [%expect {|
    External event: hello world
    () |}]
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
           [ Event.External_event "hello world"
           ; Event.no_op
           ; Event.External_event "goodbye world"
           ])
    ;;

    let compute ~inject () () = (), inject
    let name = Source_code_position.to_string [%here]
  end
  in
  let component = Bonsai.of_module (module Raises_something_from_without) in
  let driver = Driver.create ~initial_input:() ~initial_model:() component in
  let (module H) = Helpers.make_with_inject ~driver ~sexp_of_result:[%sexp_of: unit] in
  H.do_actions [ Trigger ];
  [%expect
    {|
    External event: hello world
    External event: goodbye world
    () |}]
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
  let combined =
    let a = a_map |> Bonsai.Project.Model.field Model.Fields.a in
    let b = b_map |> Bonsai.Project.Model.field Model.Fields.b in
    Bonsai.Map.merge a b ~f:(fun ~key:_ ->
      function
      | `Both (a, b) -> Some (a + b)
      | _ -> None)
  in
  let initial_model = { Model.a = Int.Map.empty; b = Int.Map.empty } in
  let driver = Driver.create ~initial_input:() ~initial_model combined in
  let (module H) = Helpers.make ~driver ~sexp_of_result:[%sexp_of: int Int.Map.t] in
  H.show ();
  [%expect "()"];
  H.set_model { initial_model with a = Int.Map.singleton 1 5 };
  [%expect "()"];
  H.set_model { Model.a = Int.Map.singleton 1 5; b = Int.Map.singleton 1 "10" };
  [%expect "((1 16))"]
;;

let%expect_test "incremental module constructor" =
  let open Incr.Let_syntax in
  let char_compute =
    Bonsai.Incremental.pure_from_model ~f:(fun model ->
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

    let name = Source_code_position.to_string [%here]
  end
  in
  let char_count = Bonsai.Incremental.of_module (module Char_count_component) in
  let component = Bonsai.compose char_compute char_count in
  let initial_model = String.Set.empty in
  let driver = Driver.create ~initial_input:() ~initial_model component in
  let (module H) =
    Helpers.make_with_inject ~driver ~sexp_of_result:[%sexp_of: String.Set.t Int.Map.t]
  in
  H.show ();
  [%expect "()"];
  let model = Set.add initial_model "hello" in
  H.set_model model;
  [%expect "((5 (hello)))"];
  let model = Set.add model "there" in
  H.set_model model;
  [%expect "((5 (hello there)))"];
  let model = Set.add model "hi" in
  H.set_model model;
  [%expect "((2 (hi)) (5 (hello there)))"];
  H.do_actions [ Char_count_component.Action.Remove_strings_of_length_5 ];
  [%expect "((2 (hi)))"]
;;

let%expect_test "cutoff" =
  let fn_component =
    Bonsai.pure_from_model ~f:Int.to_string
    |> Bonsai.Incremental.with_cutoff
         ~cutoff:
           (Incr.Cutoff.create (fun ~old_value ~new_value ->
              old_value % 2 = new_value % 2))
  in
  let driver = Driver.create ~initial_input:() ~initial_model:1 fn_component in
  let (module H) = Helpers.make_string ~driver in
  H.show ();
  [%expect "1"];
  H.set_model 5;
  [%expect "1"];
  H.set_model 6;
  [%expect "6"];
  H.set_model 2;
  [%expect "6"]
;;

let%expect_test "input" =
  let module Words_counter_component = struct
    module Input = struct
      type t = string list
    end

    module Model = Int

    module Action = struct
      type t =
        | Increment
        | Decrement
      [@@deriving sexp_of]
    end

    module Result = struct
      type t = (int * string list) * (Action.t -> Event.t)
    end

    let apply_action ~inject:_ ~schedule_event:_ _words model : Action.t -> Model.t
      = function
        | Increment -> model + 1
        | Decrement -> model - 1
    ;;

    let compute ~inject words m =
      let res = m, List.filter words ~f:(fun s -> String.length s = m) in
      res, inject
    ;;

    let name = Source_code_position.to_string [%here]
  end
  in
  let component = Bonsai.of_module (module Words_counter_component) in
  let initial_input, initial_model = [], 0 in
  let driver = Driver.create ~initial_input ~initial_model component in
  let (module H) =
    Helpers.make_with_inject ~driver ~sexp_of_result:[%sexp_of: int * string list]
  in
  H.show ();
  [%expect "(0 ())"];
  H.set_input [ "a"; "b"; "c"; "aa"; "bbb"; "cccc" ];
  [%expect "(0 ())"];
  H.do_actions [ Words_counter_component.Action.Increment ];
  let _ = Words_counter_component.Action.Decrement in
  [%expect "(1 (a b c))"];
  H.do_actions [ Words_counter_component.Action.Increment ];
  [%expect "(2 (aa))"];
  H.do_actions [ Words_counter_component.Action.Increment ];
  [%expect "(3 (bbb))"];
  H.do_actions [ Words_counter_component.Action.Increment ];
  [%expect "(4 (cccc))"];
  H.set_input [ "aaaa"; "bbbb" ];
  [%expect {| (4 (aaaa bbbb)) |}]
;;

let%expect_test "compose, pure" =
  let open Bonsai.Infix in
  let component_a = Bonsai.pure_from_model ~f:(fun model -> model mod 5) in
  let component_b = Bonsai.pure ~f:(fun input -> input + 2) in
  let component = component_a >>> component_b in
  let driver = Driver.create ~initial_input:() ~initial_model:0 component in
  let (module H) = Helpers.make ~driver ~sexp_of_result:[%sexp_of: int] in
  H.show ();
  [%expect "2"];
  H.set_model 11;
  [%expect "3"]
;;

let%expect_test "pure_incr" =
  let open Bonsai.Infix in
  let component_a = Bonsai.pure_from_model ~f:(fun model -> model mod 5) in
  let component_b =
    Bonsai.Incremental.pure ~f:(fun input -> Incr.map input ~f:(fun i -> i + 2))
  in
  let component = component_a >>> component_b in
  let driver = Driver.create ~initial_input:() ~initial_model:0 component in
  let (module H) = Helpers.make ~driver ~sexp_of_result:[%sexp_of: int] in
  H.show ();
  [%expect "2"];
  H.set_model 11;
  [%expect "3"]
;;

let%expect_test "input projection" =
  let open Bonsai.Infix in
  let component = String.length @>> Bonsai.pure ~f:(fun input -> input + 1) in
  let driver = Driver.create ~initial_input:"hi" ~initial_model:() component in
  let (module H) = Helpers.make ~driver ~sexp_of_result:[%sexp_of: int] in
  H.show ();
  [%expect "3"];
  H.set_input "hello";
  [%expect "6"]
;;

let%expect_test "Project.Model.to_input_with_other" =
  let open Bonsai.Infix in
  let component =
    Bonsai.pure ~f:(fun (input, model) -> String.length input + model)
    |> Bonsai.Project.Model.to_input_with_other
    >>> Bonsai.pure ~f:(fun input -> input + 1)
  in
  let driver = Driver.create ~initial_input:"hi" ~initial_model:0 component in
  let (module H) = Helpers.make ~driver ~sexp_of_result:[%sexp_of: int] in
  H.show ();
  [%expect "3"];
  H.set_input "hello";
  [%expect "6"];
  H.set_model 10;
  [%expect "16"];
  H.set_input "hi";
  [%expect "13"]
;;

let%expect_test "project model to input" =
  let component =
    Bonsai.pure ~f:(fun input -> input + 1) |> Bonsai.Project.Model.to_input
  in
  let driver = Driver.create ~initial_input:() ~initial_model:0 component in
  let (module H) = Helpers.make ~driver ~sexp_of_result:[%sexp_of: int] in
  H.show ();
  [%expect "1"];
  H.set_model 3;
  [%expect "4"]
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
  let driver =
    Driver.create
      ~initial_input:()
      ~initial_model:(String.Map.of_alist_exn [ "a", 0; "b", 2 ])
      component
  in
  let (module H) = Helpers.make_string_with_inject ~driver in
  H.show ();
  [%expect {| "((a 0) (b 2))" |}];
  H.do_actions [ "a", Counter_component.Action.Increment ];
  [%expect {| "((a 1) (b 2))" |}]
;;

let%expect_test "assoc on input" =
  let component = Bonsai.pure ~f:(fun x -> x + 1) |> Bonsai.Map.assoc_input in
  let driver =
    Driver.create
      ~initial_input:(String.Map.of_alist_exn [ "a", 0; "b", 2 ])
      ~initial_model:()
      component
  in
  let (module H) = Helpers.make ~driver ~sexp_of_result:[%sexp_of: int String.Map.t] in
  H.show ();
  [%expect {| ((a 1) (b 3)) |}];
  H.set_input (String.Map.of_alist_exn [ "a", 1; "b", 2 ]);
  [%expect {| ((a 2) (b 3)) |}]
;;

let%expect_test "Incremental.of_incr" =
  let var = Incr.Var.create "hello" in
  let incr = Incr.Var.watch var in
  let component = Bonsai.Incremental.of_incr incr in
  let driver =
    Driver.create
      ~initial_input:(String.Map.of_alist_exn [ "a", 0; "b", 2 ])
      ~initial_model:()
      component
  in
  let (module H) = Helpers.make_string ~driver in
  H.show ();
  [%expect {| hello |}];
  Incr.Var.set var "world";
  Driver.flush driver;
  H.show ();
  [%expect {| world |}]
;;

let%expect_test "id" =
  let component = Bonsai.id in
  let driver = Driver.create ~initial_input:5 ~initial_model:"hello" component in
  let (module H) = Helpers.make ~driver ~sexp_of_result:[%sexp_of: int * string] in
  H.show ();
  [%expect {| (5 hello) |}];
  H.set_input 20;
  [%expect {| (20 hello) |}];
  H.set_model "world";
  [%expect {| (20 world) |}]
;;
