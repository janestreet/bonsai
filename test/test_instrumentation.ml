module Bonsai_lib = Bonsai
open! Bonsai_lib
open! Proc
open! Bonsai.Let_syntax
open! Core

let start_timer label = print_endline [%string "start-%{label}"]
let stop_timer label = print_endline [%string "stop-%{label}"]

let instrument_computation component =
  Bonsai.Debug.instrument_computation component ~start_timer ~stop_timer
;;

let%expect_test "cutoff" =
  let var = Bonsai.Var.create 0 in
  let value = Bonsai.Var.value var in
  let component = return @@ Value.cutoff value ~equal:(fun _ _ -> false) in
  let timed_component = instrument_computation component in
  let handle = Handle.create (Result_spec.string (module Int)) timed_component in
  Handle.show handle;
  [%expect {|
    0 |}];
  Bonsai.Var.set var 2;
  Handle.show handle;
  [%expect {|
    start-1-return-cutoff
    stop-1-return-cutoff
    2 |}];
  Bonsai.Var.set var 1;
  Handle.show handle;
  [%expect {|
    start-1-return-cutoff
    stop-1-return-cutoff
    1 |}]
;;

let%expect_test "sub and map" =
  let component =
    let%sub x = Bonsai.const () in
    return
    @@ let%map () = x in
    ()
  in
  let timed_component = instrument_computation component in
  let handle = Handle.create (Result_spec.string (module Unit)) timed_component in
  Handle.show handle;
  [%expect
    {|
    start-5-subst(into)-return-map
    stop-5-subst(into)-return-map
    () |}];
  let component =
    let%sub x1 = Bonsai.const () in
    let%sub x2 = Bonsai.const () in
    let%sub x3 = Bonsai.const () in
    let%sub x4 = Bonsai.const () in
    return
    @@ let%mapn () = x1
    and () = x2
    and () = x3
    and () = x4 in
    ()
  in
  let timed_component = instrument_computation component in
  let handle = Handle.create (Result_spec.string (module Unit)) timed_component in
  Handle.show handle;
  [%expect
    {|
    start-14-subst(into)-subst(into)-subst(into)-subst(into)-return-map4
    stop-14-subst(into)-subst(into)-subst(into)-subst(into)-return-map4
    () |}]
;;

let%expect_test "assoc" =
  let component =
    Bonsai.assoc
      (module Int)
      (Value.return (Int.Map.of_alist_exn [ -1, (); 1, () ]))
      ~f:(fun i _ ->
        if%sub
          let%map i = i in
          i < 0
        then Bonsai.const 0
        else Bonsai.const 1)
  in
  let timed_component = instrument_computation component in
  let handle =
    Handle.create
      (Result_spec.sexp
         (module struct
           type t = int Int.Map.t [@@deriving sexp_of]
         end))
      timed_component
  in
  Handle.show handle;
  [%expect
    {|
    start-37-assoc-subst(from)-return-map
    stop-37-assoc-subst(from)-return-map
    start-37-assoc-subst(from)-return-map
    stop-37-assoc-subst(from)-return-map
    start-34-assoc-subst(into)-enum-map
    stop-34-assoc-subst(into)-enum-map
    start-34-assoc-subst(into)-enum-map
    stop-34-assoc-subst(into)-enum-map
    ((-1 0) (1 1)) |}]
;;

let%expect_test "nested values" =
  let component input =
    let value = Value.return 0 in
    let value1 =
      let%map value = value
      and input = input in
      value + input
    in
    let value2 =
      let%map value = value
      and input = input in
      value + (input % 2)
    in
    return (Value.both value1 value2)
  in
  let var = Bonsai.Var.create 0 in
  let value = Bonsai.Var.value var in
  let component = component value in
  let timed_component = instrument_computation component in
  let handle =
    Handle.create
      (Result_spec.sexp
         (module struct
           type t = int * int [@@deriving sexp_of]
         end))
      timed_component
  in
  Handle.show handle;
  [%expect
    {|
    start-42-return-both-map
    stop-42-return-both-map
    start-46-return-both-map
    stop-46-return-both-map
    (0 0) |}];
  Bonsai.Var.set var 2;
  Handle.show handle;
  [%expect
    {|
    start-46-return-both-map
    stop-46-return-both-map
    start-42-return-both-map
    stop-42-return-both-map
    (2 0) |}];
  Handle.show handle;
  [%expect {| (2 0) |}]
;;

let%expect_test "state times apply_action and compute" =
  let component = Bonsai.state [%here] (module Int) ~default_model:0 in
  let timed_component = instrument_computation component in
  let handle =
    Handle.create
      (module struct
        type t = int * (int -> unit Effect.t)
        type incoming = int

        let view (state, _) = [%string "%{state#Int}"]
        let incoming (state, set_state) add = set_state (state + add)
      end)
      timed_component
  in
  Handle.show handle;
  [%expect {|
    start-50-leaf0-compute
    stop-50-leaf0-compute
    0 |}];
  Handle.show handle;
  [%expect {| 0 |}];
  Handle.do_actions handle [ 5 ];
  Handle.show handle;
  [%expect
    {|
    start-50-leaf0-apply_action
    stop-50-leaf0-apply_action
    start-50-leaf0-compute
    stop-50-leaf0-compute
    5 |}]
;;

let%expect_test "enum" =
  let bool_var = Bonsai.Var.create true in
  let bool_value = Bonsai.Var.value bool_var in
  let component input =
    Bonsai.enum
      (module Bool)
      ~match_:bool_value
      ~with_:(function
        | true ->
          return
          @@ let%map input = input in
          input
        | false -> Bonsai.const 2)
  in
  let input_var = Bonsai.Var.create 0 in
  let input_value = Bonsai.Var.value input_var in
  let timed_component = instrument_computation (component input_value) in
  let handle =
    Handle.create
      (Result_spec.sexp
         (module struct
           type t = int [@@deriving sexp_of]
         end))
      timed_component
  in
  Handle.show handle;
  [%expect {|
    start-55-enum-return-map
    stop-55-enum-return-map
    0 |}];
  Bonsai.Var.set bool_var false;
  Handle.show handle;
  [%expect {| 2 |}];
  Bonsai.Var.set input_var 5;
  Handle.show handle;
  [%expect {| 2 |}];
  Bonsai.Var.set bool_var true;
  Handle.show handle;
  [%expect {|
    start-55-enum-return-map
    stop-55-enum-return-map
    5 |}]
;;

let%expect_test "only instrument some computations" =
  let map_var = Bonsai.Var.create Int.Map.empty in
  let map_value = Bonsai.Var.value map_var in
  let number_var = Bonsai.Var.create 1 in
  let number_value = Bonsai.Var.value number_var in
  let component =
    let%sub instrumented_assoc =
      instrument_computation
        (Bonsai.assoc
           (module Int)
           map_value
           ~f:(fun k v ->
             return
               (let%map k = k
                and v = v in
                print_endline "instrumented map";
                k * v)))
    in
    return
    @@ let%map instrumented_assoc = instrumented_assoc
    and number = number_value in
    print_endline "non-instrument map";
    match Map.find instrumented_assoc number with
    | Some v -> v + number
    | None -> number
  in
  let handle =
    Handle.create
      (Result_spec.sexp
         (module struct
           type t = int [@@deriving sexp_of]
         end))
      component
  in
  Handle.show handle;
  [%expect {|
    non-instrument map
    1 |}];
  Bonsai.Var.set map_var (Int.Map.of_alist_exn [ 1, 1; 3, 3 ]);
  Handle.show handle;
  [%expect
    {|
    start-58-assoc_simpl-by
    instrumented map
    stop-58-assoc_simpl-by
    start-58-assoc_simpl-by
    instrumented map
    stop-58-assoc_simpl-by
    non-instrument map
    2
 |}];
  Bonsai.Var.update map_var ~f:(Map.add_exn ~key:(-1) ~data:5);
  Handle.show handle;
  [%expect
    {|
    start-58-assoc_simpl-by
    instrumented map
    stop-58-assoc_simpl-by
    non-instrument map
    2
|}];
  (* Doesn't trigger any timers since the second computation is not instrumented. *)
  Bonsai.Var.set number_var 5;
  Handle.show handle;
  [%expect {|
    non-instrument map
    5 |}];
  (* Triggers a recalculation of the assoc *)
  Bonsai.Var.update map_var ~f:(Map.set ~key:(-1) ~data:0);
  Handle.show handle;
  [%expect
    {|
    start-58-assoc_simpl-by
    instrumented map
    stop-58-assoc_simpl-by
    non-instrument map
    5 |}]
;;
