open! Core_kernel
open! Import
open Composition_infix
include Helpers_intf

let sexp_to_string = Expect_test_helpers_core.sexp_to_string

let make_generic
      (type input action result s)
      ~(driver : (input, s) Driver.t)
      ~(string_of_result : result -> string)
      ~(get_result : s -> result)
      ~(schedule_action : s -> action -> unit)
  : (module S with type input = input and type action = action)
  =
  (module struct
    type nonrec input = input
    type nonrec action = action

    let show () =
      driver |> Driver.result |> get_result |> string_of_result |> print_endline
    ;;

    let set_input input =
      Driver.set_input driver input;
      Driver.flush driver;
      show ()
    ;;

    let do_actions actions =
      List.iter actions ~f:(schedule_action (Driver.result driver));
      Driver.flush driver;
      show ()
    ;;
  end)
;;

let make_string ~driver =
  make_generic
    ~driver
    ~string_of_result:Fn.id
    ~get_result:Fn.id
    ~schedule_action:(Fn.const Nothing.unreachable_code)
;;

let make ~driver ~sexp_of_result =
  make_generic
    ~driver
    ~string_of_result:(sexp_of_result >> sexp_to_string)
    ~get_result:Fn.id
    ~schedule_action:(Fn.const Nothing.unreachable_code)
;;

let make_string_with_inject ~driver =
  make_generic
    ~driver
    ~string_of_result:Fn.id
    ~get_result:fst
    ~schedule_action:(fun (_, inject) action ->
      Driver.schedule_event driver (inject action))
;;

let make_with_inject ~driver ~sexp_of_result =
  make_generic
    ~driver
    ~string_of_result:(sexp_of_result >> sexp_to_string)
    ~get_result:fst
    ~schedule_action:(fun (_, inject) action ->
      Driver.schedule_event driver (inject action))
;;
