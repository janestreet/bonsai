open! Core_kernel
open! Import
include Helpers_intf

let sexp_to_string = Expect_test_helpers_core.sexp_to_string

let make_generic
      (type input extra action result s)
      ~(driver : (input, s) Driver.t)
      ~(string_of_result : result -> string)
      ~(get_result : s -> result)
      ~(get_extra : s -> extra)
      ~(schedule_action : s -> action -> unit)
  : (module S with type input = input and type action = action and type extra = extra)
  =
  (module struct
    type nonrec input = input
    type nonrec action = action
    type nonrec extra = extra

    let show () =
      driver |> Driver.result |> get_result |> string_of_result |> print_endline
    ;;

    let show_model () =

      (* Cleans up a sexp by
         - removing empty lists (unit models are common in bonsai)
         - flattening lists that contain a single element *)
      let rec clean_up_model_sexp a =
        match a with
        | Sexp.List l ->
          (match List.filter_map l ~f:clean_up_model_sexp with
           | [] -> None
           | [ l ] -> Some l
           | l -> Some (Sexp.List l))
        | Sexp.Atom _ as a -> Some a
      in
      driver
      |> Driver.sexp_of_model
      |> clean_up_model_sexp
      |> Option.value ~default:(Sexp.List [])
      |> sexp_to_string
      |> print_endline
    ;;

    let get_extra () = driver |> Driver.result |> get_extra


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

let make_vdom_generic
      (type input action extra s)
      ~(driver : (input, s) Driver.t)
      ~(vdom_of_result : s -> Vdom.Node.t)
      ~(get_extra : s -> extra)
      ~(inject_of_result : s -> action -> Vdom.Event.t)
      ?(vdom_to_string =
        fun node ->
          node
          |> Virtual_dom_test_helpers.Node_helpers.unsafe_convert_exn
          |> Virtual_dom_test_helpers.Node_helpers.to_string_html)
      ()
  : (module S_vdom
      with type input = input
       and type action = action
       and type extra = extra)
  =
  let open Virtual_dom_test_helpers in
  let (module H) =
    make_generic
      ~driver
      ~string_of_result:vdom_to_string
      ~get_result:vdom_of_result
      ~get_extra
      ~schedule_action:(fun s action ->
        Driver.schedule_event driver ((inject_of_result s) action))
  in
  (module struct
    include H

    let get_element ~selector =
      let node =
        driver |> Driver.result |> vdom_of_result |> Node_helpers.unsafe_convert_exn
      in
      Node_helpers.select_first_exn node ~selector
    ;;

    let click_on ~selector =
      let element = get_element ~selector in
      Node_helpers.User_actions.click_on element;
      Driver.flush driver
    ;;

    let input_text ~selector ~text =
      let element = get_element ~selector in
      Node_helpers.User_actions.input_text element ~text;
      Driver.flush driver
    ;;
  end)
;;

let make_vdom_with_inject ?vdom_to_string ~driver =
  make_vdom_generic
    ?vdom_to_string
    ~driver
    ~get_extra:(Fn.const ())
    ~vdom_of_result:Tuple2.get1
    ~inject_of_result:Tuple2.get2
    ()
;;

let make_vdom_with_extra ?vdom_to_string ~driver =
  make_vdom_generic
    ?vdom_to_string
    ~driver
    ~vdom_of_result:Tuple2.get1
    ~get_extra:Tuple2.get2
    ~inject_of_result:(Fn.const Nothing.unreachable_code)
    ()
;;

let make_vdom ?vdom_to_string ~driver =
  make_vdom_generic
    ?vdom_to_string
    ~driver
    ~get_extra:(Fn.const ())
    ~vdom_of_result:Fn.id
    ~inject_of_result:(Fn.const Nothing.unreachable_code)
    ()
;;

let make_string ~driver =
  make_generic
    ~driver
    ~string_of_result:Fn.id
    ~get_result:Fn.id
    ~get_extra:(Fn.const ())
    ~schedule_action:(Fn.const Nothing.unreachable_code)
;;

let make ~driver ~sexp_of_result =
  make_generic
    ~driver
    ~string_of_result:(fun r -> r |> sexp_of_result |> sexp_to_string)
    ~get_result:Fn.id
    ~get_extra:(Fn.const ())
    ~schedule_action:(Fn.const Nothing.unreachable_code)
;;

let make_string_with_inject ~driver =
  make_generic
    ~driver
    ~string_of_result:Fn.id
    ~get_result:fst
    ~get_extra:(Fn.const ())
    ~schedule_action:(fun (_, inject) action ->
      Driver.schedule_event driver (inject action))
;;

let make_with_inject ~driver ~sexp_of_result =
  make_generic
    ~driver
    ~string_of_result:(fun r -> r |> sexp_of_result |> sexp_to_string)
    ~get_result:fst
    ~get_extra:(Fn.const ())
    ~schedule_action:(fun (_, inject) action ->
      Driver.schedule_event driver (inject action))
;;
