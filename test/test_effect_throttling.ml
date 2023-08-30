open! Core
open! Import
open Proc
open Bonsai.For_open

let create_handle component =
  Handle.create
    (module struct
      type t = int -> int Bonsai.Effect_throttling.Poll_result.t Effect.t
      type incoming = int

      let view _ = ""

      let incoming f query =
        let%bind.Effect result = f query in
        Effect.print_s
          [%message (query : int) (result : int Bonsai.Effect_throttling.Poll_result.t)]
      ;;
    end)
    component
;;

module Common (M : sig
  val poll
    :  ('a -> 'b Effect.t) Value.t
    -> ('a -> 'b Bonsai.Effect_throttling.Poll_result.t Effect.t) Computation.t
end) =
struct
  let%expect_test {| Effect_throttling.poll only runs one instance of an effect at a time |}
    =
    let qrt = Effect.For_testing.Query_response_tracker.create () in
    let respond q =
      Effect.For_testing.Query_response_tracker.maybe_respond qrt ~f:(fun _ -> Respond q)
    in
    let component =
      M.poll (Value.return (Effect.For_testing.of_query_response_tracker qrt))
    in
    let handle = create_handle component in
    Handle.do_actions handle [ 0; 1; 2 ];
    Handle.recompute_view handle;
    (* query 1 gets aborted because only 1 request can be enqueued at a time *)
    [%expect {| ((query 1) (result Aborted)) |}];
    respond 1;
    [%expect {| ((query 0) (result (Finished 1))) |}];
    respond 2;
    (* We call [recompute_view] after [respond 2] and before [respond 3] to
       demonstrate that the effect being responded to doesn't begin until the
       next time the state machine effects get run. This isn't necessarily
       desirable behavior, but it is the way this computation works, so it's
       worth showing in this test. *)
    Handle.recompute_view handle;
    [%expect {| |}];
    respond 3;
    [%expect {| ((query 2) (result (Finished 3))) |}]
  ;;

  let%expect_test {| Effect_throttling.poll resetting |} =
    let qrt = Effect.For_testing.Query_response_tracker.create () in
    let respond q =
      Effect.For_testing.Query_response_tracker.maybe_respond qrt ~f:(fun _ -> Respond q)
    in
    let effect_var =
      Bonsai.Var.create (Effect.For_testing.of_query_response_tracker qrt)
    in
    let component = Bonsai.with_model_resetter (M.poll (Bonsai.Var.value effect_var)) in
    let handle =
      Handle.create
        (module struct
          type t =
            (int -> int Bonsai.Effect_throttling.Poll_result.t Effect.t) * unit Effect.t

          type incoming =
            [ `Run of int
            | `Reset
            ]

          let view _ = ""

          let incoming (f, reset) query =
            match query with
            | `Run query ->
              let%bind.Effect result = f query in
              Effect.print_s
                [%message
                  (query : int) (result : int Bonsai.Effect_throttling.Poll_result.t)]
            | `Reset -> reset
          ;;
        end)
        component
    in
    Handle.do_actions handle [ `Run 0; `Run 1; `Run 2 ];
    Handle.recompute_view handle;
    [%expect {| ((query 1) (result Aborted)) |}];
    Handle.do_actions handle [ `Reset ];
    Handle.recompute_view handle;
    respond 1;
    Handle.recompute_view handle;
    [%expect {| ((query 0) (result (Finished 1))) |}];
    Handle.do_actions handle [ `Reset ];
    respond 2;
    [%expect {| ((query 2) (result (Finished 2))) |}]
  ;;
end

module _ = Common (struct
  let poll = Bonsai.Effect_throttling.poll
end)

module _ = Common (struct
  let poll effect =
    let open Bonsai.Let_syntax in
    let%sub effect = Bonsai.Effect_throttling.poll effect in
    let%sub effect = Bonsai.Effect_throttling.poll effect in
    let%arr effect = effect in
    fun int ->
      match%map.Effect effect int with
      | Aborted -> Bonsai.Effect_throttling.Poll_result.Aborted
      | Finished (Finished result) -> Finished result
      | Finished Aborted -> raise_s [%message "Unexpected finished of aborted"]
  ;;
end)

let%expect_test {| Effect_throttling.poll deactivation |} =
  let qrt = Effect.For_testing.Query_response_tracker.create () in
  let respond q =
    Effect.For_testing.Query_response_tracker.maybe_respond qrt ~f:(fun _ -> Respond q)
  in
  let effect_var = Bonsai.Var.create (Effect.For_testing.of_query_response_tracker qrt) in
  let match_var = Bonsai.Var.create true in
  let poll_effect = Bonsai.Effect_throttling.poll (Bonsai.Var.value effect_var) in
  let component =
    let open Bonsai.Let_syntax in
    if%sub Bonsai.Var.value match_var then poll_effect else poll_effect
  in
  let handle = create_handle component in
  (* The actions [ 0; 1; 2 ] are associated with the [true] branch, but don't get run
     until we flush/recompute_view. *)
  Handle.do_actions handle [ 0; 1; 2 ];
  Bonsai.Var.set match_var false;
  Handle.recompute_view handle;
  (* Since the [true] branch is inactive, no action is running, and they all count as
     "up_next". We will drop 0 and 1 because they will be replaced by 1 and 2,
     respectively, since we only allow one effect to be in the pending queue at a time *)
  [%expect {|
    ((query 0) (result Aborted))
    ((query 1) (result Aborted)) |}];
  (* The actions [ 3; 4; 5 ] are associated with the [false] branch*)
  Handle.do_actions handle [ 3; 4; 5 ];
  Handle.recompute_view handle;
  [%expect {| ((query 4) (result Aborted)) |}];
  respond 1;
  [%expect {| ((query 3) (result (Finished 1))) |}];
  (* The outstanding request doesn't get run until we flush/recompute_view so nothing is
     waiting for a response *)
  respond 2;
  [%expect {| |}];
  Handle.recompute_view handle;
  respond 3;
  [%expect {| ((query 5) (result (Finished 3))) |}];
  Handle.recompute_view handle;
  Bonsai.Var.set match_var true;
  Handle.recompute_view handle;
  Handle.recompute_view handle;
  respond 4;
  Handle.recompute_view handle;
  [%expect {| ((query 2) (result (Finished 4))) |}]
;;

let%expect_test {| Effect_throttling.poll gets activated and de-activated the next frame |}
  =
  let qrt = Effect.For_testing.Query_response_tracker.create () in
  let respond q =
    Effect.For_testing.Query_response_tracker.maybe_respond qrt ~f:(fun _ -> Respond q)
  in
  let effect_var = Bonsai.Var.create (Effect.For_testing.of_query_response_tracker qrt) in
  let match_var = Bonsai.Var.create true in
  let poll_effect = Bonsai.Effect_throttling.poll (Bonsai.Var.value effect_var) in
  let component =
    let open Bonsai.Let_syntax in
    if%sub Bonsai.Var.value match_var then poll_effect else poll_effect
  in
  let handle = create_handle component in
  Handle.do_actions handle [ 0 ];
  Bonsai.Var.set match_var false;
  Handle.recompute_view handle;
  [%expect {| |}];
  respond 1;
  Bonsai.Var.set match_var true;
  Handle.recompute_view handle;
  [%expect {| |}];
  respond 2;
  Bonsai.Var.set match_var false;
  Handle.recompute_view handle;
  [%expect {| |}];
  respond 3;
  Bonsai.Var.set match_var true;
  Handle.recompute_view handle;
  [%expect {| |}];
  respond 4;
  Handle.recompute_view handle;
  respond 5;
  [%expect {| ((query 0) (result (Finished 5))) |}]
;;

let%expect_test {| Effect_throttling.poll effect finishes while inactive and effect is queued |}
  =
  let qrt = Effect.For_testing.Query_response_tracker.create () in
  let respond q =
    Effect.For_testing.Query_response_tracker.maybe_respond qrt ~f:(fun _ -> Respond q)
  in
  let effect_var = Bonsai.Var.create (Effect.For_testing.of_query_response_tracker qrt) in
  let match_var = Bonsai.Var.create true in
  let poll_effect = Bonsai.Effect_throttling.poll (Bonsai.Var.value effect_var) in
  let component =
    let open Bonsai.Let_syntax in
    if%sub Bonsai.Var.value match_var then poll_effect else poll_effect
  in
  let handle = create_handle component in
  Handle.do_actions handle [ 0; 1 ];
  Handle.recompute_view handle;
  Handle.recompute_view handle;
  Handle.recompute_view handle;
  Bonsai.Var.set match_var false;
  [%expect {| |}];
  respond 1;
  Handle.recompute_view handle;
  [%expect {| ((query 0) (result (Finished 1))) |}];
  Bonsai.Var.set match_var true;
  Handle.recompute_view handle;
  [%expect {| |}];
  Handle.recompute_view handle;
  respond 2;
  [%expect {| ((query 1) (result (Finished 2))) |}]
;;

let%expect_test {| Effect_throttling.poll in an assoc |} =
  let qrt = Effect.For_testing.Query_response_tracker.create () in
  let respond q =
    Effect.For_testing.Query_response_tracker.maybe_respond qrt ~f:(fun _ -> Respond q)
  in
  let map_var = Bonsai.Var.create (Int.Map.of_alist_exn [ 1, (); 2, (); 3, () ]) in
  let effect_var = Bonsai.Var.create (Effect.For_testing.of_query_response_tracker qrt) in
  let component =
    let open Bonsai.Let_syntax in
    Bonsai.assoc
      (module Int)
      (Bonsai.Var.value map_var)
      ~f:(fun key _data ->
        let%sub poll_effect =
          Bonsai.Effect_throttling.poll (Bonsai.Var.value effect_var)
        in
        let%arr key = key
        and poll_effect = poll_effect in
        poll_effect key)
  in
  let handle =
    Handle.create
      (module struct
        type t = int Bonsai.Effect_throttling.Poll_result.t Effect.t Int.Map.t
        type incoming = int

        let view _ = ""

        let incoming map query =
          match Map.find map query with
          | Some effect ->
            let%bind.Effect result = effect in
            Effect.print_s
              [%message
                (query : int) (result : int Bonsai.Effect_throttling.Poll_result.t)]
          | None -> Effect.print_s [%message "Key not in map" (query : int)]
        ;;
      end)
      component
  in
  Handle.do_actions handle [ 1; 2; 3; 1; 2; 3; 2; 1; 3 ];
  Handle.recompute_view handle;
  [%expect
    {|
    ((query 2) (result Aborted))
    ((query 1) (result Aborted))
    ((query 3) (result Aborted)) |}];
  Handle.recompute_view handle;
  respond 1;
  [%expect
    {|
    ((query 3) (result (Finished 1)))
    ((query 2) (result (Finished 1)))
    ((query 1) (result (Finished 1))) |}];
  respond 2;
  Handle.recompute_view handle;
  respond 3;
  [%expect
    {|
    ((query 1) (result (Finished 3)))
    ((query 2) (result (Finished 3)))
    ((query 3) (result (Finished 3))) |}];
  Handle.do_actions handle [ 1; 2; 3; 1; 2; 3 ];
  Bonsai.Var.update map_var ~f:(fun map -> Map.remove map 1);
  Handle.recompute_view handle;
  [%expect {| ((query 1) (result Aborted)) |}];
  respond 4;
  [%expect
    {|
    ((query 3) (result (Finished 4)))
    ((query 2) (result (Finished 4))) |}]
;;

let%expect_test "collapse functions" =
  let print ?tag_s input =
    let output = Bonsai.Effect_throttling.Poll_result.collapse_to_or_error ?tag_s input in
    print_s
      [%message
        ""
          ~_:(input : unit Or_error.t Bonsai.Effect_throttling.Poll_result.t)
          "=>"
          ~_:(output : unit Or_error.t)]
  in
  print Aborted;
  print (Finished (Error (Error.of_string "oh no!")));
  print (Finished (Ok ()));
  [%expect
    {|
    (Aborted => (Error "request was aborted"))
    ((Finished (Error "oh no!")) => (Error "oh no!"))
    ((Finished (Ok ())) => (Ok ())) |}];
  print ~tag_s:(lazy (Sexp.Atom "my tag")) Aborted;
  print ~tag_s:(lazy (Sexp.Atom "my tag")) (Finished (Error (Error.of_string "oh no!")));
  [%expect
    {|
    (Aborted => (Error ("my tag" "request was aborted")))
    ((Finished (Error "oh no!")) => (Error ("my tag" "oh no!"))) |}]
;;
