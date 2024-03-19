open! Core
open! Import
open Bonsai_test
open Bonsai.For_open
open Bonsai.Let_syntax

(* A big focus of the tests in this file is about making sure that there are no
   "in-transit" frames - frames during which the result has some intermediate
   value because lifecycle events haven't yet run. To accomplish this goal, all
   the tests have been written in a [Common] functor, which accepts a module
   that specifies what [Handle.show] should do. We supply three different
   answers to that question:

   - it should do what it normally does.
   - it should recompute_view an extra time prior to calling Handle.show.
   - it should recompute_view_until_stable prior to calling Handle.show.

   We do this to ensure that all the functions being tested behave the same no
   matter which one of those options is chosen. *)

let advance_by_sec handle seconds =
  Handle.advance_clock_by handle (Time_ns.Span.of_sec seconds)
;;

let%test_module "Bonsai_extra.with_last_modified_time" =
  (module struct
    module Common (M : sig
      val with_last_modified_time
        :  equal:('a -> 'a -> bool)
        -> 'a Value.t
        -> ('a * Time_ns.t) Computation.t

      val show_handle : ('a, 'b) Handle.t -> unit
    end) =
    struct
      let show = M.show_handle

      let%expect_test _ =
        let v' = Bonsai.Var.create 1 in
        let v = Bonsai.Var.value v' in
        let c = M.with_last_modified_time ~equal:Int.equal v in
        let handle =
          Handle.create
            (Result_spec.sexp
               (module struct
                 type t = int * Time_ns.Alternate_sexp.t [@@deriving sexp]
               end))
            c
        in
        show handle;
        [%expect {| (1 "1970-01-01 00:00:00Z") |}];
        advance_by_sec handle 1.0;
        show handle;
        [%expect {| (1 "1970-01-01 00:00:00Z") |}];
        Bonsai.Var.set v' 2;
        show handle;
        [%expect {| (2 "1970-01-01 00:00:01Z") |}];
        Bonsai.Var.set v' 3;
        advance_by_sec handle 1.0;
        show handle;
        [%expect {| (3 "1970-01-01 00:00:02Z") |}];
        show handle;
        [%expect {| (3 "1970-01-01 00:00:02Z") |}]
      ;;

      let%expect_test _ =
        let v' = Bonsai.Var.create 1 in
        let on' = Bonsai.Var.create true in
        let v = Bonsai.Var.value v' in
        let on = Bonsai.Var.value on' in
        let c =
          match%sub on with
          | true ->
            let%sub x = M.with_last_modified_time ~equal:Int.equal v in
            let%arr x = x in
            Some x
          | false -> Bonsai.const None
        in
        let handle =
          Handle.create
            (Result_spec.sexp
               (module struct
                 type t = (int * Time_ns.Alternate_sexp.t) option [@@deriving sexp]
               end))
            c
        in
        show handle;
        [%expect {| ((1 "1970-01-01 00:00:00Z")) |}];
        advance_by_sec handle 1.0;
        show handle;
        [%expect {| ((1 "1970-01-01 00:00:00Z")) |}];
        Bonsai.Var.set on' false;
        show handle;
        [%expect {| () |}];
        Bonsai.Var.set on' true;
        show handle;
        [%expect {| ((1 "1970-01-01 00:00:01Z")) |}];
        Bonsai.Var.set on' false;
        show handle;
        [%expect {| () |}];
        advance_by_sec handle 1.0;
        show handle;
        [%expect {| () |}];
        Bonsai.Var.set on' true;
        show handle;
        [%expect {| ((1 "1970-01-01 00:00:02Z")) |}];
        advance_by_sec handle 1.0;
        show handle;
        [%expect {| ((1 "1970-01-01 00:00:02Z")) |}];
        Bonsai.Var.set v' 2;
        show handle;
        [%expect {| ((2 "1970-01-01 00:00:03Z")) |}]
      ;;
    end

    module _ = Common (struct
      let with_last_modified_time = Bonsai_extra.with_last_modified_time
      let show_handle = Handle.show
    end)

    module _ = Common (struct
      let with_last_modified_time = Bonsai_extra.with_last_modified_time

      let show_handle handle =
        Handle.recompute_view handle;
        Handle.show handle
      ;;
    end)

    module _ = Common (struct
      let with_last_modified_time = Bonsai_extra.with_last_modified_time

      let show_handle handle =
        Handle.recompute_view_until_stable handle;
        Handle.show handle
      ;;
    end)
  end)
;;

let%test_module "Bonsai_extra.is_stable" =
  (module struct
    module Common (M : sig
      val is_stable
        :  equal:('a -> 'a -> bool)
        -> 'a Value.t
        -> time_to_stable:Time_ns.Span.t Value.t
        -> bool Computation.t

      val show_handle : ('a, 'b) Handle.t -> unit
    end) =
    struct
      let show = M.show_handle

      type controls =
        { v' : int Bonsai.Var.t
        ; on' : bool Bonsai.Var.t
        ; span : Time_ns.Span.t Bonsai.Var.t
        }

      let gen_handle ~initial_span_secs =
        let span = Bonsai.Var.create (Time_ns.Span.of_sec initial_span_secs) in
        let v' = Bonsai.Var.create 1 in
        let on' = Bonsai.Var.create true in
        let controls = { span; v'; on' } in
        let v = Bonsai.Var.value v' in
        let on = Bonsai.Var.value on' in
        let c =
          match%sub on with
          | true ->
            let%sub x =
              M.is_stable ~equal:Int.equal v ~time_to_stable:(Bonsai.Var.value span)
            in
            let%arr x = x
            and v = v in
            Some (x, v)
          | false -> Bonsai.const None
        in
        let handle =
          Handle.create
            (Result_spec.sexp
               (module struct
                 type t = (bool * int) option [@@deriving sexp]
               end))
            c
        in
        handle, controls
      ;;

      let%expect_test {|advancing time and value interactions|} =
        let handle, controls = gen_handle ~initial_span_secs:1.0 in
        show handle;
        [%expect {| ((false 1)) |}];
        advance_by_sec handle 1.0;
        show handle;
        [%expect {| ((true 1)) |}];
        Bonsai.Var.set controls.v' 2;
        show handle;
        [%expect {| ((false 2)) |}];
        advance_by_sec handle 0.5;
        show handle;
        [%expect {| ((false 2)) |}];
        Bonsai.Var.set controls.v' 3;
        show handle;
        [%expect {| ((false 3)) |}];
        advance_by_sec handle 0.5;
        show handle;
        [%expect {| ((false 3)) |}];
        advance_by_sec handle 0.5;
        show handle;
        [%expect {| ((true 3)) |}];
        Bonsai.Var.set controls.v' 4;
        show handle;
        [%expect {| ((false 4)) |}];
        advance_by_sec handle 1.0;
        Bonsai.Var.set controls.v' 5;
        show handle;
        [%expect {| ((false 5)) |}];
        advance_by_sec handle 1.0;
        show handle;
        [%expect {| ((true 5)) |}];
        advance_by_sec handle 0.5;
        Bonsai.Var.set controls.v' 4;
        show handle;
        [%expect {| ((false 4)) |}];
        advance_by_sec handle 0.5;
        Bonsai.Var.set controls.v' 5;
        show handle;
        [%expect {| ((false 5)) |}]
      ;;

      let%expect_test {|advancing time, value interactions, and enabling/disabling the computation|}
        =
        let handle, controls = gen_handle ~initial_span_secs:1.0 in
        show handle;
        [%expect {| ((false 1)) |}];
        advance_by_sec handle 1.0;
        show handle;
        [%expect {| ((true 1)) |}];
        Bonsai.Var.set controls.on' false;
        show handle;
        [%expect {| () |}];
        Bonsai.Var.set controls.on' true;
        show handle;
        [%expect {| ((false 1)) |}]
      ;;

      (* The order of lifecycle effect prints and handler show prints
         depends on if the view is recomputed. This standardizes things. *)
      let print_sorted_expect_test_output expect_output =
        expect_output
        |> String.split_lines
        |> List.sort ~compare:String.compare
        |> String.concat_lines
        |> print_endline
      ;;

      let%expect_test {|zero values for the timespan should be permitted (but issue a warning) and always return false |}
        =
        let handle, controls = gen_handle ~initial_span_secs:0.0 in
        show handle;
        print_sorted_expect_test_output [%expect.output];
        [%expect {| ((true 1)) |}];
        advance_by_sec handle 1.0;
        show handle;
        [%expect {| ((true 1)) |}];
        Bonsai.Var.set controls.on' false;
        show handle;
        [%expect {| () |}];
        Bonsai.Var.set controls.on' true;
        show handle;
        print_sorted_expect_test_output [%expect.output];
        [%expect {| ((true 1)) |}]
      ;;

      let%expect_test {|negative values for the timespan should be permitted (but issue a warning) and always return false |}
        =
        let handle, _ = gen_handle ~initial_span_secs:(-1.0) in
        show handle;
        print_sorted_expect_test_output [%expect.output];
        [%expect
          {|
          "Bonsai_extra.is_stable: [time_to_stable] should not be negative"
          ((true 1))
          |}]
      ;;

      let%expect_test {|changing span |} =
        let handle, controls = gen_handle ~initial_span_secs:1.0 in
        show handle;
        [%expect {| ((false 1)) |}];
        advance_by_sec handle 2.;
        show handle;
        [%expect {| ((true 1)) |}];
        Bonsai.Var.set controls.v' 4;
        Bonsai.Var.set controls.span (Time_ns.Span.of_sec 5.);
        show handle;
        [%expect {| ((false 4)) |}];
        advance_by_sec handle 2.;
        show handle;
        [%expect {| ((false 4)) |}];
        advance_by_sec handle 4.;
        show handle;
        [%expect {| ((true 4)) |}];
        Bonsai.Var.set controls.v' 6;
        Bonsai.Var.set controls.span (Time_ns.Span.of_sec 0.);
        show handle;
        print_sorted_expect_test_output [%expect.output];
        [%expect {| ((true 6)) |}];
        Bonsai.Var.set controls.v' 999;
        Bonsai.Var.set controls.span (Time_ns.Span.of_sec (-1.));
        show handle;
        print_sorted_expect_test_output [%expect.output];
        [%expect
          {|
          "Bonsai_extra.is_stable: [time_to_stable] should not be negative"
          ((true 999))
          |}];
        Bonsai.Var.set controls.v' 17;
        Bonsai.Var.set controls.span (Time_ns.Span.of_sec 1.);
        show handle;
        [%expect {| ((false 17)) |}];
        advance_by_sec handle 2.;
        show handle;
        [%expect {| ((true 17)) |}]
      ;;

      let%expect_test {| keeping the value the same and changing the span |} =
        let handle, controls = gen_handle ~initial_span_secs:1.0 in
        show handle;
        [%expect {| ((false 1)) |}];
        advance_by_sec handle 1.;
        show handle;
        [%expect {| ((true 1)) |}];
        Bonsai.Var.set controls.span (Time_ns.Span.of_sec 1.5);
        show handle;
        [%expect {| ((false 1)) |}];
        Bonsai.Var.set controls.span (Time_ns.Span.of_sec 0.5);
        show handle;
        [%expect {| ((true 1)) |}]
      ;;
    end

    module _ = Common (struct
      let is_stable = Bonsai_extra.is_stable
      let show_handle = Handle.show
    end)

    module _ = Common (struct
      let is_stable = Bonsai_extra.is_stable

      let show_handle handle =
        Handle.recompute_view handle;
        Handle.show handle
      ;;
    end)

    module _ = Common (struct
      let is_stable = Bonsai_extra.is_stable

      let show_handle handle =
        Handle.recompute_view_until_stable handle;
        Handle.show handle
      ;;
    end)
  end)
;;

let%test_module "Bonsai.most_recent_value_satisfying" =
  (module struct
    module Common (M : sig
      val most_recent_value_satisfying
        :  ?sexp_of_model:('a -> Sexp.t)
        -> equal:('a -> 'a -> bool)
        -> 'a Value.t
        -> condition:('a -> bool)
        -> 'a option Computation.t

      val show_handle : ('a, 'b) Handle.t -> unit
    end) =
    struct
      let show = M.show_handle

      let%expect_test _ =
        let v' = Bonsai.Var.create 1 in
        let v = Bonsai.Var.value v' in
        let c =
          M.most_recent_value_satisfying
            ~sexp_of_model:[%sexp_of: Int.t]
            ~equal:[%equal: Int.t]
            v
            ~condition:(fun x -> x % 2 = 0)
        in
        let handle =
          Handle.create
            (Result_spec.sexp
               (module struct
                 type t = int option [@@deriving sexp]
               end))
            c
        in
        show handle;
        [%expect {| () |}];
        Bonsai.Var.set v' 2;
        show handle;
        [%expect {| (2) |}];
        Bonsai.Var.set v' 3;
        show handle;
        [%expect {| (2) |}];
        Bonsai.Var.set v' 4;
        show handle;
        [%expect {| (4) |}]
      ;;

      let%expect_test _ =
        let v' = Bonsai.Var.create 1 in
        let on' = Bonsai.Var.create true in
        let v = Bonsai.Var.value v' in
        let on = Bonsai.Var.value on' in
        let c =
          match%sub on with
          | true ->
            let%sub x =
              M.most_recent_value_satisfying
                ~sexp_of_model:[%sexp_of: Int.t]
                ~equal:[%equal: Int.t]
                v
                ~condition:(fun x -> x % 2 = 0)
            in
            let%arr x = x in
            Some x
          | false -> Bonsai.const None
        in
        let handle =
          Handle.create
            (Result_spec.sexp
               (module struct
                 type t = int option option [@@deriving sexp]
               end))
            c
        in
        show handle;
        [%expect {| (()) |}];
        Bonsai.Var.set v' 2;
        show handle;
        [%expect {| ((2)) |}];
        Bonsai.Var.set on' false;
        show handle;
        [%expect {| () |}];
        Bonsai.Var.set v' 3;
        show handle;
        [%expect {| () |}];
        Bonsai.Var.set on' true;
        show handle;
        [%expect {| ((2)) |}]
      ;;
    end

    module _ = Common (struct
      let most_recent_value_satisfying = Bonsai.most_recent_value_satisfying
      let show_handle = Handle.show
    end)

    module _ = Common (struct
      let most_recent_value_satisfying = Bonsai.most_recent_value_satisfying

      let show_handle handle =
        Handle.recompute_view handle;
        Handle.show handle
      ;;
    end)

    module _ = Common (struct
      let most_recent_value_satisfying = Bonsai.most_recent_value_satisfying

      let show_handle handle =
        Handle.recompute_view_until_stable handle;
        Handle.show handle
      ;;
    end)
  end)
;;

let%test_module "Bonsai_extra.value_stability" =
  (module struct
    let alternate_value_stability_implementation
      (type a)
      ?sexp_of_model
      ~equal
      input
      ~time_to_stable
      =
      let module M = struct
        type t = a

        let sexp_of_t = Option.value ~default:sexp_of_opaque sexp_of_model
      end
      in
      let%sub input =
        (* apply cutoff as an optimistic performance improvement *)
        Bonsai.Incr.value_cutoff input ~equal
      in
      let module T = struct
        module Model = struct
          let equal_a = equal
          let sexp_of_a = M.sexp_of_t

          type stability =
            | Inactive of { previously_stable : a option }
            | Unstable of
                { previously_stable : a option
                ; unstable_value : a
                }
            | Stable of a
          [@@deriving sexp_of, equal]

          let set_value new_value = function
            | Inactive { previously_stable } ->
              Unstable { previously_stable; unstable_value = new_value }
            | Stable stable ->
              Unstable { previously_stable = Some stable; unstable_value = new_value }
            | Unstable { previously_stable; unstable_value = _ } ->
              Unstable { previously_stable; unstable_value = new_value }
          ;;

          type t =
            { stability : stability
            ; time_to_next_stable : Time_ns.Alternate_sexp.t option
            }
          [@@deriving sexp_of, equal]

          let default =
            { stability = Inactive { previously_stable = None }
            ; time_to_next_stable = None
            }
          ;;
        end

        module Action = struct
          type t =
            | Deactivate
            | Bounce of M.t * Time_ns.Alternate_sexp.t
            | Set_stable of M.t * Time_ns.Alternate_sexp.t
          [@@deriving sexp_of]
        end
      end
      in
      let open T in
      let%sub { stability; time_to_next_stable }, inject =
        Bonsai.state_machine1
          time_to_stable
          ~sexp_of_model:[%sexp_of: Model.t]
          ~equal:[%equal: Model.t]
          ~sexp_of_action:[%sexp_of: Action.t]
          ~default_model:Model.default
          ~apply_action:
            (fun
              (_ : _ Bonsai.Apply_action_context.t) time_to_stable model action ->
          match action, model, time_to_stable with
          | _, _, Inactive -> model
          | Deactivate, { stability; _ }, _ ->
            let stability =
              match stability with
              | Inactive _ -> stability
              | Unstable { previously_stable; _ } -> Inactive { previously_stable }
              | Stable stable -> Inactive { previously_stable = Some stable }
            in
            (* Deactivating this component will automatically cause the value to be
                   considered unstable.  This is because we have no way to tell what is
                   happening to the value when this component is inactive, and I consider
                   it safer to assume instability than to assume stability. *)
            { stability; time_to_next_stable = None }
          | Bounce (new_value, now), { stability; _ }, Active time_to_stable ->
            (* Bouncing will cause the value to become unstable, and set the
                   time-to-next-stable to the provided value. *)
            let stability = Model.set_value new_value stability in
            let time_to_next_stable = Some (Time_ns.add now time_to_stable) in
            { stability; time_to_next_stable }
          | ( Set_stable (stable, now)
            , { stability; time_to_next_stable }
            , Active time_to_stable ) ->
            (* Sets the value which is considered to be stable and resets
                   the time until next stability. *)
            (match stability with
             | Inactive { previously_stable } ->
               { stability = Unstable { previously_stable; unstable_value = stable }
               ; time_to_next_stable = Some (Time_ns.add now time_to_stable)
               }
             | Stable previously_stable ->
               if equal previously_stable stable
               then { stability = Stable stable; time_to_next_stable = None }
               else
                 { stability =
                     Unstable
                       { unstable_value = stable
                       ; previously_stable = Some previously_stable
                       }
                 ; time_to_next_stable = Some (Time_ns.add now time_to_stable)
                 }
             | Unstable { unstable_value; previously_stable } ->
               let candidate_time_to_next_stable = Time_ns.add now time_to_stable in
               (match equal unstable_value stable, time_to_next_stable with
                | true, Some time_to_next_stable
                  when Time_ns.( >= ) now time_to_next_stable ->
                  { stability = Stable stable; time_to_next_stable = None }
                | _ ->
                  { stability = Unstable { unstable_value = stable; previously_stable }
                  ; time_to_next_stable = Some candidate_time_to_next_stable
                  })))
      in
      let%sub get_current_time = Bonsai.Clock.get_current_time in
      let%sub bounce =
        (* [bounce] is an effect which, when scheduled, will bounce the
           state-machine and set the time-until-stable to the current wallclock
           time plus the provided offset *)
        let%arr get_current_time = get_current_time
        and inject = inject
        and input = input in
        let%bind.Effect now = get_current_time in
        inject (Bounce (input, now))
      in
      let%sub () =
        (* the input value changing triggers a bounce *)
        let%sub callback =
          let%arr bounce = bounce in
          fun _ -> bounce
        in
        Bonsai.Edge.on_change ~sexp_of_model:[%sexp_of: M.t] ~equal input ~callback
      in
      let%sub () =
        let%sub on_deactivate =
          let%arr inject = inject in
          inject Deactivate
        in
        (* activating the component bounces it to reset the timer *)
        Bonsai.Edge.lifecycle ~on_deactivate ~on_activate:bounce ()
      in
      let%sub () =
        match%sub time_to_next_stable with
        | None -> Bonsai.const ()
        | Some next_stable ->
          let%sub callback =
            let%arr inject = inject
            and input = input
            and get_current_time = get_current_time
            and bounce = bounce in
            fun (prev : Bonsai.Clock.Before_or_after.t option)
                (cur : Bonsai.Clock.Before_or_after.t) ->
              match prev, cur with
              | Some Before, After ->
                let%bind.Effect now = get_current_time in
                inject (Set_stable (input, now))
              | None, After ->
                print_s [%message "BUG" [%here] "clock moves straight to 'after'"];
                bounce
              | _ -> Effect.Ignore
          in
          let%sub before_or_after = Bonsai.Clock.at next_stable in
          Bonsai.Edge.on_change'
            ~sexp_of_model:[%sexp_of: Bonsai.Clock.Before_or_after.t]
            ~equal:[%equal: Bonsai.Clock.Before_or_after.t]
            before_or_after
            ~callback
      in
      let%arr stability = stability
      and input = input in
      match stability with
      | Stable input' when equal input' input -> Bonsai_extra.Stability.Stable input
      | Stable previously_stable ->
        (* Even if the state-machine claims that the value is stable, we can still witness
           instability one frame before the lifecycle events run. *)
        Unstable { previously_stable = Some previously_stable; unstable_value = input }
      | Unstable { previously_stable; unstable_value = _ } ->
        Unstable { previously_stable; unstable_value = input }
      | Inactive { previously_stable } ->
        Unstable { previously_stable; unstable_value = input }
    ;;

    module Common (M : sig
      val value_stability
        :  ?sexp_of_model:('a -> Sexp.t)
        -> equal:('a -> 'a -> bool)
        -> 'a Value.t
        -> time_to_stable:Time_ns.Span.t Value.t
        -> 'a Bonsai_extra.Stability.t Computation.t

      val show_handle : ('a, 'b) Handle.t -> unit
    end) =
    struct
      let show = M.show_handle

      let%expect_test _ =
        let v' = Bonsai.Var.create 1 in
        let v = Bonsai.Var.value v' in
        let c =
          M.value_stability
            ~sexp_of_model:[%sexp_of: Int.t]
            ~equal:[%equal: Int.t]
            v
            ~time_to_stable:(Value.return (Time_ns.Span.of_sec 1.0))
        in
        let handle =
          Handle.create
            (Result_spec.sexp
               (module struct
                 type t = int Bonsai_extra.Stability.t [@@deriving sexp]
               end))
            c
        in
        show handle;
        [%expect {| (Unstable (previously_stable ()) (unstable_value 1)) |}];
        advance_by_sec handle 1.0;
        show handle;
        [%expect {| (Stable 1) |}];
        Bonsai.Var.set v' 2;
        show handle;
        [%expect {| (Unstable (previously_stable (1)) (unstable_value 2)) |}];
        advance_by_sec handle 0.5;
        show handle;
        [%expect {| (Unstable (previously_stable (1)) (unstable_value 2)) |}];
        Bonsai.Var.set v' 3;
        show handle;
        [%expect {| (Unstable (previously_stable (1)) (unstable_value 3)) |}];
        advance_by_sec handle 0.5;
        show handle;
        [%expect {| (Unstable (previously_stable (1)) (unstable_value 3)) |}];
        advance_by_sec handle 0.5;
        show handle;
        [%expect {| (Stable 3) |}];
        Bonsai.Var.set v' 4;
        show handle;
        [%expect {| (Unstable (previously_stable (3)) (unstable_value 4)) |}];
        advance_by_sec handle 1.0;
        Bonsai.Var.set v' 5;
        show handle;
        [%expect {| (Unstable (previously_stable (3)) (unstable_value 5)) |}];
        advance_by_sec handle 1.0;
        show handle;
        [%expect {| (Stable 5) |}];
        advance_by_sec handle 0.5;
        Bonsai.Var.set v' 4;
        show handle;
        [%expect {| (Unstable (previously_stable (5)) (unstable_value 4)) |}];
        advance_by_sec handle 0.5;
        Bonsai.Var.set v' 5;
        show handle;
        [%expect {| (Unstable (previously_stable (5)) (unstable_value 5)) |}]
      ;;

      let%expect_test _ =
        let v' = Bonsai.Var.create 1 in
        let on' = Bonsai.Var.create true in
        let v = Bonsai.Var.value v' in
        let on = Bonsai.Var.value on' in
        let c =
          match%sub on with
          | true ->
            let%sub x =
              M.value_stability
                ~sexp_of_model:[%sexp_of: Int.t]
                ~equal:[%equal: Int.t]
                v
                ~time_to_stable:(Value.return (Time_ns.Span.of_sec 1.0))
            in
            let%arr x = x in
            Some x
          | false -> Bonsai.const None
        in
        let handle =
          Handle.create
            (Result_spec.sexp
               (module struct
                 type t = int Bonsai_extra.Stability.t option [@@deriving sexp]
               end))
            c
        in
        show handle;
        [%expect {| ((Unstable (previously_stable ()) (unstable_value 1))) |}];
        advance_by_sec handle 1.0;
        show handle;
        [%expect {| ((Stable 1)) |}];
        Bonsai.Var.set on' false;
        show handle;
        [%expect {| () |}];
        Bonsai.Var.set on' true;
        show handle;
        [%expect {| ((Unstable (previously_stable (1)) (unstable_value 1))) |}]
      ;;
    end

    module _ = Common (struct
      (* The function reference below is an implementation that exists purely
           as a sanity check for the real implementation. If two vastly
           different implemenations always yield the same result, that's an
           encouraging sign. Sadly, this implementation relies on having a
           frame between certain real-world events, so we only run the tests
           with [recompute_view_until_stable] being called before Handle.show.
           (This downside is one reason why this is not the real
           implementation.) *)

      let value_stability = alternate_value_stability_implementation

      let show_handle handle =
        Handle.recompute_view_until_stable handle;
        Handle.show handle
      ;;
    end)

    module _ = Common (struct
      let value_stability = Bonsai_extra.value_stability
      let show_handle = Handle.show
    end)

    module _ = Common (struct
      let value_stability = Bonsai_extra.value_stability

      let show_handle handle =
        Handle.recompute_view handle;
        Handle.show handle
      ;;
    end)

    module _ = Common (struct
      let value_stability = Bonsai_extra.value_stability

      let show_handle handle =
        Handle.recompute_view_until_stable handle;
        Handle.show handle
      ;;
    end)
  end)
;;
