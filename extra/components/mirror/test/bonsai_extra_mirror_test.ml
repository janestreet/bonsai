open! Core
open! Bonsai_test
module Effect = Bonsai.Effect
open Bonsai.Let_syntax

module%test [@name "mirror with var"] _ = struct
  let prepare_test ~store ~interactive =
    let store = Bonsai.Expert.Var.create store in
    let interactive = Bonsai.Expert.Var.create interactive in
    let store_set =
      (fun value ->
        print_endline [%string "will set store to %{value} next frame"];
        Bonsai.Expert.Var.set store value)
      |> Ui_effect.of_sync_fun
    in
    let interactive_set =
      (fun value ->
        print_endline [%string "will set interactive to %{value} next frame"];
        Bonsai.Expert.Var.set interactive value)
      |> Ui_effect.of_sync_fun
    in
    let component graph =
      let () =
        Bonsai_extra.Mirror.mirror
          ~equal:[%equal: String.t]
          ~store_set:(return store_set)
          ~interactive_set:(return interactive_set)
          ~store_value:(Bonsai.Expert.Var.value store)
          ~interactive_value:(Bonsai.Expert.Var.value interactive)
          graph
      in
      let%map store = Bonsai.Expert.Var.value store
      and interactive = Bonsai.Expert.Var.value interactive in
      sprintf "store: %s, interactive: %s" store interactive
    in
    let handle = Handle.create (Result_spec.string (module String)) component in
    handle, store, interactive
  ;;

  let%expect_test "starts stable" =
    let handle, _store, _interactive = prepare_test ~store:"a" ~interactive:"a" in
    Handle.show handle;
    [%expect {| store: a, interactive: a |}]
  ;;

  let%expect_test "starts unstable" =
    let handle, _store, _interactive = prepare_test ~store:"a" ~interactive:"b" in
    Handle.show handle;
    [%expect
      {|
      will set interactive to a next frame
      store: a, interactive: b
      |}];
    Handle.show handle;
    [%expect {| store: a, interactive: a |}]
  ;;

  let%expect_test "starts stable and then interactive changes" =
    let handle, _store, interactive = prepare_test ~store:"a" ~interactive:"a" in
    Handle.show handle;
    [%expect {| store: a, interactive: a |}];
    Bonsai.Expert.Var.set interactive "b";
    Handle.show handle;
    [%expect
      {|
      will set store to b next frame
      store: a, interactive: b
      |}];
    Handle.show handle;
    [%expect {| store: b, interactive: b |}]
  ;;

  let%expect_test "starts stable and then store changes" =
    let handle, store, _interactive = prepare_test ~store:"a" ~interactive:"a" in
    Handle.show handle;
    [%expect {| store: a, interactive: a |}];
    Bonsai.Expert.Var.set store "b";
    Handle.show handle;
    [%expect
      {|
      will set interactive to b next frame
      store: b, interactive: a
      |}];
    Handle.show handle;
    [%expect {| store: b, interactive: b |}]
  ;;

  let%expect_test "starts stable and then both change at the same time" =
    let handle, store, interactive = prepare_test ~store:"a" ~interactive:"a" in
    Handle.show handle;
    [%expect {| store: a, interactive: a |}];
    Bonsai.Expert.Var.set store "b";
    Bonsai.Expert.Var.set interactive "c";
    Handle.show handle;
    [%expect
      {|
      will set store to c next frame
      store: b, interactive: c
      |}];
    Handle.show handle;
    [%expect {| store: c, interactive: c |}]
  ;;

  let%expect_test "starts stable and then interactive changes once a frame" =
    let handle, _store, interactive = prepare_test ~store:"a" ~interactive:"a" in
    Handle.show handle;
    [%expect {| store: a, interactive: a |}];
    Bonsai.Expert.Var.set interactive "b";
    Handle.show handle;
    [%expect
      {|
      will set store to b next frame
      store: a, interactive: b
      |}];
    Bonsai.Expert.Var.set interactive "c";
    Handle.show handle;
    [%expect
      {|
      will set store to c next frame
      store: b, interactive: c
      |}];
    Bonsai.Expert.Var.set interactive "d";
    Handle.show handle;
    [%expect
      {|
      will set store to d next frame
      store: c, interactive: d
      |}];
    Handle.show handle;
    [%expect {| store: d, interactive: d |}]
  ;;

  let%expect_test "starts stable and then store changes once a frame" =
    let handle, store, _interactive = prepare_test ~store:"a" ~interactive:"a" in
    Handle.show handle;
    [%expect {| store: a, interactive: a |}];
    Bonsai.Expert.Var.set store "b";
    Handle.show handle;
    [%expect
      {|
      will set interactive to b next frame
      store: b, interactive: a
      |}];
    Bonsai.Expert.Var.set store "c";
    Handle.show handle;
    [%expect
      {|
      will set store to b next frame
      store: c, interactive: b
      |}];
    Bonsai.Expert.Var.set store "d";
    Handle.show handle;
    [%expect
      {|
      will set interactive to d next frame
      store: d, interactive: b
      |}];
    Handle.show handle;
    [%expect {| store: d, interactive: d |}]
  ;;
end

module%test [@name "mirror with state"] _ = struct
  let prepare_test ~store ~interactive =
    let print_set name value =
      Effect.of_thunk (fun () -> printf "%s set to \"%s\"\n" name value)
    in
    let component graph =
      let store_value, store_set = Bonsai.state store graph in
      let interactive_value, interactive_set = Bonsai.state interactive graph in
      let store_set =
        let%arr store_set in
        fun value ->
          let%bind.Effect () = print_set "store" value in
          store_set value
      in
      let interactive_set =
        let%arr interactive_set in
        fun value ->
          let%bind.Effect () = print_set "interactive" value in
          interactive_set value
      in
      let () =
        Bonsai_extra.Mirror.mirror
          ~equal:[%equal: String.t]
          ~store_set
          ~interactive_set
          ~store_value
          ~interactive_value
          graph
      in
      let%map store_value and interactive_value and store_set and interactive_set in
      store_value, store_set, interactive_value, interactive_set
    in
    Handle.create
      (module struct
        type t = string * (string -> unit Effect.t) * string * (string -> unit Effect.t)

        type incoming =
          [ `Store_set of string
          | `Interactive_set of string
          ]

        let view (store, _, interactive, _) =
          sprintf "store: %s, interactive: %s" store interactive
        ;;

        let incoming (_, store_set, _, interactive_set) = function
          | `Store_set value -> store_set value
          | `Interactive_set value -> interactive_set value
        ;;
      end)
      component
  ;;

  let%expect_test "starts stable" =
    let handle = prepare_test ~store:"a" ~interactive:"a" in
    Handle.show handle;
    [%expect {| store: a, interactive: a |}]
  ;;

  let%expect_test "starts unstable" =
    let handle = prepare_test ~store:"a" ~interactive:"b" in
    Handle.show handle;
    [%expect
      {|
      interactive set to "a"
      store: a, interactive: b
      |}];
    Handle.show handle;
    [%expect {| store: a, interactive: a |}]
  ;;

  let%expect_test "starts stable and then interactive changes" =
    let handle = prepare_test ~store:"a" ~interactive:"a" in
    Handle.show handle;
    [%expect {| store: a, interactive: a |}];
    Handle.do_actions handle [ `Interactive_set "b" ];
    Handle.show handle;
    [%expect
      {|
      interactive set to "b"
      store set to "b"
      store: a, interactive: b
      |}];
    Handle.show handle;
    [%expect {| store: b, interactive: b |}]
  ;;

  let%expect_test "starts stable and then store changes" =
    let handle = prepare_test ~store:"a" ~interactive:"a" in
    Handle.show handle;
    [%expect {| store: a, interactive: a |}];
    Handle.do_actions handle [ `Store_set "b" ];
    Handle.show handle;
    [%expect
      {|
      store set to "b"
      interactive set to "b"
      store: b, interactive: a
      |}];
    Handle.show handle;
    [%expect {| store: b, interactive: b |}]
  ;;

  let%expect_test "starts stable and then both change at the same time" =
    let handle = prepare_test ~store:"a" ~interactive:"a" in
    Handle.show handle;
    [%expect {| store: a, interactive: a |}];
    Handle.do_actions handle [ `Store_set "b"; `Interactive_set "c" ];
    Handle.show handle;
    [%expect
      {|
      store set to "b"
      interactive set to "c"
      store set to "c"
      store: b, interactive: c
      |}];
    Handle.show handle;
    [%expect {| store: c, interactive: c |}]
  ;;

  let%expect_test "starts stable and then both change at the same time" =
    let handle = prepare_test ~store:"a" ~interactive:"a" in
    Handle.show handle;
    [%expect {| store: a, interactive: a |}];
    Handle.do_actions handle [ `Store_set "b"; `Interactive_set "c" ];
    Handle.show handle;
    [%expect
      {|
      store set to "b"
      interactive set to "c"
      store set to "c"
      store: b, interactive: c
      |}];
    Handle.show handle;
    [%expect {| store: c, interactive: c |}]
  ;;

  let%expect_test "starts stable and then interactive changes once a frame" =
    let handle = prepare_test ~store:"a" ~interactive:"a" in
    Handle.show handle;
    [%expect {| store: a, interactive: a |}];
    Handle.do_actions handle [ `Interactive_set "b" ];
    Handle.show handle;
    [%expect
      {|
      interactive set to "b"
      store set to "b"
      store: a, interactive: b
      |}];
    Handle.do_actions handle [ `Interactive_set "c" ];
    Handle.show handle;
    [%expect
      {|
      interactive set to "c"
      store set to "c"
      store: b, interactive: c
      |}];
    Handle.do_actions handle [ `Interactive_set "d" ];
    Handle.show handle;
    [%expect
      {|
      interactive set to "d"
      store set to "d"
      store: c, interactive: d
      |}];
    Handle.show handle;
    [%expect {| store: d, interactive: d |}]
  ;;

  let%expect_test "starts stable and then store changes once a frame" =
    let handle = prepare_test ~store:"a" ~interactive:"a" in
    Handle.show handle;
    [%expect {| store: a, interactive: a |}];
    Handle.do_actions handle [ `Store_set "b" ];
    Handle.show handle;
    [%expect
      {|
      store set to "b"
      interactive set to "b"
      store: b, interactive: a
      |}];
    Handle.do_actions handle [ `Store_set "c" ];
    Handle.show handle;
    [%expect
      {|
      store set to "c"
      store set to "b"
      store: c, interactive: b
      |}];
    Handle.do_actions handle [ `Store_set "d" ];
    Handle.show handle;
    [%expect
      {|
      store set to "d"
      interactive set to "d"
      store: d, interactive: b
      |}];
    Handle.show handle;
    [%expect {| store: d, interactive: d |}]
  ;;
end

module%test [@name "mirror' with var"] _ = struct
  let prepare_test ~store ~interactive =
    let store = Bonsai.Expert.Var.create store in
    let interactive = Bonsai.Expert.Var.create interactive in
    let store_set =
      (fun value ->
        printf "store set to \"%s\"" value;
        Bonsai.Expert.Var.set store (Some value))
      |> Ui_effect.of_sync_fun
    in
    let interactive_set =
      (fun value ->
        printf "interactive set to \"%s\"" value;
        Bonsai.Expert.Var.set interactive (Some value))
      |> Ui_effect.of_sync_fun
    in
    let component graph =
      let () =
        Bonsai_extra.Mirror.mirror'
          ~equal:[%equal: String.t]
          ~store_set:(return store_set)
          ~interactive_set:(return interactive_set)
          ~store_value:(Bonsai.Expert.Var.value store)
          ~interactive_value:(Bonsai.Expert.Var.value interactive)
          graph
      in
      let%map store = Bonsai.Expert.Var.value store
      and interactive = Bonsai.Expert.Var.value interactive in
      sprintf
        "store: %s, interactive: %s"
        (Option.value store ~default:"<none>")
        (Option.value interactive ~default:"<none>")
    in
    let handle = Handle.create (Result_spec.string (module String)) component in
    handle, store, interactive
  ;;

  let%expect_test "starts both none" =
    let handle, _store, _interactive = prepare_test ~store:None ~interactive:None in
    Handle.show handle;
    [%expect {| store: <none>, interactive: <none> |}]
  ;;

  let%expect_test "starts interactive some" =
    let handle, _store, _interactive =
      prepare_test ~store:None ~interactive:(Some "hi")
    in
    Handle.show handle;
    [%expect {| store set to "hi"store: <none>, interactive: hi |}];
    Handle.show handle;
    [%expect {| store: hi, interactive: hi |}]
  ;;

  let%expect_test "starts store some" =
    let handle, _store, _interactive =
      prepare_test ~store:(Some "hi") ~interactive:None
    in
    Handle.show handle;
    [%expect {| interactive set to "hi"store: hi, interactive: <none> |}];
    Handle.show handle;
    [%expect {| store: hi, interactive: hi |}]
  ;;

  let%expect_test "starts both some (same value)" =
    let handle, _store, _interactive =
      prepare_test ~store:(Some "hi") ~interactive:(Some "hi")
    in
    Handle.show handle;
    [%expect {| store: hi, interactive: hi |}]
  ;;

  let%expect_test "starts both some (different values)" =
    let handle, _store, _interactive =
      prepare_test ~store:(Some "hi") ~interactive:(Some "hello")
    in
    Handle.show handle;
    [%expect {| interactive set to "hi"store: hi, interactive: hello |}];
    Handle.show handle;
    [%expect {| store: hi, interactive: hi |}]
  ;;

  let%expect_test "starts both none, store set " =
    let handle, store, _interactive = prepare_test ~store:None ~interactive:None in
    Handle.show handle;
    [%expect {| store: <none>, interactive: <none> |}];
    Bonsai.Expert.Var.set store (Some "hi");
    Handle.show handle;
    [%expect {| interactive set to "hi"store: hi, interactive: <none> |}];
    Handle.show handle;
    [%expect {| store: hi, interactive: hi |}]
  ;;

  let%expect_test "starts both none, interactive set " =
    let handle, _store, interactive = prepare_test ~store:None ~interactive:None in
    Handle.show handle;
    [%expect {| store: <none>, interactive: <none> |}];
    Bonsai.Expert.Var.set interactive (Some "hi");
    Handle.show handle;
    [%expect {| store set to "hi"store: <none>, interactive: hi |}];
    Handle.show handle;
    [%expect {| store: hi, interactive: hi |}]
  ;;

  let%expect_test "starts both none, both set to same value" =
    let handle, store, interactive = prepare_test ~store:None ~interactive:None in
    Handle.show handle;
    [%expect {| store: <none>, interactive: <none> |}];
    Bonsai.Expert.Var.set interactive (Some "hi");
    Bonsai.Expert.Var.set store (Some "hi");
    Handle.show handle;
    [%expect {| store: hi, interactive: hi |}]
  ;;

  let%expect_test "starts both none, both set to different values" =
    let handle, store, interactive = prepare_test ~store:None ~interactive:None in
    Handle.show handle;
    [%expect {| store: <none>, interactive: <none> |}];
    Bonsai.Expert.Var.set interactive (Some "hi");
    Bonsai.Expert.Var.set store (Some "hello");
    Handle.show handle;
    [%expect {| store set to "hi"store: hello, interactive: hi |}];
    Handle.show handle;
    [%expect {| store: hi, interactive: hi |}]
  ;;

  let%expect_test "starts both some, both set to different values" =
    let handle, store, interactive =
      prepare_test ~store:(Some "hi") ~interactive:(Some "hi")
    in
    Handle.show handle;
    [%expect {| store: hi, interactive: hi |}];
    Bonsai.Expert.Var.set interactive (Some "abc");
    Bonsai.Expert.Var.set store (Some "def");
    Handle.show handle;
    [%expect {| store set to "abc"store: def, interactive: abc |}];
    Handle.show handle;
    [%expect {| store: abc, interactive: abc |}]
  ;;

  let%expect_test "starts both some (same value), store reset to none" =
    let handle, store, _interactive =
      prepare_test ~store:(Some "hi") ~interactive:(Some "hi")
    in
    Handle.show handle;
    [%expect {| store: hi, interactive: hi |}];
    Bonsai.Expert.Var.set store None;
    Handle.show handle;
    (* The noneness isn't propagated to interactive *)
    [%expect {| store: <none>, interactive: hi |}]
  ;;

  let%expect_test "starts both some (same value), interactive reset to none" =
    let handle, _store, interactive =
      prepare_test ~store:(Some "hi") ~interactive:(Some "hi")
    in
    Handle.show handle;
    [%expect {| store: hi, interactive: hi |}];
    Bonsai.Expert.Var.set interactive None;
    Handle.show handle;
    (* The noneness isn't propagated to the store *)
    [%expect {| store: hi, interactive: <none> |}]
  ;;

  let%expect_test "starts both some (same value), both set to none" =
    let handle, store, interactive =
      prepare_test ~store:(Some "hi") ~interactive:(Some "hi")
    in
    Handle.show handle;
    [%expect {| store: hi, interactive: hi |}];
    Bonsai.Expert.Var.set store None;
    Bonsai.Expert.Var.set interactive None;
    Handle.show handle;
    (* The noneness isn't propagated to the store *)
    [%expect {| store: <none>, interactive: <none> |}]
  ;;

  let%expect_test "starts both some (same value), interactive set to none, both swap" =
    let handle, store, interactive =
      prepare_test ~store:(Some "hi") ~interactive:(Some "hi")
    in
    Handle.show handle;
    [%expect {| store: hi, interactive: hi |}];
    Bonsai.Expert.Var.set store None;
    Handle.show handle;
    [%expect {| store: <none>, interactive: hi |}];
    Bonsai.Expert.Var.set store (Some "abc");
    Bonsai.Expert.Var.set interactive None;
    Handle.show handle;
    [%expect {| interactive set to "abc"store: abc, interactive: <none> |}];
    Handle.show handle;
    [%expect {| store: abc, interactive: abc |}]
  ;;

  let%expect_test "starts both some (same value), store set to none, both swap" =
    let handle, store, interactive =
      prepare_test ~store:(Some "hi") ~interactive:(Some "hi")
    in
    Handle.show handle;
    [%expect {| store: hi, interactive: hi |}];
    Bonsai.Expert.Var.set interactive None;
    Handle.show handle;
    [%expect {| store: hi, interactive: <none> |}];
    Bonsai.Expert.Var.set interactive (Some "abc");
    Bonsai.Expert.Var.set store None;
    Handle.show handle;
    [%expect {| store set to "abc"store: <none>, interactive: abc |}];
    Handle.show handle;
    [%expect {| store: abc, interactive: abc |}]
  ;;
end

module%test [@name "mirror' with state"] _ = struct
  let prepare_test ~store ~interactive =
    let print_set name value =
      Effect.of_thunk (fun () -> printf "%s set to \"%s\"\n" name value)
    in
    let component graph =
      let store_value, store_set = Bonsai.state_opt ?default_model:store graph in
      let interactive_value, interactive_set =
        Bonsai.state_opt ?default_model:interactive graph
      in
      let store_set_some =
        let%arr store_set in
        fun value ->
          let%bind.Effect () = print_set "store" value in
          store_set (Some value)
      in
      let store_set_opt =
        let%arr store_set in
        fun store ->
          let%bind.Effect () = print_set "store" (Option.value store ~default:"<none>") in
          store_set store
      in
      let interactive_set_some =
        let%arr interactive_set in
        fun value ->
          let%bind.Effect () = print_set "interactive" value in
          interactive_set (Some value)
      in
      let interactive_set_opt =
        let%arr interactive_set in
        fun interactive ->
          let%bind.Effect () =
            print_set "interactive" (Option.value interactive ~default:"<none>")
          in
          interactive_set interactive
      in
      let () =
        Bonsai_extra.Mirror.mirror'
          ~equal:[%equal: String.t]
          ~store_set:store_set_some
          ~interactive_set:interactive_set_some
          ~store_value
          ~interactive_value
          graph
      in
      let%map store_value
      and interactive_value
      and store_set_opt
      and interactive_set_opt in
      store_value, store_set_opt, interactive_value, interactive_set_opt
    in
    Handle.create
      (module struct
        type t =
          string option
          * (string option -> unit Effect.t)
          * string option
          * (string option -> unit Effect.t)

        type incoming =
          [ `Store_set of string option
          | `Interactive_set of string option
          ]

        let view (store, _, interactive, _) =
          sprintf
            "store: %s, interactive: %s"
            (Option.value store ~default:"<none>")
            (Option.value interactive ~default:"<none>")
        ;;

        let incoming (_, store_set, _, interactive_set) = function
          | `Store_set value -> store_set value
          | `Interactive_set value -> interactive_set value
        ;;
      end)
      component
  ;;

  let%expect_test "starts both none" =
    let handle = prepare_test ~store:None ~interactive:None in
    Handle.show handle;
    [%expect {| store: <none>, interactive: <none> |}]
  ;;

  let%expect_test "starts interactive some" =
    let handle = prepare_test ~store:None ~interactive:(Some "hi") in
    Handle.show handle;
    [%expect
      {|
      store set to "hi"
      store: <none>, interactive: hi
      |}];
    Handle.show handle;
    [%expect {| store: hi, interactive: hi |}]
  ;;

  let%expect_test "starts store some" =
    let handle = prepare_test ~store:(Some "hi") ~interactive:None in
    Handle.show handle;
    [%expect
      {|
      interactive set to "hi"
      store: hi, interactive: <none>
      |}];
    Handle.show handle;
    [%expect {| store: hi, interactive: hi |}]
  ;;

  let%expect_test "starts both some (same value)" =
    let handle = prepare_test ~store:(Some "hi") ~interactive:(Some "hi") in
    Handle.show handle;
    [%expect {| store: hi, interactive: hi |}]
  ;;

  let%expect_test "starts both some (different values)" =
    let handle = prepare_test ~store:(Some "hi") ~interactive:(Some "hello") in
    Handle.show handle;
    [%expect
      {|
      interactive set to "hi"
      store: hi, interactive: hello
      |}];
    Handle.show handle;
    [%expect {| store: hi, interactive: hi |}]
  ;;

  let%expect_test "starts both none, store set " =
    let handle = prepare_test ~store:None ~interactive:None in
    Handle.show handle;
    [%expect {| store: <none>, interactive: <none> |}];
    Handle.do_actions handle [ `Store_set (Some "hi") ];
    Handle.show handle;
    [%expect
      {|
      store set to "hi"
      interactive set to "hi"
      store: hi, interactive: <none>
      |}];
    Handle.show handle;
    [%expect {| store: hi, interactive: hi |}]
  ;;

  let%expect_test "starts both none, interactive set " =
    let handle = prepare_test ~store:None ~interactive:None in
    Handle.show handle;
    [%expect {| store: <none>, interactive: <none> |}];
    Handle.do_actions handle [ `Interactive_set (Some "hi") ];
    Handle.show handle;
    [%expect
      {|
      interactive set to "hi"
      store set to "hi"
      store: <none>, interactive: hi
      |}];
    Handle.show handle;
    [%expect {| store: hi, interactive: hi |}]
  ;;

  let%expect_test "starts both none, both set to same value" =
    let handle = prepare_test ~store:None ~interactive:None in
    Handle.show handle;
    [%expect {| store: <none>, interactive: <none> |}];
    Handle.do_actions handle [ `Interactive_set (Some "hi") ];
    Handle.do_actions handle [ `Store_set (Some "hi") ];
    Handle.show handle;
    [%expect
      {|
      interactive set to "hi"
      store set to "hi"
      store: hi, interactive: hi
      |}]
  ;;

  let%expect_test "starts both none, both set to different values" =
    let handle = prepare_test ~store:None ~interactive:None in
    Handle.show handle;
    [%expect {| store: <none>, interactive: <none> |}];
    Handle.do_actions handle [ `Interactive_set (Some "hi") ];
    Handle.do_actions handle [ `Store_set (Some "hello") ];
    Handle.show handle;
    [%expect
      {|
      interactive set to "hi"
      store set to "hello"
      store set to "hi"
      store: hello, interactive: hi
      |}];
    Handle.show handle;
    [%expect {| store: hi, interactive: hi |}]
  ;;

  let%expect_test "starts both some, both set to different values" =
    let handle = prepare_test ~store:(Some "hi") ~interactive:(Some "hi") in
    Handle.show handle;
    [%expect {| store: hi, interactive: hi |}];
    Handle.do_actions handle [ `Interactive_set (Some "abc") ];
    Handle.do_actions handle [ `Store_set (Some "def") ];
    Handle.show handle;
    [%expect
      {|
      interactive set to "abc"
      store set to "def"
      store set to "abc"
      store: def, interactive: abc
      |}];
    Handle.show handle;
    [%expect {| store: abc, interactive: abc |}]
  ;;

  let%expect_test "starts both some (same value), store reset to none" =
    let handle = prepare_test ~store:(Some "hi") ~interactive:(Some "hi") in
    Handle.show handle;
    [%expect {| store: hi, interactive: hi |}];
    Handle.do_actions handle [ `Store_set None ];
    Handle.show handle;
    (* The noneness isn't propagated to interactive *)
    [%expect
      {|
      store set to "<none>"
      store: <none>, interactive: hi
      |}]
  ;;

  let%expect_test "starts both some (same value), interactive reset to none" =
    let handle = prepare_test ~store:(Some "hi") ~interactive:(Some "hi") in
    Handle.show handle;
    [%expect {| store: hi, interactive: hi |}];
    Handle.do_actions handle [ `Interactive_set None ];
    Handle.show handle;
    (* The noneness isn't propagated to the store *)
    [%expect
      {|
      interactive set to "<none>"
      store: hi, interactive: <none>
      |}]
  ;;

  let%expect_test "starts both some (same value), both set to none" =
    let handle = prepare_test ~store:(Some "hi") ~interactive:(Some "hi") in
    Handle.show handle;
    [%expect {| store: hi, interactive: hi |}];
    Handle.do_actions handle [ `Store_set None ];
    Handle.do_actions handle [ `Interactive_set None ];
    Handle.show handle;
    (* The noneness isn't propagated to the store *)
    [%expect
      {|
      store set to "<none>"
      interactive set to "<none>"
      store: <none>, interactive: <none>
      |}]
  ;;

  let%expect_test "starts both some (same value), interactive set to none, both swap" =
    let handle = prepare_test ~store:(Some "hi") ~interactive:(Some "hi") in
    Handle.show handle;
    [%expect {| store: hi, interactive: hi |}];
    Handle.do_actions handle [ `Store_set None ];
    Handle.show handle;
    [%expect
      {|
      store set to "<none>"
      store: <none>, interactive: hi
      |}];
    Handle.do_actions handle [ `Store_set (Some "abc") ];
    Handle.do_actions handle [ `Interactive_set None ];
    Handle.show handle;
    [%expect
      {|
      store set to "abc"
      interactive set to "<none>"
      interactive set to "abc"
      store: abc, interactive: <none>
      |}];
    Handle.show handle;
    [%expect {| store: abc, interactive: abc |}]
  ;;

  let%expect_test "starts both some (same value), store set to none, both swap" =
    let handle = prepare_test ~store:(Some "hi") ~interactive:(Some "hi") in
    Handle.show handle;
    [%expect {| store: hi, interactive: hi |}];
    Handle.do_actions handle [ `Interactive_set None ];
    Handle.show handle;
    [%expect
      {|
      interactive set to "<none>"
      store: hi, interactive: <none>
      |}];
    Handle.do_actions handle [ `Interactive_set (Some "abc") ];
    Handle.do_actions handle [ `Store_set None ];
    Handle.show handle;
    [%expect
      {|
      interactive set to "abc"
      store set to "<none>"
      store set to "abc"
      store: <none>, interactive: abc
      |}];
    Handle.show handle;
    [%expect {| store: abc, interactive: abc |}]
  ;;
end
