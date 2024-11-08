open Core
open Ppxlib

let loc = Location.none

let test_bonsai_expect_test = function
  | [ ({ pstr_desc = Pstr_value (_, binding :: []); _ } as structure_item) ] ->
    Ppx_bonsai.Test_extension.For_testing.create_structure ~loc structure_item binding
    |> Pprintast.string_of_structure
    |> print_endline
  | _ -> failwith "expected a single value binding"
;;

module%test [@name "expect_test.bonsai"] _ = struct
  let no_expect_blocks =
    [%str
      let "no expect blocks" =
        let open Bonsai.Let_syntax in
        let handle =
          Handle.create (Result_spec.vdom Fn.id) Bonsai_testing_example_lib.hello_world
        in
        Handle.show handle
      ;;]
  ;;

  let%expect_test "no expect blocks" =
    test_bonsai_expect_test no_expect_blocks;
    [%expect
      {xxx|
      [%%expect_test
        let "no expect blocks" =
          let open Bonsai.Let_syntax in
            let handle =
              Handle.create (Result_spec.vdom Fn.id)
                Bonsai_testing_example_lib.hello_world in
            Handle.show handle]
      |xxx}]
  ;;

  let single_expect_block =
    [%str
      let "single expect block" =
        let open Bonsai.Let_syntax in
        let handle =
          Handle.create (Result_spec.vdom Fn.id) Bonsai_testing_example_lib.hello_world
        in
        Handle.show handle;
        [%expect {| <span> hello world </span> |}]
      ;;]
  ;;

  let%expect_test "single expect block" =
    test_bonsai_expect_test single_expect_block;
    [%expect
      {xxx|
      [%%expect_test
        let "single expect block" =
          let open Bonsai.Let_syntax in
            let handle =
              Handle.create (Result_spec.vdom Fn.id)
                Bonsai_testing_example_lib.hello_world in
            Handle.show handle; [%expect {| <span> hello world </span> |}]]
      |xxx}]
  ;;

  let multiple_expect_blocks =
    [%str
      let "multiple expect blocks" =
        let handle = Handle.create (Result_spec.vdom Fn.id) hello_textbox in
        Handle.show handle;
        [%expect
          {|
          <div>
            <input oninput> </input>
            <span> hello  </span>
          </div>
          |}];
        Handle.input_text handle ~get_vdom:Fn.id ~selector:"input" ~text:"Bob";
        Handle.show_diff handle;
        [%expect
          {|
            <div>
              <input oninput> </input>
          -|  <span> hello  </span>
          +|  <span> hello Bob </span>
            </div>
          |}];
        Handle.input_text handle ~get_vdom:Fn.id ~selector:"input" ~text:"Alice";
        Handle.show_diff handle;
        [%expect
          {|
            <div>
              <input oninput> </input>
          -|  <span> hello Bob </span>
          +|  <span> hello Alice </span>
            </div>
          |}]
      ;;]
  ;;

  let%expect_test "multiple expect blocks" =
    test_bonsai_expect_test multiple_expect_blocks;
    [%expect
      {xxx|
      [%%expect_test
        let "multiple expect blocks" =
          let handle = Handle.create (Result_spec.vdom Fn.id) hello_textbox in
          Handle.show handle;
          [%expect
            {|
                <div>
                  <input oninput> </input>
                  <span> hello  </span>
                </div>
                |}];
          Handle.input_text handle ~get_vdom:Fn.id ~selector:"input" ~text:"Bob";
          Handle.show_diff handle;
          [%expect
            {|
                  <div>
                    <input oninput> </input>
                -|  <span> hello  </span>
                +|  <span> hello Bob </span>
                  </div>
                |}];
          Handle.input_text handle ~get_vdom:Fn.id ~selector:"input" ~text:"Alice";
          Handle.show_diff handle;
          [%expect
            {|
                  <div>
                    <input oninput> </input>
                -|  <span> hello Bob </span>
                +|  <span> hello Alice </span>
                  </div>
                |}]]
      |xxx}]
  ;;
end
