open! Core
open Bonsai.Let_syntax
open Virtual_dom
open Proc

(* This test basically just exists because it's really hard to dispatch
   events deep into a tree without an intermediate representation.  Turns
   out that Vdom.Node.t is a pretty good intermediate representation. *)
let%expect_test "recursive component" =
  let module M = struct
    type t =
      { label : string
      ; children : t Int.Map.t
      }
    [@@deriving fields]
  end
  in
  let rec tree input path =
    let%sub { M.label; children } = return input in
    let%sub children =
      Bonsai.assoc
        (module Int)
        children
        ~f:(fun index child ->
          let path = Bonsai.Value.map2 index path ~f:List.cons in
          let%sub child = Bonsai.lazy_ (lazy (tree child path)) in
          let%arr child = child
          and index = index in
          Vdom.Node.div [ Vdom.Node.textf "%d" index; child ])
    in
    let%sub state = Bonsai.state (module Int) ~default_model:0 in
    let%arr children = children
    and label = label
    and path = path
    and state, set_state = state in
    let path_text =
      String.concat ~sep:"_" ("path" :: List.rev_map path ~f:Int.to_string)
    in
    Vdom.Node.div
      ~attr:(Vdom.Attr.id path_text)
      (Vdom.Node.textf "%s %d" label state
       :: Vdom.Node.button
            ~attr:(Vdom.Attr.on_click (fun _ -> set_state (state + 1)))
            [ Vdom.Node.text "+1" ]
       :: Map.data (children : _ Int.Map.t))
  in
  let var =
    Bonsai.Var.create
      { M.label = "hi"
      ; children =
          Int.Map.of_alist_exn
            [ ( 0
              , { M.label = "hello"
                ; children =
                    Int.Map.of_alist_exn
                      [ 99, { M.label = "another"; children = Int.Map.empty } ]
                } )
            ; 1, { M.label = "test"; children = Int.Map.empty }
            ; 2, { M.label = "foobar"; children = Int.Map.empty }
            ]
      }
  in
  let value = Bonsai.Var.value var in
  let handle =
    Handle.create (Result_spec.vdom Fn.id) (tree value (Bonsai.Value.return [ 0 ]))
  in
  Handle.show handle;
  let before = [%expect.output] in
  Handle.click_on handle ~get_vdom:Fn.id ~selector:"#path_0_0_99 button";
  Handle.show handle;
  let after = [%expect.output] in
  Expect_test_patdiff.print_patdiff before after;
  [%expect
    {|
    -1,28 +1,28
      <div id="path_0">
        hi 0
        <button onclick> +1 </button>
        <div>
          0
          <div id="path_0_0">
            hello 0
            <button onclick> +1 </button>
            <div>
              99
              <div id="path_0_0_99">
    -|          another 0
    +|          another 1
                <button onclick> +1 </button>
              </div>
            </div>
          </div>
        </div>
        <div>
          1
          <div id="path_0_1">
            test 0
            <button onclick> +1 </button>
          </div>
        </div>
        <div>
          2
          <div id="path_0_2">
            foobar 0 |}];
  (* test sending an action to an inactive component *)
  (* capture the result so that we can schedule actions from it. *)
  let old_input = Bonsai.Var.get var in
  let old_result = Handle.result handle in
  Bonsai.Var.set var { M.label = "hi"; children = Int.Map.empty };
  Handle.show handle;
  [%expect
    {|
    <div id="path_0">
      hi 0
      <button onclick> +1 </button>
    </div> |}];
  Handle.click_on handle ~get_vdom:(fun _ -> old_result) ~selector:"#path_0_0_99 button";
  Bonsai.Var.set var old_input;
  Handle.show handle;
  [%expect
    {|
    <div id="path_0">
      hi 0
      <button onclick> +1 </button>
      <div>
        0
        <div id="path_0_0">
          hello 0
          <button onclick> +1 </button>
          <div>
            99
            <div id="path_0_0_99">
              another 2
              <button onclick> +1 </button>
            </div>
          </div>
        </div>
      </div>
      <div>
        1
        <div id="path_0_1">
          test 0
          <button onclick> +1 </button>
        </div>
      </div>
      <div>
        2
        <div id="path_0_2">
          foobar 0
          <button onclick> +1 </button>
        </div>
      </div>
    </div> |}]
;;
