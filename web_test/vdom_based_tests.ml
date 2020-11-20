open! Core_kernel
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
    let%pattern_bind { M.label; children } = input in
    let%sub children =
      Bonsai.assoc
        (module Int)
        children
        ~f:(fun index child ->
          let path = Bonsai.Value.map2 index path ~f:List.cons in
          let%sub child = Bonsai.lazy_ (lazy (tree child path)) in
          return
          @@ let%map child = child
          and index = index in
          Vdom.Node.div [] [ Vdom.Node.textf "%d" index; child ])
    in
    let%sub state = Bonsai.state [%here] (module Int) ~default_model:0 in
    return
    @@ let%map children = children
    and label = label
    and path = path
    and state, set_state = state in
    let path_text =
      String.concat ~sep:"_" ("path" :: List.rev_map path ~f:Int.to_string)
    in
    Vdom.Node.div
      [ Vdom.Attr.id path_text ]
      (Vdom.Node.textf "%s %d" label state
       :: Vdom.Node.button
            [ Vdom.Attr.on_click (fun _ -> set_state (state + 1)) ]
            [ Vdom.Node.text "+1" ]
       :: Int.Map.data children)
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
        <button onclick={handler}> +1 </button>
        <div>
          0
          <div id="path_0_0">
            hello 0
            <button onclick={handler}> +1 </button>
            <div>
              99
              <div id="path_0_0_99">
    -|          another 0
    +|          another 1
                <button onclick={handler}> +1 </button>
              </div>
            </div>
          </div>
        </div>
        <div>
          1
          <div id="path_0_1">
            test 0
            <button onclick={handler}> +1 </button>
          </div>
        </div>
        <div>
          2
          <div id="path_0_2">
            foobar 0 |}]
;;
