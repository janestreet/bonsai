open! Core
open Bonsai_web
open Bonsai.Let_syntax
open Bonsai_web_test

module Id : sig
  include Bonsai_experimental_dagviz.Name

  val of_string : string -> t
end = struct
  module Count = Int

  module T = struct
    type t =
      | User of string
      | Gen of Count.t
    [@@deriving bin_io, compare, sexp]
  end

  include T
  include Comparable.Make_binable (T)

  let curr = ref 0

  let create () =
    curr := !curr + 1;
    User [%string "generated_id_%{!curr#Int}"]
  ;;

  let next count =
    let count = Count.succ count in
    Gen count, count
  ;;

  let of_string s = User s

  let to_string = function
    | User s -> s
    | Gen s -> [%string "gen_%{s#Int}"]
  ;;
end

module To_vdom = Bonsai_experimental_dagviz.To_vdom.Make (Id)
open To_vdom

module Dummy_nodes = struct
  let a = Id.of_string "A"
  let b = Id.of_string "B"
  let c = Id.of_string "C"
  let d = Id.of_string "D"
  let e = Id.of_string "E"
  let f = Id.of_string "F"
  let g = Id.of_string "G"
  let ( ==> ) from to_ = { Edge.from; to_ }
  let map_with_ids ids = Id.Map.of_alist_exn (List.map ids ~f:(fun x -> x, ()))
end

let node_to_vdom (id : Id.t Value.t) _ : Vdom.Node.t Computation.t =
  let%arr id = id in
  Vdom.Node.text (Id.to_string id)
;;

let edge_to_svg ~(edge : Edge.t Value.t) ~from:_ ~to_:_ : Vdom.Node.t Computation.t =
  let%arr edge = edge in
  Vdom.Node.sexp_for_debugging [%message (edge : Edge.t)]
;;

let create_handle ~dag ~curr_id =
  let component =
    let%sub dag, _curr_id =
      create ~curr_id ~direction:`Top_to_bottom ~node_to_vdom ~edge_to_svg dag
    in
    match%sub dag with
    | Ok dag -> Bonsai.read dag
    | Error error ->
      let%arr error = error in
      Vdom.Node.sexp_for_debugging [%message (error : Error.t)]
  in
  Handle.create
    (Result_spec.vdom
       ~filter_printed_attributes:(fun key _ ->
         match key with
         | "bulk_position_tracker" | "bulk_size_tracker" -> false
         | s when String.is_prefix s ~prefix:"style" -> false
         | _ -> true)
       Fn.id)
    component
;;

let set_positions ~handle ~ids =
  let positions =
    List.map ids ~f:(fun id ->
      { Handle.Position_tracker.selector = [%string "[src-name='%{id#Id}']"]
      ; width = 1
      ; height = 1
      ; top = 1
      ; left = 1
      })
  in
  Handle.Position_tracker.change_positions handle ~get_vdom:Fn.id positions
;;

let curr_id = Value.return Id.Count.zero

let%expect_test "Linked list graph" =
  let open Dummy_nodes in
  let edges = [ a ==> b; b ==> c; c ==> d ] |> Edge.Set.of_list in
  (*
     {v
         a
         |
         b
         |
         c
         |
         d
     v}
  *)
  let nodes = map_with_ids [ a; b; c; d ] in
  let dag = Value.return { edges; nodes } in
  let handle = create_handle ~dag ~curr_id in
  Handle.show handle;
  [%expect
    {|
    <div class="map_hash_replaced_in_test testcase_hash_replaced_in_test">
      <svg> </svg>
      <div class="hbox_hash_replaced_in_test">
        <div> </div>
        <div class="vbox_hash_replaced_in_test">
          <div> </div>
          <div class="vbox_hash_replaced_in_test">
            <div @key=generated_id_14 data-kind="singleton" src-name="A" outside-id="generated_id_14"> A </div>
            <div @key=generated_id_13
                 kind="mapn"
                 src-name="B"
                 outside-id="generated_id_13"
                 my-id="generated_id_13"
                 class="dest-class-generated_id_13"> B </div>
            <div @key=generated_id_9
                 kind="mapn"
                 src-name="C"
                 outside-id="generated_id_9"
                 my-id="generated_id_9"
                 class="dest-class-generated_id_9"> C </div>
            <div class="hbox_hash_replaced_in_test">
              <div @key=generated_id_5
                   kind="mapn"
                   src-name="D"
                   outside-id="generated_id_5"
                   my-id="generated_id_5"
                   class="dest-class-generated_id_5"> D </div>
            </div>
          </div>
          <div> </div>
        </div>
        <div> </div>
      </div>
    </div>
     |}];
  set_positions ~handle ~ids:[ a; b; c; d ];
  Handle.show_diff handle;
  [%expect
    {|
      <div class="map_hash_replaced_in_test testcase_hash_replaced_in_test">
    -|  <svg> </svg>
    +|  <svg>
    +|    <pre> (edge ((from (User generated_id_13)) (to_ (User generated_id_9)))) </pre>
    +|    <pre> (edge ((from (User generated_id_14)) (to_ (User generated_id_13)))) </pre>
    +|    <pre> (edge ((from (User generated_id_9)) (to_ (User generated_id_5)))) </pre>
    +|  </svg>
        <div class="hbox_hash_replaced_in_test">
          <div> </div>
          <div class="vbox_hash_replaced_in_test">
            <div> </div>
            <div class="vbox_hash_replaced_in_test">
              <div @key=generated_id_14 data-kind="singleton" src-name="A" outside-id="generated_id_14"> A </div>
              <div @key=generated_id_13
                   kind="mapn"
                   src-name="B"
                   outside-id="generated_id_13"
                   my-id="generated_id_13"
                   class="dest-class-generated_id_13"> B </div>
              <div @key=generated_id_9
                   kind="mapn"
                   src-name="C"
                   outside-id="generated_id_9" |}]
;;

let%expect_test "Dominator reorganizing." =
  let open Dummy_nodes in
  let edges = [ a ==> c; b ==> c; c ==> d ] |> Edge.Set.of_list in
  (*
     {v
         A   B
          \ /
           C
           |
           D
     v}
  *)
  let nodes = map_with_ids [ a; b; c; d ] in
  let dag_var = Bonsai.Var.create { edges; nodes } in
  let dag = Bonsai.Var.value dag_var in
  let handle = create_handle ~dag ~curr_id in
  Handle.show handle;
  [%expect
    {|
    <div class="map_hash_replaced_in_test testcase_hash_replaced_in_test">
      <svg> </svg>
      <div class="hbox_hash_replaced_in_test">
        <div> </div>
        <div class="vbox_hash_replaced_in_test">
          <div> </div>
          <div class="vbox_hash_replaced_in_test">
            <div class="hbox_hash_replaced_in_test">
              <div @key=generated_id_26
                   data-kind="singleton"
                   src-name="A"
                   outside-id="generated_id_26"> A </div>
              <div @key=generated_id_25
                   data-kind="singleton"
                   src-name="B"
                   outside-id="generated_id_25"> B </div>
            </div>
            <div @key=generated_id_24
                 kind="mapn"
                 src-name="C"
                 outside-id="generated_id_24"
                 my-id="generated_id_24"
                 class="dest-class-generated_id_24"> C </div>
            <div class="hbox_hash_replaced_in_test">
              <div @key=generated_id_18
                   kind="mapn"
                   src-name="D"
                   outside-id="generated_id_18"
                   my-id="generated_id_18"
                   class="dest-class-generated_id_18"> D </div>
            </div>
          </div>
          <div> </div>
        </div>
        <div> </div>
      </div>
    </div> |}];
  set_positions ~handle ~ids:[ a; b; c; d ];
  Handle.show_diff handle;
  [%expect
    {|
      <div class="map_hash_replaced_in_test testcase_hash_replaced_in_test">
    -|  <svg> </svg>
    +|  <svg>
    +|    <pre> (edge ((from (User generated_id_24)) (to_ (User generated_id_18)))) </pre>
    +|    <pre> (edge ((from (User generated_id_25)) (to_ (User generated_id_24)))) </pre>
    +|    <pre> (edge ((from (User generated_id_26)) (to_ (User generated_id_24)))) </pre>
    +|  </svg>
        <div class="hbox_hash_replaced_in_test">
          <div> </div>
          <div class="vbox_hash_replaced_in_test">
            <div> </div>
            <div class="vbox_hash_replaced_in_test">
              <div class="hbox_hash_replaced_in_test">
                <div @key=generated_id_26
                     data-kind="singleton"
                     src-name="A"
                     outside-id="generated_id_26"> A </div>
                <div @key=generated_id_25
                     data-kind="singleton"
                     src-name="B"
                     outside-id="generated_id_25"> B </div>
              </div>
              <div @key=generated_id_24 |}];
  let edges = Set.add edges (e ==> a) |> Fn.flip Set.add (e ==> b) in
  let nodes = map_with_ids [ a; b; c; d; e ] in
  Bonsai.Var.set dag_var { edges; nodes };
  (*
     {v
           E
          / \
         A   B
          \ /
           C
           |
           D
     v}
  *)
  Handle.show handle;
  (* Since E was added, it is now the only root. *)
  [%expect
    {|
    <div class="map_hash_replaced_in_test testcase_hash_replaced_in_test">
      <svg> </svg>
      <div class="hbox_hash_replaced_in_test">
        <div> </div>
        <div class="vbox_hash_replaced_in_test">
          <div> </div>
          <div class="vbox_hash_replaced_in_test">
            <div @key=generated_id_45 data-kind="singleton" src-name="E" outside-id="generated_id_45"> E </div>
            <div class="hbox_hash_replaced_in_test">
              <div @key=generated_id_40
                   kind="mapn"
                   src-name="B"
                   outside-id="generated_id_40"
                   my-id="generated_id_40"
                   class="dest-class-generated_id_40"> B </div>
              <div @key=generated_id_44
                   kind="mapn"
                   src-name="A"
                   outside-id="generated_id_44"
                   my-id="generated_id_44"
                   class="dest-class-generated_id_44"> A </div>
            </div>
            <div @key=generated_id_36
                 kind="mapn"
                 src-name="C"
                 outside-id="generated_id_36"
                 my-id="generated_id_36"
                 class="dest-class-generated_id_36"> C </div>
            <div class="hbox_hash_replaced_in_test">
              <div @key=generated_id_30
                   kind="mapn"
                   src-name="D"
                   outside-id="generated_id_30"
                   my-id="generated_id_30"
                   class="dest-class-generated_id_30"> D </div>
            </div>
          </div>
          <div> </div>
        </div>
        <div> </div>
      </div>
    </div> |}];
  set_positions ~handle ~ids:[ a; b; c; d; e ];
  Handle.show_diff handle;
  [%expect
    {|
      <div class="map_hash_replaced_in_test testcase_hash_replaced_in_test">
    -|  <svg> </svg>
    +|  <svg>
    +|    <pre> (edge ((from (User generated_id_36)) (to_ (User generated_id_30)))) </pre>
    +|    <pre> (edge ((from (User generated_id_40)) (to_ (User generated_id_36)))) </pre>
    +|    <pre> (edge ((from (User generated_id_44)) (to_ (User generated_id_36)))) </pre>
    +|    <pre> (edge ((from (User generated_id_45)) (to_ (User generated_id_40)))) </pre>
    +|    <pre> (edge ((from (User generated_id_45)) (to_ (User generated_id_44)))) </pre>
    +|  </svg>
        <div class="hbox_hash_replaced_in_test">
          <div> </div>
          <div class="vbox_hash_replaced_in_test">
            <div> </div>
            <div class="vbox_hash_replaced_in_test">
              <div @key=generated_id_45 data-kind="singleton" src-name="E" outside-id="generated_id_45"> E </div>
              <div class="hbox_hash_replaced_in_test">
                <div @key=generated_id_40
                     kind="mapn"
                     src-name="B"
                     outside-id="generated_id_40"
                     my-id="generated_id_40"
                     class="dest-class-generated_id_40"> B </div>
                <div @key=generated_id_44
                     kind="mapn"
                     src-name="A" |}]
;;

let%expect_test "Missing node in node map from edges is treated as a redirect" =
  let open Dummy_nodes in
  let edges = [ a ==> b; b ==> c ] |> Edge.Set.of_list in
  (* B is not present in the map.  *)
  let nodes = map_with_ids [ a; c ] in
  let dag = Value.return { edges; nodes } in
  let handle = create_handle ~dag ~curr_id in
  (* The node where [B] would've been is empty.*)
  Handle.show handle;
  [%expect
    {|
    <div class="map_hash_replaced_in_test testcase_hash_replaced_in_test">
      <svg> </svg>
      <div class="hbox_hash_replaced_in_test">
        <div> </div>
        <div class="vbox_hash_replaced_in_test">
          <div> </div>
          <div class="vbox_hash_replaced_in_test">
            <div @key=generated_id_54 data-kind="singleton" src-name="A" outside-id="generated_id_54"> A </div>
            <div @key=generated_id_53
                 kind="mapn"
                 src-name="B"
                 outside-id="generated_id_53"
                 my-id="generated_id_53"
                 class="dest-class-generated_id_53">
              <div> </div>
            </div>
            <div class="hbox_hash_replaced_in_test">
              <div @key=generated_id_49
                   kind="mapn"
                   src-name="C"
                   outside-id="generated_id_49"
                   my-id="generated_id_49"
                   class="dest-class-generated_id_49"> C </div>
            </div>
          </div>
          <div> </div>
        </div>
        <div> </div>
      </div>
    </div> |}];
  set_positions ~handle ~ids:[ a; b; c ];
  Handle.show_diff handle;
  [%expect
    {|
      <div class="map_hash_replaced_in_test testcase_hash_replaced_in_test">
    -|  <svg> </svg>
    +|  <svg>
    +|    <pre> (edge ((from (User generated_id_53)) (to_ (User generated_id_49)))) </pre>
    +|    <pre> (edge ((from (User generated_id_54)) (to_ (User generated_id_53)))) </pre>
    +|  </svg>
        <div class="hbox_hash_replaced_in_test">
          <div> </div>
          <div class="vbox_hash_replaced_in_test">
            <div> </div>
            <div class="vbox_hash_replaced_in_test">
              <div @key=generated_id_54 data-kind="singleton" src-name="A" outside-id="generated_id_54"> A </div>
              <div @key=generated_id_53
                   kind="mapn"
                   src-name="B"
                   outside-id="generated_id_53"
                   my-id="generated_id_53"
                   class="dest-class-generated_id_53">
                <div> </div>
              </div>
              <div class="hbox_hash_replaced_in_test">
                <div @key=generated_id_49 |}]
;;

let%expect_test "Tree-like DAG" =
  let open Dummy_nodes in
  let edges = [ a ==> b; a ==> c; a ==> d; d ==> e ] |> Edge.Set.of_list in
  (*
     {v
          A
         /|\
        B C D
            |
            E
     v}
  *)
  let nodes = map_with_ids [ a; b; c; d; e ] in
  let dag_var = Bonsai.Var.create { edges; nodes } in
  let dag = Bonsai.Var.value dag_var in
  let handle = create_handle ~dag ~curr_id in
  Handle.show handle;
  [%expect
    {|
    <div class="map_hash_replaced_in_test testcase_hash_replaced_in_test">
      <svg> </svg>
      <div class="hbox_hash_replaced_in_test">
        <div> </div>
        <div class="vbox_hash_replaced_in_test">
          <div> </div>
          <div class="vbox_hash_replaced_in_test">
            <div @key=generated_id_71 data-kind="singleton" src-name="A" outside-id="generated_id_71"> A </div>
            <div class="hbox_hash_replaced_in_test">
              <div @key=generated_id_62
                   kind="mapn"
                   src-name="D"
                   outside-id="generated_id_62"
                   my-id="generated_id_62"
                   class="dest-class-generated_id_62"> D </div>
              <div class="vbox_hash_replaced_in_test">
                <div @key=gen_4
                     data-kind="redirect"
                     src-name="gen_3"
                     outside-id="gen_4"
                     class="dest-class-A redirect_hash_replaced_in_test">
                  <div> </div>
                </div>
                <div @key=gen_5
                     data-kind="redirect"
                     src-name="gen_2"
                     outside-id="gen_5"
                     class="dest-class-gen_3 redirect_hash_replaced_in_test">
                  <div> </div>
                </div>
              </div>
            </div>
            <div class="hbox_hash_replaced_in_test">
              <div @key=generated_id_58
                   kind="mapn"
                   src-name="E"
                   outside-id="generated_id_58"
                   my-id="generated_id_58"
                   class="dest-class-generated_id_58"> E </div>
              <div @key=generated_id_66
                   kind="mapn"
                   src-name="C"
                   outside-id="generated_id_66"
                   my-id="generated_id_66"
                   class="dest-class-generated_id_66"> C </div>
              <div @key=generated_id_70
                   kind="mapn"
                   src-name="B"
                   outside-id="generated_id_70"
                   my-id="generated_id_70"
                   class="dest-class-generated_id_70"> B </div>
            </div>
          </div>
          <div> </div>
        </div>
        <div> </div>
      </div>
    </div> |}];
  (* This is not ideal, but having this here showcases current behavior.

     Despite there being 4 edges in

     {v
          A
         /|\
        B C D
            |
            E
     v}


     only two of them are shown, namely A-D and D-E. The reason for this is that this test
     suite needs to manually set the position that each node would appear in the browser
     in order for the edges to be rendered. However, after topologically sorting, there is
     a topological gap between A-B and A-C. The reason for this is that redirect edges
     with id's generated using [count] were inserted between A-B and A-C. Since the ID's and
     structure number of redirect nodes can change, hardcoding the generated id could lead
     to brittle tests.
  *)
  set_positions ~handle ~ids:[ a; b; c; d; e ];
  Handle.show_diff handle;
  [%expect
    {|
      <div class="map_hash_replaced_in_test testcase_hash_replaced_in_test">
    -|  <svg> </svg>
    +|  <svg>
    +|    <pre> (edge ((from (User generated_id_62)) (to_ (User generated_id_58)))) </pre>
    +|    <pre> (edge ((from (User generated_id_71)) (to_ (User generated_id_62)))) </pre>
    +|  </svg>
        <div class="hbox_hash_replaced_in_test">
          <div> </div>
          <div class="vbox_hash_replaced_in_test">
            <div> </div>
            <div class="vbox_hash_replaced_in_test">
              <div @key=generated_id_71 data-kind="singleton" src-name="A" outside-id="generated_id_71"> A </div>
              <div class="hbox_hash_replaced_in_test">
                <div @key=generated_id_62
                     kind="mapn"
                     src-name="D"
                     outside-id="generated_id_62"
                     my-id="generated_id_62"
                     class="dest-class-generated_id_62"> D </div>
                <div class="vbox_hash_replaced_in_test">
                  <div @key=gen_4
                       data-kind="redirect" |}]
;;

let%expect_test "Cycle" =
  let open Dummy_nodes in
  let edges = [ a ==> b; a ==> c; a ==> d; d ==> a ] |> Edge.Set.of_list in
  (*
     {v
          A <--\
         /|\   |
        B C D -/


     v}
  *)
  let nodes = map_with_ids [ a; b; c; d ] in
  let dag_var = Bonsai.Var.create { edges; nodes } in
  let dag = Bonsai.Var.value dag_var in
  let handle = create_handle ~dag ~curr_id in
  Handle.show handle;
  [%expect {|
    <pre> (error cycle!) </pre> |}]
;;

let%expect_test "redirect nodes" =
  let open Dummy_nodes in
  let edges =
    [ a ==> b; a ==> c; a ==> e; a ==> g; b ==> d; c ==> e; d ==> e; e ==> g; f ==> g ]
    |> Edge.Set.of_list
  in
  (*
     {v
            a
         __/|\
        /   | |
        b   / |
        |  c  R      f
        d  | / \    /
         \ //   R  /
          e    /  /
           \  /  /
            \|__/
             g
     v}
  *)
  (* This test case showcases the creation of "redirect" nodes across different levels.
     "redirect" nodes are nodes that help route the different edges. In this particular test case
     there are two different redirect nodes created marked in the diagram with "R". *)
  let nodes = map_with_ids [ a; b; c; d; e; f; g ] in
  let dag_var = Bonsai.Var.create { edges; nodes } in
  let dag = Bonsai.Var.value dag_var in
  let handle = create_handle ~dag ~curr_id in
  Handle.show handle;
  [%expect
    {|
    <div class="map_hash_replaced_in_test testcase_hash_replaced_in_test">
      <svg> </svg>
      <div class="hbox_hash_replaced_in_test">
        <div> </div>
        <div class="vbox_hash_replaced_in_test">
          <div> </div>
          <div class="vbox_hash_replaced_in_test">
            <div @key=generated_id_101
                 data-kind="singleton"
                 src-name="A"
                 outside-id="generated_id_101"> A </div>
            <div class="hbox_hash_replaced_in_test">
              <div @key=generated_id_99
                   kind="mapn"
                   src-name="B"
                   outside-id="generated_id_99"
                   my-id="generated_id_99"
                   class="dest-class-generated_id_99"> B </div>
              <div class="vbox_hash_replaced_in_test">
                <div @key=gen_12
                     data-kind="redirect"
                     src-name="gen_11"
                     outside-id="gen_12"
                     class="dest-class-A redirect_hash_replaced_in_test">
                  <div> </div>
                </div>
                <div @key=gen_13
                     data-kind="redirect"
                     src-name="gen_10"
                     outside-id="gen_13"
                     class="dest-class-gen_11 redirect_hash_replaced_in_test">
                  <div> </div>
                </div>
              </div>
            </div>
            <div class="hbox_hash_replaced_in_test">
              <div @key=generated_id_91
                   kind="mapn"
                   src-name="D"
                   outside-id="generated_id_91"
                   my-id="generated_id_91"
                   class="dest-class-generated_id_91"> D </div>
              <div @key=generated_id_95
                   kind="mapn"
                   src-name="C"
                   outside-id="generated_id_95"
                   my-id="generated_id_95"
                   class="dest-class-generated_id_95"> C </div>
              <div class="vbox_hash_replaced_in_test">
                <div @key=gen_8
                     data-kind="redirect"
                     src-name="gen_7"
                     outside-id="gen_8"
                     class="dest-class-gen_10 redirect_hash_replaced_in_test">
                  <div> </div>
                </div>
                <div @key=gen_9
                     data-kind="redirect"
                     src-name="gen_6"
                     outside-id="gen_9"
                     class="dest-class-gen_7 redirect_hash_replaced_in_test">
                  <div> </div>
                </div>
              </div>
            </div>
            <div class="hbox_hash_replaced_in_test">
              <div @key=generated_id_100
                   data-kind="singleton"
                   src-name="F"
                   outside-id="generated_id_100"> F </div>
              <div @key=generated_id_87
                   kind="mapn"
                   src-name="E"
                   outside-id="generated_id_87"
                   my-id="generated_id_87"
                   class="dest-class-generated_id_87"> E </div>
              <div class="vbox_hash_replaced_in_test">
                <div @key=gen_4
                     data-kind="redirect"
                     src-name="gen_3"
                     outside-id="gen_4"
                     class="dest-class-gen_6 redirect_hash_replaced_in_test">
                  <div> </div>
                </div>
                <div @key=gen_5
                     data-kind="redirect"
                     src-name="gen_2"
                     outside-id="gen_5"
                     class="dest-class-gen_3 redirect_hash_replaced_in_test">
                  <div> </div>
                </div>
              </div>
            </div>
            <div class="hbox_hash_replaced_in_test">
              <div @key=generated_id_79
                   kind="mapn"
                   src-name="G"
                   outside-id="generated_id_79"
                   my-id="generated_id_79"
                   class="dest-class-generated_id_79"> G </div>
            </div>
          </div>
          <div> </div>
        </div>
        <div> </div>
      </div>
    </div> |}];
  (* For reasons similar to the test case above, (not knowing the id's of generated
     redirect nodes to set their positions), this test case does not show all of the edges
     that would be created in a browser environment.  *)
  set_positions ~handle ~ids:[ a; b; c; d; e; f; g ];
  Handle.show_diff handle;
  [%expect
    {|
      <div class="map_hash_replaced_in_test testcase_hash_replaced_in_test">
    -|  <svg> </svg>
    +|  <svg>
    +|    <pre> (edge ((from (User generated_id_100)) (to_ (User generated_id_79)))) </pre>
    +|    <pre> (edge ((from (User generated_id_101)) (to_ (User generated_id_99)))) </pre>
    +|    <pre> (edge ((from (User generated_id_87)) (to_ (User generated_id_79)))) </pre>
    +|    <pre> (edge ((from (User generated_id_91)) (to_ (User generated_id_87)))) </pre>
    +|    <pre> (edge ((from (User generated_id_95)) (to_ (User generated_id_87)))) </pre>
    +|    <pre> (edge ((from (User generated_id_99)) (to_ (User generated_id_91)))) </pre>
    +|  </svg>
        <div class="hbox_hash_replaced_in_test">
          <div> </div>
          <div class="vbox_hash_replaced_in_test">
            <div> </div>
            <div class="vbox_hash_replaced_in_test">
              <div @key=generated_id_101
                   data-kind="singleton"
                   src-name="A"
                   outside-id="generated_id_101"> A </div>
              <div class="hbox_hash_replaced_in_test">
                <div @key=generated_id_99
                     kind="mapn"
                     src-name="B"
                     outside-id="generated_id_99"
                     my-id="generated_id_99"
                     class="dest-class-generated_id_99"> B </div> |}]
;;

let%expect_test "Disjoint DAGs" =
  let open Dummy_nodes in
  let edges = [ a ==> b; c ==> d; e ==> f ] |> Edge.Set.of_list in
  (*
     {v
        A  C  E  G
        |  |  |
        B  D  F
     v}
  *)
  let nodes = map_with_ids [ a; b; c; d; e; f; g ] in
  let dag_var = Bonsai.Var.create { edges; nodes } in
  let dag = Bonsai.Var.value dag_var in
  let handle = create_handle ~dag ~curr_id in
  Handle.show handle;
  [%expect
    {|
    <div class="map_hash_replaced_in_test testcase_hash_replaced_in_test">
      <svg> </svg>
      <div class="hbox_hash_replaced_in_test">
        <div> </div>
        <div class="vbox_hash_replaced_in_test">
          <div> </div>
          <div class="vbox_hash_replaced_in_test">
            <div class="hbox_hash_replaced_in_test">
              <div @key=generated_id_117
                   data-kind="singleton"
                   src-name="A"
                   outside-id="generated_id_117"> A </div>
              <div @key=generated_id_116
                   data-kind="singleton"
                   src-name="C"
                   outside-id="generated_id_116"> C </div>
              <div @key=generated_id_115
                   data-kind="singleton"
                   src-name="E"
                   outside-id="generated_id_115"> E </div>
            </div>
            <div class="hbox_hash_replaced_in_test">
              <div @key=generated_id_114
                   data-kind="singleton"
                   src-name="G"
                   outside-id="generated_id_114"> G </div>
              <div @key=generated_id_113
                   kind="mapn"
                   src-name="B"
                   outside-id="generated_id_113"
                   my-id="generated_id_113"
                   class="dest-class-generated_id_113"> B </div>
              <div @key=generated_id_109
                   kind="mapn"
                   src-name="D"
                   outside-id="generated_id_109"
                   my-id="generated_id_109"
                   class="dest-class-generated_id_109"> D </div>
              <div @key=generated_id_105
                   kind="mapn"
                   src-name="F"
                   outside-id="generated_id_105"
                   my-id="generated_id_105"
                   class="dest-class-generated_id_105"> F </div>
            </div>
          </div>
          <div> </div>
        </div>
        <div> </div>
      </div>
    </div> |}];
  set_positions ~handle ~ids:[ a; b; c; d; e; f; g ];
  Handle.show_diff handle;
  [%expect
    {|
      <div class="map_hash_replaced_in_test testcase_hash_replaced_in_test">
    -|  <svg> </svg>
    +|  <svg>
    +|    <pre> (edge ((from (User generated_id_115)) (to_ (User generated_id_105)))) </pre>
    +|    <pre> (edge ((from (User generated_id_116)) (to_ (User generated_id_109)))) </pre>
    +|    <pre> (edge ((from (User generated_id_117)) (to_ (User generated_id_113)))) </pre>
    +|  </svg>
        <div class="hbox_hash_replaced_in_test">
          <div> </div>
          <div class="vbox_hash_replaced_in_test">
            <div> </div>
            <div class="vbox_hash_replaced_in_test">
              <div class="hbox_hash_replaced_in_test">
                <div @key=generated_id_117
                     data-kind="singleton"
                     src-name="A"
                     outside-id="generated_id_117"> A </div>
                <div @key=generated_id_116
                     data-kind="singleton"
                     src-name="C"
                     outside-id="generated_id_116"> C </div>
                <div @key=generated_id_115
                     data-kind="singleton" |}]
;;
