open! Core
open! Import

(* > What is [proc_layer2] and why is it needed?
   The tower of bonsai implementations are as follows
   1. proc_min    : the bare minimum bonsai combinators
   2. proc        : includes bonsai combinators that can be built on top of proc_min
   3. cont        : implements the local-graph API on top of proc
   4. proc_layer2 : re-implements the proc API on top of cont

   The reason that the 4th layer is necessary is so that its `Computation.t` can be defined
   to be exactly `local_ Cont.graph -> 'a Cont.t` and its `Value.t` can be defined to be
   exactly `'a Cont.t`. *)

(* These aren't pulled from `Cont` because they are no longer recommended,
   and therefore not included in the new API. *)
module type Model = Module_types.Model
module type Action = Module_types.Action

module Apply_action_context = Proc.Apply_action_context

module Value = struct
  type 'a t = 'a Cont.t

  let return ?(here = Stdlib.Lexing.dummy_pos) a =
    Value.return ~here a |> Cont.Conv.conceal_value
  ;;

  (* we depend on Proc's [map] function so that we can keep passing
     the [here] parameter for the let%arr and let%sub ppxes. *)
  let map ?(here = Stdlib.Lexing.dummy_pos) v ~f =
    Proc.Let_syntax.Let_syntax.map ~here (Cont.Conv.reveal_value v) ~f
    |> Cont.Conv.conceal_value
  ;;

  let transpose_opt opt =
    Option.value_map opt ~default:(return None) ~f:(map ~f:Option.some)
  ;;

  let cutoff ?(here = Stdlib.Lexing.dummy_pos) a ~equal =
    Cont.Conv.reveal_value a
    |> Value.cutoff ~here ~added_by_let_syntax:false ~equal
    |> Cont.Conv.conceal_value
  ;;

  module Mapn = struct
    let map2 = Cont.map2
    let map3 = Cont.map3
    let map4 = Cont.map4
    let map5 = Cont.map5
    let map6 = Cont.map6
    let map7 = Cont.map7
  end

  include Mapn

  include Applicative.Make_using_map2 (struct
      type nonrec 'a t = 'a t

      let return = return
      let map2 = map2
      let map ?(here = Stdlib.Lexing.dummy_pos) a ~f = map ~here a ~f
      let map = `Custom map
    end)

  let both ?(here = Stdlib.Lexing.dummy_pos) a b =
    Value.both ~here (Cont.Conv.reveal_value a) (Cont.Conv.reveal_value b)
    |> Cont.Conv.conceal_value
  ;;

  module Let_syntax = struct
    let ( >>| ) ?(here = Stdlib.Lexing.dummy_pos) a f =
      Value.map ~here (Cont.Conv.reveal_value a) ~f |> Cont.Conv.conceal_value
    ;;

    let ( <*> ) ?(here = Stdlib.Lexing.dummy_pos) f a =
      Value.map2
        ~here
        (Cont.Conv.reveal_value a)
        (Cont.Conv.reveal_value f)
        ~f:(fun a f -> f a)
      |> Cont.Conv.conceal_value
    ;;

    let ( <$> ) ?(here = Stdlib.Lexing.dummy_pos) f a =
      Cont.Conv.reveal_value a
      |> Value.map ~here ~f:(fun a -> f a)
      |> Cont.Conv.conceal_value
    ;;

    module Let_syntax = struct
      let map ?(here = Stdlib.Lexing.dummy_pos) v ~f =
        Proc.Let_syntax.Let_syntax.map ~here (Cont.Conv.reveal_value v) ~f
        |> Cont.Conv.conceal_value
      ;;

      let cutoff ?(here = Stdlib.Lexing.dummy_pos) a ~equal =
        Cont.Conv.reveal_value a
        |> Proc.Let_syntax.Let_syntax.cutoff ~here ~equal
        |> Cont.Conv.conceal_value
      ;;

      let both ?(here = Stdlib.Lexing.dummy_pos) a b =
        Value.both ~here (Cont.Conv.reveal_value a) (Cont.Conv.reveal_value b)
        |> Cont.Conv.conceal_value
      ;;

      include Mapn
    end
  end
end

module This_let_syntax = struct
  let comp_return ?(here = Stdlib.Lexing.dummy_pos) v graph =
    Cont.Conv.perform ~here graph (Proc.read ~here (Cont.Conv.reveal_value v))
  ;;

  include Value.Let_syntax

  let return = comp_return

  module Let_syntax = struct
    include Value.Let_syntax.Let_syntax

    let subcomputation ?(here = Stdlib.Lexing.dummy_pos) a graph =
      Cont.Conv.handle ~here graph ~f:(fun graph -> a graph)
      |> Cont.Conv.perform ~here graph
    ;;

    let sub ?(here = Stdlib.Lexing.dummy_pos) a ~f graph =
      f (subcomputation ~here a graph) graph
    ;;

    let return = comp_return

    let arr ?(here = Stdlib.Lexing.dummy_pos) v ~f graph =
      Cont.For_proc2.arr1 ~here graph v ~f
    ;;

    let arr2 ?(here = Stdlib.Lexing.dummy_pos) v1 v2 ~f graph =
      Cont.For_proc2.arr2 ~here graph v1 v2 ~f
    ;;

    let arr3 ?(here = Stdlib.Lexing.dummy_pos) v1 v2 v3 ~f graph =
      Cont.For_proc2.arr3 ~here graph v1 v2 v3 ~f
    ;;

    let arr4 ?(here = Stdlib.Lexing.dummy_pos) v1 v2 v3 v4 ~f graph =
      Cont.For_proc2.arr4 ~here graph v1 v2 v3 v4 ~f
    ;;

    let arr5 ?(here = Stdlib.Lexing.dummy_pos) v1 v2 v3 v4 v5 ~f graph =
      Cont.For_proc2.arr5 ~here graph v1 v2 v3 v4 v5 ~f
    ;;

    let arr6 ?(here = Stdlib.Lexing.dummy_pos) v1 v2 v3 v4 v5 v6 ~f graph =
      Cont.For_proc2.arr6 ~here graph v1 v2 v3 v4 v5 v6 ~f
    ;;

    let arr7 ?(here = Stdlib.Lexing.dummy_pos) v1 v2 v3 v4 v5 v6 v7 ~f graph =
      Cont.For_proc2.arr7 ~here graph v1 v2 v3 v4 v5 v6 v7 ~f
    ;;

    let switch ~here ~match_ ~branches ~with_ graph =
      Cont.For_proc2.switch ~here ~match_ ~branches ~with_ graph
    ;;
  end
end

module Computation = struct
  type 'a t = Cont.graph -> 'a Cont.t

  include Applicative.Make_using_map2 (struct
      type nonrec 'a t = 'a t

      let return ?(here = Stdlib.Lexing.dummy_pos) (a : 'a) : 'a t =
        fun _graph -> Value.return ~here a
      ;;

      let map2 ?(here = Stdlib.Lexing.dummy_pos) a b ~f graph =
        let a = a graph
        and b = b graph in
        Cont.arr2 ~here graph a b ~f
      ;;

      let map ?(here = Stdlib.Lexing.dummy_pos) a ~f graph =
        Cont.arr1 ~here graph (a graph) ~f
      ;;

      let map = `Custom map
    end)

  let read = This_let_syntax.return
  let computation_return = return

  open This_let_syntax

  let return = computation_return

  module Mapn = struct
    let map2 = map2

    let map3 ?(here = Stdlib.Lexing.dummy_pos) t1 t2 t3 ~f =
      let%sub t1 in
      let%sub t2 in
      let%sub t3 in
      read (Value.Let_syntax.Let_syntax.map3 ~here t1 t2 t3 ~f)
    ;;

    let map4 ?(here = Stdlib.Lexing.dummy_pos) t1 t2 t3 t4 ~f =
      let%sub t1 in
      let%sub t2 in
      let%sub t3 in
      let%sub t4 in
      read (Value.Let_syntax.Let_syntax.map4 ~here t1 t2 t3 t4 ~f)
    ;;

    let map5 ?(here = Stdlib.Lexing.dummy_pos) t1 t2 t3 t4 t5 ~f =
      let%sub t1 in
      let%sub t2 in
      let%sub t3 in
      let%sub t4 in
      let%sub t5 in
      read (Value.Let_syntax.Let_syntax.map5 ~here t1 t2 t3 t4 t5 ~f)
    ;;

    let map6 ?(here = Stdlib.Lexing.dummy_pos) t1 t2 t3 t4 t5 t6 ~f =
      let%sub t1 in
      let%sub t2 in
      let%sub t3 in
      let%sub t4 in
      let%sub t5 in
      let%sub t6 in
      read (Value.Let_syntax.Let_syntax.map6 ~here t1 t2 t3 t4 t5 t6 ~f)
    ;;

    let map7 ?(here = Stdlib.Lexing.dummy_pos) t1 t2 t3 t4 t5 t6 t7 ~f =
      let%sub t1 in
      let%sub t2 in
      let%sub t3 in
      let%sub t4 in
      let%sub t5 in
      let%sub t6 in
      let%sub t7 in
      read (Value.Let_syntax.Let_syntax.map7 ~here t1 t2 t3 t4 t5 t6 t7 ~f)
    ;;
  end

  include Mapn

  let rec all ~here = function
    | [] -> return ~here []
    | [ t1 ] -> map ~here t1 ~f:(fun a1 -> [ a1 ])
    | [ t1; t2 ] -> map2 ~here t1 t2 ~f:(fun a1 a2 -> [ a1; a2 ])
    | [ t1; t2; t3 ] -> map3 ~here t1 t2 t3 ~f:(fun a1 a2 a3 -> [ a1; a2; a3 ])
    | [ t1; t2; t3; t4 ] ->
      map4 ~here t1 t2 t3 t4 ~f:(fun a1 a2 a3 a4 -> [ a1; a2; a3; a4 ])
    | [ t1; t2; t3; t4; t5 ] ->
      map5 ~here t1 t2 t3 t4 t5 ~f:(fun a1 a2 a3 a4 a5 -> [ a1; a2; a3; a4; a5 ])
    | [ t1; t2; t3; t4; t5; t6 ] ->
      map6 ~here t1 t2 t3 t4 t5 t6 ~f:(fun a1 a2 a3 a4 a5 a6 ->
        [ a1; a2; a3; a4; a5; a6 ])
    | [ t1; t2; t3; t4; t5; t6; t7 ] ->
      map7 ~here t1 t2 t3 t4 t5 t6 t7 ~f:(fun a1 a2 a3 a4 a5 a6 a7 ->
        [ a1; a2; a3; a4; a5; a6; a7 ])
    | t1 :: t2 :: t3 :: t4 :: t5 :: t6 :: t7 :: rest ->
      let left =
        map7 ~here t1 t2 t3 t4 t5 t6 t7 ~f:(fun a1 a2 a3 a4 a5 a6 a7 ->
          [ a1; a2; a3; a4; a5; a6; a7 ])
      in
      let right = all ~here rest in
      map2 ~here left right ~f:(fun left right -> left @ right)
  ;;

  let all ?(here = Stdlib.Lexing.dummy_pos) xs =
    Let_syntax.subcomputation ~here (all ~here xs)
  ;;

  let reduce_balanced ?(here = Stdlib.Lexing.dummy_pos) xs ~f =
    List.reduce_balanced xs ~f:(fun a b ->
      let%sub a in
      let%sub b in
      f a b)
  ;;

  let reduce_balanced xs ~f =
    match xs with
    | [] -> None
    | _ -> Some (Let_syntax.subcomputation (Option.value_exn (reduce_balanced xs ~f)))
  ;;

  let fold_right ?(here = Stdlib.Lexing.dummy_pos) xs ~f ~init =
    List.fold_right xs ~init:(read init) ~f:(fun a b ->
      let%sub a in
      let%sub b in
      f a b)
  ;;

  let fold_right xs ~f ~init = Let_syntax.subcomputation (fold_right xs ~f ~init)
  let all_unit ~here xs = all ~here xs |> map ~here ~f:(fun (_ : unit list) -> ())

  let all_unit ?(here = Stdlib.Lexing.dummy_pos) xs =
    Let_syntax.subcomputation (all_unit ~here xs)
  ;;

  let all_map ?(here = Stdlib.Lexing.dummy_pos) map_of_computations =
    map_of_computations
    |> Map.to_alist
    |> List.map ~f:(fun (key, data) -> map ~here data ~f:(Tuple2.create key))
    |> all ~here
    |> map ~here ~f:(Map.of_alist_exn (Map.comparator_s map_of_computations))
  ;;

  let all_map ?(here = Stdlib.Lexing.dummy_pos) map_of_computations =
    Let_syntax.subcomputation ~here (all_map ~here map_of_computations)
  ;;

  module Let_syntax = struct
    let return = return

    include Applicative_infix

    module Let_syntax = struct
      let return = return
      let map = map
      let both = both

      include Mapn
    end
  end
end

module Var = Cont.Expert.Var
module Effect = Effect
module Private_value = Value
module Private_computation = Computation

module For_open = struct
  module Computation = Computation
  module Effect = Effect
  module Value = Value
end

include (
  Cont :
    module type of Cont
    with module Let_syntax := Cont.Let_syntax
    with module Apply_action_context := Apply_action_context
    with module Effect := Effect)

include Cont.For_proc2

let path_id ?(here = Stdlib.Lexing.dummy_pos) () graph = path_id ~here graph
let path ?(here = Stdlib.Lexing.dummy_pos) () graph = path ~here graph

let toggle ?(here = Stdlib.Lexing.dummy_pos) ~default_model () graph =
  toggle ~here ~default_model graph
;;

let toggle' ?(here = Stdlib.Lexing.dummy_pos) ~default_model () graph =
  toggle' ~here ~default_model graph
;;

open Cont.Let_syntax

open struct
  module Map = Core.Map
end

let read v _graph = v
let const ?(here = Stdlib.Lexing.dummy_pos) a _graph = return ~here a
let pure ?(here = Stdlib.Lexing.dummy_pos) f i _graph = map ~here i ~f

let scope_model ?(here = Stdlib.Lexing.dummy_pos) cmp ~on for_ =
  scope_model ~here cmp ~on ~for_
;;

let yoink = peek

module Clock = struct
  open Clock
  module Before_or_after = Before_or_after

  let approx_now ?(here = Stdlib.Lexing.dummy_pos) ~tick_every () graph =
    approx_now ~here ~tick_every graph
  ;;

  let now ?(here = Stdlib.Lexing.dummy_pos) () graph = now ~here graph

  let get_current_time ?(here = Stdlib.Lexing.dummy_pos) () graph =
    get_current_time ~here graph
  ;;

  let every
    ?(here = Stdlib.Lexing.dummy_pos)
    ~when_to_start_next_effect
    ?trigger_on_activate
    time_span
    callback
    graph
    =
    every ~here ~when_to_start_next_effect ?trigger_on_activate time_span callback graph;
    return ~here ()
  ;;

  let sleep ?(here = Stdlib.Lexing.dummy_pos) () graph = sleep ~here graph
  let until ?(here = Stdlib.Lexing.dummy_pos) () graph = until ~here graph
  let at = at
end

module Incr = struct
  include Incr

  let with_clock ?(here = Stdlib.Lexing.dummy_pos) f = with_clock ~here ~f
end

module Edge = struct
  open Edge

  let on_change = For_proc2.on_change
  let on_change' = For_proc2.on_change'
  let lifecycle = For_proc2.lifecycle
  let lifecycle' = For_proc2.lifecycle'
  let after_display = For_proc2.after_display
  let after_display' = For_proc2.after_display'

  let wait_after_display ?(here = Stdlib.Lexing.dummy_pos) () graph =
    wait_after_display ~here graph
  ;;

  module Poll = struct
    include Poll

    let manual_refresh = For_proc2.manual_refresh
  end
end

module Debug = struct
  include Debug

  let on_change = debug_on_change
  let on_change_print_s = debug_on_change_print_s
end

module Expert = struct
  include Expert

  let thunk ?(here = Stdlib.Lexing.dummy_pos) f graph = thunk ~here ~f graph
end

let of_module1
  (type i m a r)
  ?(here = Stdlib.Lexing.dummy_pos)
  ?sexp_of_model
  (component : (i, m, a, r) component_s)
  ?equal
  ~default_model
  input
  graph
  =
  let (module M) = component in
  let model, inject =
    Cont.state_machine1
      ~here
      ~sexp_of_action:M.Action.sexp_of_t
      ?sexp_of_model
      ?equal
      ~default_model
      ~apply_action:(fun ctx input model action ->
        match input with
        | Active input -> M.apply_action ctx input model action
        | Inactive ->
          eprint_s
            [%message
              "An action sent to an [of_module1] has been dropped because its input was \
               not present. This happens when the [of_module1] is inactive when it \
               receives a message."
                (action : M.Action.t)];
          model)
      input
      graph
  in
  let%map model and inject and input in
  M.compute ~inject input model
;;

let of_module0
  (type m a r)
  ?sexp_of_model
  ?equal
  (component : (unit, m, a, r) component_s)
  ~default_model
  graph
  =
  let (module M) = component in
  let model, inject =
    Cont.state_machine0
      ~sexp_of_action:M.Action.sexp_of_t
      ?sexp_of_model
      ?equal
      ~default_model
      ~apply_action:(fun ctx -> M.apply_action ctx ())
      graph
  in
  let%map model and inject in
  M.compute ~inject () model
;;

let of_module2
  ?(here = Stdlib.Lexing.dummy_pos)
  ?sexp_of_model
  c
  ?equal
  ~default_model
  i1
  i2
  =
  of_module1 ~here ?sexp_of_model c ?equal ~default_model (both ~here i1 i2)
;;

let enum
  (type k)
  ?(here = Stdlib.Lexing.dummy_pos)
  (module E : Enum with type t = k)
  ~match_
  ~with_
  graph
  =
  let module E = struct
    include E
    include Comparator.Make (E)
  end
  in
  let forward_index = List.to_array E.all in
  let reverse_index =
    Map.of_alist_exn (module E) (List.mapi E.all ~f:(fun i k -> k, i))
  in
  let match_ = ( >>| ) match_ ~here (Map.find_exn reverse_index) in
  let branches = Array.length forward_index in
  let with_ i = with_ (Array.get forward_index i) in
  For_proc2.switch ~here ~match_ ~branches ~with_ graph
;;

let sub = This_let_syntax.Let_syntax.sub

module Map = Cont.Map
module Let_syntax = This_let_syntax
