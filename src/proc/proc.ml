open! Core
open! Import
module Bonsai_cont = Bonsai
module Apply_action_context = Bonsai_cont.Apply_action_context

module Value = struct
  type 'a t = 'a Bonsai_cont.t

  let return ~(here : [%call_pos]) a =
    Value.return ~here a |> Bonsai_cont.Private.conceal_value
  ;;

  (* we depend on Proc's [map] function so that we can keep passing the [here] parameter
     for the let%arr and let%sub ppxes. *)
  let map ~(here : [%call_pos]) v ~f = Bonsai_cont.Private.For_proc.value_map ~here v ~f

  let transpose_opt opt =
    Option.value_map opt ~default:(return None) ~f:(map ~f:Option.some)
  ;;

  let cutoff ~(here : [%call_pos]) a ~equal =
    Bonsai_cont.Private.reveal_value a
    |> Value.cutoff ~here ~added_by_let_syntax:false ~equal
    |> Bonsai_cont.Private.conceal_value
  ;;

  module Mapn = struct
    let map2 = Bonsai_cont.map2
    let map3 = Bonsai_cont.map3
    let map4 = Bonsai_cont.map4
    let map5 = Bonsai_cont.map5
    let map6 = Bonsai_cont.map6
    let map7 = Bonsai_cont.map7
  end

  include Mapn

  include Applicative.Make_using_map2 (struct
      type nonrec 'a t = 'a t

      let return = return
      let map2 = map2
      let map ~(here : [%call_pos]) a ~f = map ~here a ~f
      let map = `Custom map
    end)

  let both ~(here : [%call_pos]) a b =
    Value.both
      ~here
      (Bonsai_cont.Private.reveal_value a)
      (Bonsai_cont.Private.reveal_value b)
    |> Bonsai_cont.Private.conceal_value
  ;;

  module Let_syntax = struct
    let ( >>| ) ~(here : [%call_pos]) a f =
      Value.map ~here (Bonsai_cont.Private.reveal_value a) ~f
      |> Bonsai_cont.Private.conceal_value
    ;;

    let ( <*> ) ~(here : [%call_pos]) f a =
      Value.map2
        ~here
        (Bonsai_cont.Private.reveal_value a)
        (Bonsai_cont.Private.reveal_value f)
        ~f:(fun a f -> f a)
      |> Bonsai_cont.Private.conceal_value
    ;;

    let ( <$> ) ~(here : [%call_pos]) f a =
      Bonsai_cont.Private.reveal_value a
      |> Value.map ~here ~f:(fun a -> f a)
      |> Bonsai_cont.Private.conceal_value
    ;;

    module Let_syntax = struct
      let map ~(here : [%call_pos]) v ~f =
        Bonsai_cont.Private.For_proc.value_map ~here v ~f
      ;;

      let cutoff ~(here : [%call_pos]) a ~equal =
        Bonsai_cont.Let_syntax.Let_syntax.cutoff ~here a ~equal
      ;;

      let both ~(here : [%call_pos]) a b =
        Value.both
          ~here
          (Bonsai_cont.Private.reveal_value a)
          (Bonsai_cont.Private.reveal_value b)
        |> Bonsai_cont.Private.conceal_value
      ;;

      include Mapn
    end
  end
end

module This_let_syntax = struct
  let comp_return ~(here : [%call_pos]) v graph =
    Bonsai_cont.Private.perform
      ~here
      graph
      (Bonsai_cont.Private.read ~here (Bonsai_cont.Private.reveal_value v))
  ;;

  include Value.Let_syntax

  let return = comp_return

  module Let_syntax = struct
    include Value.Let_syntax.Let_syntax

    let subcomputation ~(here : [%call_pos]) a graph =
      Bonsai_cont.Private.handle ~here graph ~f:(fun graph -> a graph)
      |> Bonsai_cont.Private.perform ~here graph
    ;;

    let sub ~(here : [%call_pos]) a ~f graph = f (subcomputation ~here a graph) graph
    let return = comp_return
    let arr ~(here : [%call_pos]) v ~f graph = Bonsai_cont.arr1 ~here graph v ~f

    let arr2 ~(here : [%call_pos]) v1 v2 ~f graph =
      Bonsai_cont.Private.For_proc.arr2 ~here graph v1 v2 ~f
    ;;

    let arr3 ~(here : [%call_pos]) v1 v2 v3 ~f graph =
      Bonsai_cont.Private.For_proc.arr3 ~here graph v1 v2 v3 ~f
    ;;

    let arr4 ~(here : [%call_pos]) v1 v2 v3 v4 ~f graph =
      Bonsai_cont.Private.For_proc.arr4 ~here graph v1 v2 v3 v4 ~f
    ;;

    let arr5 ~(here : [%call_pos]) v1 v2 v3 v4 v5 ~f graph =
      Bonsai_cont.Private.For_proc.arr5 ~here graph v1 v2 v3 v4 v5 ~f
    ;;

    let arr6 ~(here : [%call_pos]) v1 v2 v3 v4 v5 v6 ~f graph =
      Bonsai_cont.Private.For_proc.arr6 ~here graph v1 v2 v3 v4 v5 v6 ~f
    ;;

    let arr7 ~(here : [%call_pos]) v1 v2 v3 v4 v5 v6 v7 ~f graph =
      Bonsai_cont.Private.For_proc.arr7 ~here graph v1 v2 v3 v4 v5 v6 v7 ~f
    ;;

    let switch ~here ~match_ ~branches ~with_ graph =
      Bonsai_cont.Private.For_proc.switch ~here ~match_ ~branches ~with_ graph
    ;;
  end
end

module Computation = struct
  type 'a t = local_ Bonsai_cont.graph -> 'a Bonsai_cont.t

  include Applicative.Make_using_map2 (struct
      type nonrec 'a t = 'a t

      let return ~(here : [%call_pos]) (a : 'a) : 'a t =
        fun _graph -> Value.return ~here a
      ;;

      let map2 ~(here : [%call_pos]) a b ~f graph =
        let a = a graph
        and b = b graph in
        Bonsai_cont.arr2 ~here graph a b ~f
      ;;

      let map ~(here : [%call_pos]) a ~f graph = Bonsai_cont.arr1 ~here graph (a graph) ~f
      let map = `Custom map
    end)

  let read = This_let_syntax.return
  let computation_return = return

  open This_let_syntax

  let return = computation_return

  module Mapn = struct
    let map2 = map2

    let map3 ~(here : [%call_pos]) t1 t2 t3 ~f =
      let%sub t1 in
      let%sub t2 in
      let%sub t3 in
      read (Value.Let_syntax.Let_syntax.map3 ~here t1 t2 t3 ~f)
    ;;

    let map4 ~(here : [%call_pos]) t1 t2 t3 t4 ~f =
      let%sub t1 in
      let%sub t2 in
      let%sub t3 in
      let%sub t4 in
      read (Value.Let_syntax.Let_syntax.map4 ~here t1 t2 t3 t4 ~f)
    ;;

    let map5 ~(here : [%call_pos]) t1 t2 t3 t4 t5 ~f =
      let%sub t1 in
      let%sub t2 in
      let%sub t3 in
      let%sub t4 in
      let%sub t5 in
      read (Value.Let_syntax.Let_syntax.map5 ~here t1 t2 t3 t4 t5 ~f)
    ;;

    let map6 ~(here : [%call_pos]) t1 t2 t3 t4 t5 t6 ~f =
      let%sub t1 in
      let%sub t2 in
      let%sub t3 in
      let%sub t4 in
      let%sub t5 in
      let%sub t6 in
      read (Value.Let_syntax.Let_syntax.map6 ~here t1 t2 t3 t4 t5 t6 ~f)
    ;;

    let map7 ~(here : [%call_pos]) t1 t2 t3 t4 t5 t6 t7 ~f =
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

  let all ~(here : [%call_pos]) xs = Let_syntax.subcomputation ~here (all ~here xs)

  let reduce_balanced ~(here : [%call_pos]) xs ~f =
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

  let fold_right ~(here : [%call_pos]) xs ~f ~init =
    List.fold_right xs ~init:(read init) ~f:(fun a b ->
      let%sub a in
      let%sub b in
      f a b)
  ;;

  let fold_right xs ~f ~init = Let_syntax.subcomputation (fold_right xs ~f ~init)
  let all_unit ~here xs = all ~here xs |> map ~here ~f:(fun (_ : unit list) -> ())
  let all_unit ~(here : [%call_pos]) xs = Let_syntax.subcomputation (all_unit ~here xs)

  let all_map ~(here : [%call_pos]) map_of_computations =
    map_of_computations
    |> Map.to_alist
    |> List.map ~f:(fun (key, data) -> map ~here data ~f:(Tuple2.create key))
    |> all ~here
    |> map ~here ~f:(Map.of_alist_exn (Map.comparator_s map_of_computations))
  ;;

  let all_map ~(here : [%call_pos]) map_of_computations =
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

module Var = Bonsai_cont.Expert.Var
module Effect = Effect

module For_open = struct
  module Computation = Computation
  module Effect = Effect
  module Value = Value
end

include (
  Bonsai_cont.Cont :
    module type of Bonsai_cont.Cont
    with module Let_syntax := Bonsai_cont.Cont.Let_syntax
    with module Apply_action_context := Apply_action_context
    with module Effect := Effect)

include Bonsai_cont.Private.For_proc

let path_id ~(here : [%call_pos]) () (local_ graph) = path_id ~here graph
let path ~(here : [%call_pos]) () (local_ graph) = path ~here graph

let toggle ~(here : [%call_pos]) ~default_model () (local_ graph) =
  toggle ~here ~default_model graph
;;

let toggle' ~(here : [%call_pos]) ~default_model () (local_ graph) =
  toggle' ~here ~default_model graph
;;

open Bonsai_cont.Let_syntax

open struct
  module Map = Core.Map
end

let read v _graph = v
let const ~(here : [%call_pos]) a _graph = return ~here a
let pure ~(here : [%call_pos]) f i _graph = map ~here i ~f
let scope_model ~(here : [%call_pos]) cmp ~on for_ = scope_model ~here cmp ~on ~for_
let yoink = peek

module Clock = struct
  open Clock
  module Before_or_after = Before_or_after

  let approx_now ~(here : [%call_pos]) ~tick_every () (local_ graph) =
    approx_now ~here ~tick_every graph
  ;;

  let get_current_time ~(here : [%call_pos]) () (local_ graph) =
    get_current_time ~here graph
  ;;

  let every
    ~(here : [%call_pos])
    ~when_to_start_next_effect
    ?trigger_on_activate
    time_span
    callback
    graph
    =
    every ~here ~when_to_start_next_effect ?trigger_on_activate time_span callback graph;
    return ~here ()
  ;;

  let sleep ~(here : [%call_pos]) () (local_ graph) = sleep ~here graph
  let until ~(here : [%call_pos]) () (local_ graph) = until ~here graph
  let at = at

  module Expert = struct
    let now ~(here : [%call_pos]) () (local_ graph) = Expert.now ~here graph
  end
end

module Incr = struct
  include Incr

  let with_clock ~(here : [%call_pos]) f = with_clock ~here ~f
end

module Edge = struct
  open Edge

  let on_change = Bonsai_cont.Private.For_proc.on_change
  let on_change' = Bonsai_cont.Private.For_proc.on_change'
  let lifecycle = Bonsai_cont.Private.For_proc.lifecycle
  let lifecycle' = Bonsai_cont.Private.For_proc.lifecycle'
  let before_display = Bonsai_cont.Private.For_proc.before_display
  let before_display' = Bonsai_cont.Private.For_proc.before_display'
  let after_display = Bonsai_cont.Private.For_proc.after_display
  let after_display' = Bonsai_cont.Private.For_proc.after_display'

  let wait_after_display ~(here : [%call_pos]) () (local_ graph) =
    wait_after_display ~here graph
  ;;

  module Poll = struct
    include Poll

    let manual_refresh = Bonsai_cont.Private.For_proc.manual_refresh
  end
end

module Debug = struct
  include Debug

  let on_change = debug_on_change
  let on_change_print_s = debug_on_change_print_s
end

module Expert = struct
  include Expert

  let thunk ~(here : [%call_pos]) f graph = thunk ~here ~f graph
end

let of_module_with_input
  (type i m a r)
  ~(here : [%call_pos])
  ?sexp_of_model
  (component : (i, m, a, r) component_s)
  ?equal
  ~default_model
  input
  (local_ graph)
  =
  let (module M) = component in
  let model, inject =
    Bonsai_cont.state_machine_with_input
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
              "An action sent to an [of_module_with_input] has been dropped because its \
               input was not present. This happens when the [of_module_with_input] is \
               inactive when it receives a message."
                (action : M.Action.t)];
          model)
      input
      graph
  in
  let%map model and inject and input in
  M.compute ~inject input model
;;

let of_module
  (type m a r)
  ?sexp_of_model
  ?equal
  (component : (unit, m, a, r) component_s)
  ~default_model
  graph
  =
  let (module M) = component in
  let model, inject =
    Bonsai_cont.state_machine
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

let of_module2 ~(here : [%call_pos]) ?sexp_of_model c ?equal ~default_model i1 i2 =
  of_module_with_input ~here ?sexp_of_model c ?equal ~default_model (both ~here i1 i2)
;;

let enum
  (type k)
  ~(here : [%call_pos])
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
  Bonsai_cont.Private.For_proc.switch ~here ~match_ ~branches ~with_ graph
;;

let sub = This_let_syntax.Let_syntax.sub

module Map = Bonsai_cont.Cont.Map
module Set = Bonsai_cont.Cont.Set
module Let_syntax = This_let_syntax
