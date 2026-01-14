open! Core
open! Import

(* Constant folding an assoc with a large constant input (> 50,000 in tests) results in a
   stack overflow on startup.

   It can also result in significantly slower startup, and less efficient graph structure.
   It's cheaper to maintain one 1,000-input assoc around a `Bonsai.state` than 1,000
   separate `Bonsai.state`s.

   The value here is relatively arbitrary.
*)
let assoc_max_input_size_to_fold = 20

module Constants_in_scope =
  Univ_map.Make
    (Univ_map.Type_id_key)
    (struct
      include Value

      let sexp_of_t _ = sexp_of_opaque
    end)

module Evaluated = struct
  type t =
    | Unconditionally
    | Maybe
end

module Types = struct
  module Down = struct
    type t =
      { constants_in_scope : Constants_in_scope.t
      ; evaluated : Evaluated.t
      }
  end

  module Acc = Unit
  module Up = Fix_transform.Unit
end

open Types.Down

include struct
  let wrap_value ~(here : [%call_pos]) v = { Value.value = v; here }

  let value_exception_folder ~f =
    try f () with
    | exn -> wrap_value (Value.Exception exn)
  ;;

  let computation_exception_folder ~here ~f =
    try f () with
    | exn ->
      Trampoline.return (Computation.Return { value = wrap_value (Exception exn); here })
  ;;

  let lazy_contents_if_value_is_constant : type a. a Value.t -> a Lazy.t option =
    fun { value; here = _ } ->
    match value with
    | Incr _
    | Named _
    | Both _
    | Cutoff _
    | Map _
    | Map2 _
    | Map3 _
    | Map4 _
    | Map5 _
    | Map6 _
    | Map7 _ -> None
    | Constant x -> Some x
    | Exception ex -> Some (lazy (raise ex))
  ;;

  let force_but_capture_exceptions ({ Value.value; here } as t) =
    match value with
    | Constant x ->
      (try
         let _ : _ = Lazy.force x in
         t
       with
       | exn -> wrap_value ~here (Value.Exception exn))
    | _ -> t
  ;;

  let value_is_constant value = Option.is_some (lazy_contents_if_value_is_constant value)

  let constant_or_value (with_id : _ Value.t) ~f =
    value_exception_folder ~f:(fun () ->
      match f () with
      | Some (`Constant constant) -> { with_id with value = Constant constant }
      | Some (`Value value) -> { with_id with value }
      | None -> with_id)
  ;;

  let simplify_assoc_if_simpl
    (type k v cmp)
    ~(here : Source_code_position.t)
    ~(key_comparator : (k, cmp) Comparator.Module.t)
    ~(key_id : k Type_equal.Id.t)
    ~(data_id : v Type_equal.Id.t)
    (map : (k, v, cmp) Map.t Value.t)
    by
    =
    let module C = (val key_comparator) in
    let%map.Option by, may_contain =
      Simplify.computation_to_function
        by
        ~key_compare:(Comparator.compare C.comparator)
        ~key_id
        ~data_id
    in
    Computation.Assoc_simpl { map; by; may_contain; here }
  ;;
end

module Constant_fold (Recurse : Fix_transform.Recurse with module Types := Types) = struct
  let transform_v (type a) { constants_in_scope; evaluated } (value : a Value.t)
    : a Value.t
    =
    let (), (), ({ Value.value; here = _ } as value_with_position) =
      Recurse.on_value { constants_in_scope; evaluated } () `Skipping_over value
    in
    let rebuild value = { value_with_position with value } in
    let open Option.Let_syntax in
    match value with
    | Exception _ | Constant _ | Incr _ -> value_with_position
    | Named (_, id) ->
      (match Constants_in_scope.find constants_in_scope id with
       | Some value -> value
       | None -> value_with_position)
    | Both (a, b) as original ->
      value_exception_folder ~f:(fun () ->
        let value =
          match
            lazy_contents_if_value_is_constant a, lazy_contents_if_value_is_constant b
          with
          | Some l, Some r -> Value.Constant (lazy (Lazy.force l, Lazy.force r))
          | Some l, None -> Map { t = b; f = (fun b -> Lazy.force l, b) }
          | None, Some r -> Map { t = a; f = (fun a -> a, Lazy.force r) }
          | None, None -> original
        in
        rebuild value)
    | Cutoff { t; equal; added_by_let_syntax = was_wrapper_cutoff_added_by_let_syntax } as
      original ->
      value_exception_folder ~f:(fun () ->
        rebuild
          (match lazy_contents_if_value_is_constant t, t.value with
           | Some v, _ -> Constant v
           | ( None
             , Cutoff
                 { t
                 ; equal = inner_equal
                 ; added_by_let_syntax = was_nested_cutoff_added_by_let_syntax
                 } ) ->
             let added_by_let_syntax =
               was_wrapper_cutoff_added_by_let_syntax
               && was_nested_cutoff_added_by_let_syntax
             in
             Cutoff
               { t
               ; equal = (fun a b -> inner_equal a b || equal a b)
               ; added_by_let_syntax
               }
           | None, _ -> original))
    | Map { t; f } ->
      constant_or_value value_with_position ~f:(fun () ->
        let%map t1 = lazy_contents_if_value_is_constant t in
        `Constant (Lazy.map t1 ~f))
    | Map2 { t1; t2; f } ->
      constant_or_value value_with_position ~f:(fun () ->
        match
          lazy_contents_if_value_is_constant t1, lazy_contents_if_value_is_constant t2
        with
        | Some t1, Some t2 -> Some (`Constant (lazy (f (Lazy.force t1) (Lazy.force t2))))
        | Some t1, None ->
          Some (`Value (Value.Map { t = t2; f = (fun t2 -> f (Lazy.force t1) t2) }))
        | None, Some t2 ->
          Some (`Value (Value.Map { t = t1; f = (fun t1 -> f t1 (Lazy.force t2)) }))
        | None, None -> None)
    | Map3 { t1; t2; t3; f } ->
      constant_or_value value_with_position ~f:(fun () ->
        match
          ( lazy_contents_if_value_is_constant t1
          , lazy_contents_if_value_is_constant t2
          , lazy_contents_if_value_is_constant t3 )
        with
        | Some t1, Some t2, Some t3 ->
          Some (`Constant (lazy (f (Lazy.force t1) (Lazy.force t2) (Lazy.force t3))))
        | Some t1, Some t2, None ->
          Some
            (`Value
              (Value.Map { t = t3; f = (fun t3 -> f (Lazy.force t1) (Lazy.force t2) t3) }))
        | Some t1, None, Some t3 ->
          Some
            (`Value
              (Value.Map { t = t2; f = (fun t2 -> f (Lazy.force t1) t2 (Lazy.force t3)) }))
        | None, Some t2, Some t3 ->
          Some
            (`Value
              (Value.Map { t = t1; f = (fun t1 -> f t1 (Lazy.force t2) (Lazy.force t3)) }))
        | Some t1, None, None ->
          Some
            (`Value
              (Value.Map2 { t1 = t2; t2 = t3; f = (fun t2 t3 -> f (Lazy.force t1) t2 t3) }))
        | None, None, Some t3 ->
          Some
            (`Value (Value.Map2 { t1; t2; f = (fun t1 t2 -> f t1 t2 (Lazy.force t3)) }))
        | None, Some t2, None ->
          Some
            (`Value
              (Value.Map2 { t1; t2 = t3; f = (fun t1 t3 -> f t1 (Lazy.force t2) t3) }))
        | None, None, None -> None)
    | Map4 { t1; t2; t3; t4; f } ->
      constant_or_value value_with_position ~f:(fun () ->
        match
          ( lazy_contents_if_value_is_constant t1
          , lazy_contents_if_value_is_constant t2
          , lazy_contents_if_value_is_constant t3
          , lazy_contents_if_value_is_constant t4 )
        with
        | Some t1, Some t2, Some t3, Some t4 ->
          Some
            (`Constant
              (lazy (f (Lazy.force t1) (Lazy.force t2) (Lazy.force t3) (Lazy.force t4))))
        | Some t1, Some t2, Some t3, None ->
          Some
            (`Value
              (Value.Map
                 { t = t4
                 ; f = (fun t4 -> f (Lazy.force t1) (Lazy.force t2) (Lazy.force t3) t4)
                 }))
        | Some t1, Some t2, None, Some t4 ->
          Some
            (`Value
              (Value.Map
                 { t = t3
                 ; f = (fun t3 -> f (Lazy.force t1) (Lazy.force t2) t3 (Lazy.force t4))
                 }))
        | Some t1, None, Some t3, Some t4 ->
          Some
            (`Value
              (Value.Map
                 { t = t2
                 ; f = (fun t2 -> f (Lazy.force t1) t2 (Lazy.force t3) (Lazy.force t4))
                 }))
        | None, Some t2, Some t3, Some t4 ->
          Some
            (`Value
              (Value.Map
                 { t = t1
                 ; f = (fun t1 -> f t1 (Lazy.force t2) (Lazy.force t3) (Lazy.force t4))
                 }))
        | Some t1, Some t2, None, None ->
          Some
            (`Value
              (Value.Map2
                 { t1 = t3
                 ; t2 = t4
                 ; f = (fun t3 t4 -> f (Lazy.force t1) (Lazy.force t2) t3 t4)
                 }))
        | Some t1, None, Some t3, None ->
          Some
            (`Value
              (Value.Map2
                 { t1 = t2
                 ; t2 = t4
                 ; f = (fun t2 t4 -> f (Lazy.force t1) t2 (Lazy.force t3) t4)
                 }))
        | None, Some t2, Some t3, None ->
          Some
            (`Value
              (Value.Map2
                 { t1
                 ; t2 = t4
                 ; f = (fun t1 t4 -> f t1 (Lazy.force t2) (Lazy.force t3) t4)
                 }))
        | Some t1, None, None, Some t4 ->
          Some
            (`Value
              (Value.Map2
                 { t1 = t2
                 ; t2 = t3
                 ; f = (fun t2 t3 -> f (Lazy.force t1) t2 t3 (Lazy.force t4))
                 }))
        | None, Some t2, None, Some t4 ->
          Some
            (`Value
              (Value.Map2
                 { t1
                 ; t2 = t3
                 ; f = (fun t1 t3 -> f t1 (Lazy.force t2) t3 (Lazy.force t4))
                 }))
        | None, None, Some t3, Some t4 ->
          Some
            (`Value
              (Value.Map2
                 { t1; t2; f = (fun t1 t2 -> f t1 t2 (Lazy.force t3) (Lazy.force t4)) }))
        | Some t1, None, None, None ->
          Some
            (`Value
              (Value.Map3
                 { t1 = t2
                 ; t2 = t3
                 ; t3 = t4
                 ; f = (fun t2 t3 t4 -> f (Lazy.force t1) t2 t3 t4)
                 }))
        | None, Some t2, None, None ->
          Some
            (`Value
              (Value.Map3
                 { t1
                 ; t2 = t3
                 ; t3 = t4
                 ; f = (fun t1 t3 t4 -> f t1 (Lazy.force t2) t3 t4)
                 }))
        | None, None, Some t3, None ->
          Some
            (`Value
              (Value.Map3
                 { t1; t2; t3 = t4; f = (fun t1 t2 t4 -> f t1 t2 (Lazy.force t3) t4) }))
        | None, None, None, Some t4 ->
          Some
            (`Value
              (Value.Map3 { t1; t2; t3; f = (fun t1 t2 t3 -> f t1 t2 t3 (Lazy.force t4)) }))
        | None, None, None, None -> None)
    | Map5 { t1; t2; t3; t4; t5; f } ->
      constant_or_value value_with_position ~f:(fun () ->
        match
          ( lazy_contents_if_value_is_constant t1
          , lazy_contents_if_value_is_constant t2
          , lazy_contents_if_value_is_constant t3
          , lazy_contents_if_value_is_constant t4
          , lazy_contents_if_value_is_constant t5 )
        with
        | Some t1, Some t2, Some t3, Some t4, Some t5 ->
          Some
            (`Constant
              (lazy
                (f
                   (Lazy.force t1)
                   (Lazy.force t2)
                   (Lazy.force t3)
                   (Lazy.force t4)
                   (Lazy.force t5))))
        | _ -> None)
    | Map6 { t1; t2; t3; t4; t5; t6; f } ->
      constant_or_value value_with_position ~f:(fun () ->
        match
          ( lazy_contents_if_value_is_constant t1
          , lazy_contents_if_value_is_constant t2
          , lazy_contents_if_value_is_constant t3
          , lazy_contents_if_value_is_constant t4
          , lazy_contents_if_value_is_constant t5
          , lazy_contents_if_value_is_constant t6 )
        with
        | Some t1, Some t2, Some t3, Some t4, Some t5, Some t6 ->
          Some
            (`Constant
              (lazy
                (f
                   (Lazy.force t1)
                   (Lazy.force t2)
                   (Lazy.force t3)
                   (Lazy.force t4)
                   (Lazy.force t5)
                   (Lazy.force t6))))
        | _ -> None)
    | Map7 { t1; t2; t3; t4; t5; t6; t7; f } ->
      constant_or_value value_with_position ~f:(fun () ->
        match
          ( lazy_contents_if_value_is_constant t1
          , lazy_contents_if_value_is_constant t2
          , lazy_contents_if_value_is_constant t3
          , lazy_contents_if_value_is_constant t4
          , lazy_contents_if_value_is_constant t5
          , lazy_contents_if_value_is_constant t6
          , lazy_contents_if_value_is_constant t7 )
        with
        | Some t1, Some t2, Some t3, Some t4, Some t5, Some t6, Some t7 ->
          Some
            (`Constant
              (lazy
                (f
                   (Lazy.force t1)
                   (Lazy.force t2)
                   (Lazy.force t3)
                   (Lazy.force t4)
                   (Lazy.force t5)
                   (Lazy.force t6)
                   (Lazy.force t7))))
        | _ -> None)
  ;;

  open Trampoline.Let_syntax

  let transform_c (type a) { constants_in_scope; evaluated } (t : a Computation.t)
    : a Computation.t Trampoline.t
    =
    match t with
    | Assoc ({ map; key_id; data_id; by; key_comparator; here; _ } as assoc_t) ->
      let (), (), map_v =
        Recurse.on_value { constants_in_scope; evaluated } () `Directly_on map
      in
      (match (force_but_capture_exceptions map_v).value with
       | Exception exn -> return (Proc.read (Value.return_exn exn))
       | Constant (lazy map) when Map.length map <= assoc_max_input_size_to_fold ->
         let folded =
           Map.mapi map ~f:(fun ~key ~data ->
             (* In this case, the map is constant, so we have access to the key/data pair
                directly. We use the [Sub]s below with the correct [key_id]/[data_id] so
                that [by] will refer to these constants and then we can recursively rely
                on the constant-folding optimizations to clean up these [Sub]s for us.

                We only invert lifecycles for explicit calls to
                [Bonsai.with_inverted_lifecycle_ordering], so it's [false] here. *)
             let data_binding =
               Computation.Sub
                 { here
                 ; from = Proc.const data
                 ; via = data_id
                 ; into = by
                 ; invert_lifecycles = false
                 }
             in
             Computation.Sub
               { here
               ; from = Proc.const key
               ; via = key_id
               ; into = data_binding
               ; invert_lifecycles = false
               })
           |> Proc.Computation.all_map ~here
         in
         let%bind (), (), r =
           Recurse.on_computation { constants_in_scope; evaluated } () `Directly_on folded
         in
         return r
       | _ ->
         let%bind (), (), by =
           Recurse.on_computation
             { constants_in_scope; evaluated = Maybe }
             ()
             `Directly_on
             by
         in
         (match
            simplify_assoc_if_simpl ~here ~key_comparator ~key_id ~data_id map_v by
          with
          | Some kind -> return kind
          | None -> return (Computation.Assoc { assoc_t with map = map_v; by })))
    | Assoc_on
        ({ map; io_comparator = key_comparator; io_key_id = key_id; data_id; by; here; _ }
         as assoc_on_t) ->
      let (), (), map =
        Recurse.on_value { constants_in_scope; evaluated } () `Directly_on map
      in
      let%bind (), (), by =
        Recurse.on_computation
          { constants_in_scope; evaluated = Maybe }
          ()
          `Directly_on
          by
      in
      (match simplify_assoc_if_simpl ~here ~key_comparator ~key_id ~data_id map by with
       | Some kind -> return kind
       | None -> return (Computation.Assoc_on { assoc_on_t with map; by }))
    | Switch { match_; arms; here } ->
      let (), (), match_ =
        Recurse.on_value { constants_in_scope; evaluated } () `Directly_on match_
      in
      (match (force_but_capture_exceptions match_).value with
       | Exception exn -> return (Proc.read (Value.return_exn exn))
       | Constant (lazy i) ->
         (match Map.find arms i with
          | Some c ->
            let%bind (), (), r =
              Recurse.on_computation { constants_in_scope; evaluated } () `Directly_on c
            in
            return r
          | None ->
            [%sexp
              "switch with value", ((i : int), "does not have a corresponding computation")]
            |> Error.create_s
            |> Error.to_exn
            |> Value.return_exn
            |> Proc.read
            |> return)
       | _ ->
         let%bind arms =
           Map.map arms ~f:(fun c ->
             let%bind (), (), r =
               Recurse.on_computation
                 { constants_in_scope; evaluated = Maybe }
                 ()
                 `Directly_on
                 c
             in
             return r)
           |> Trampoline.all_map
         in
         return (Computation.Switch { match_; arms; here }))
    | Sub { from; via; into; invert_lifecycles; here } ->
      let%bind (), (), from =
        Recurse.on_computation { constants_in_scope; evaluated } () `Directly_on from
      in
      (match from with
       | Return { value = with_id; here = _ } when value_is_constant with_id ->
         let new_constants_in_scope =
           Constants_in_scope.add_exn ~key:via ~data:with_id constants_in_scope
         in
         let%bind (), (), c =
           Recurse.on_computation
             { constants_in_scope = new_constants_in_scope; evaluated }
             ()
             `Directly_on
             into
         in
         return c
       | _ ->
         let%bind (), (), into =
           Recurse.on_computation { constants_in_scope; evaluated } () `Directly_on into
         in
         return (Computation.Sub { from; via; into; invert_lifecycles; here }))
    | Leaf1 { input; input_id; model; dynamic_action; apply_action; reset; here } ->
      let (), (), input =
        Recurse.on_value { constants_in_scope; evaluated } () `Directly_on input
      in
      computation_exception_folder ~here ~f:(fun () ->
        match lazy_contents_if_value_is_constant input with
        | None ->
          return
            (Computation.Leaf1
               { input; input_id; model; dynamic_action; apply_action; reset; here })
        | Some input ->
          let apply_action ~inject = apply_action ~inject (Some (Lazy.force input)) in
          return
            (Computation.Leaf0
               { model; static_action = dynamic_action; apply_action; reset; here }))
    | Lazy { t; here } ->
      (match evaluated with
       | Unconditionally ->
         let%bind (), (), c =
           Recurse.on_computation
             { constants_in_scope; evaluated = Unconditionally }
             ()
             `Directly_on
             (Lazy.force t)
         in
         return c
       | Maybe ->
         Lazy.map t ~f:(fun t ->
           (* Because this recursion is inside of a Lazy.map, it'll only proceed be run if
              the lazy is forced, at which point we _are_ unconditionally running it. *)
           (* NOTE: Constant folding on lazys is deferred until lazys are forced. One
              important consideration is that multiple deferred optimizations should occur
              as if the lazy's were not there to begin with. One possible bug here is that
              nested optimizations could be reversed e.g. (constant_fold (lazy (comp)))
              ==> (lazy (constant_fold (comp))), but because we use Lazy.map the same
              order is preserved. There is not a use case for this right now, but if we
              want more interesting interacctions between deferred optimizations, we could
              mint a type for deferred optimizations which we can introspect.
           *)
           (Timer.timer ()).time `Preprocess ~f:(fun () ->
             Trampoline.run
               (let%bind (), (), t =
                  Recurse.on_computation
                    { constants_in_scope; evaluated = Unconditionally }
                    ()
                    `Directly_on
                    t
                in
                return t)))
         |> fun t -> Computation.Lazy { t; here } |> return)
    | Fix_define _
    | Fix_recurse _
    | Return _
    | Leaf0 _
    | Leaf_incr _
    | Store _
    | Fetch _
    | Wrap _
    | With_model_resetter _
    | Path _
    | Assoc_simpl _
    | Computation_watcher _
    | Lifecycle _ ->
      let%bind (), (), c =
        Recurse.on_computation { constants_in_scope; evaluated } () `Skipping_over t
      in
      return c
  ;;

  let transform_v constants_in_scope () v = (), (), transform_v constants_in_scope v

  let transform_c constants_in_scope () c =
    let%bind r = transform_c constants_in_scope c in
    return ((), (), r)
  ;;
end

open Fix_transform.Make (Types) (Constant_fold)

let constant_fold c =
  let (), (), r =
    Trampoline.run
      (transform_c
         { constants_in_scope = Constants_in_scope.empty; evaluated = Unconditionally }
         ()
         c)
  in
  r
;;
