open! Core
open! Import
module Free_variables = Type_id_set

(* When executing the generated function, this Env is used to pass down any variables that
   were bound in a let%sub. *)
module Env =
  Univ_map.Make
    (Univ_map.Type_id_key)
    (struct
      type 'a t = 'a [@@deriving sexp_of]
    end)

(* This is a tri-state option, where the third state is a function that permits the
   generation of the contained value, provided that all of the free variables that were
   found, are bound inside the environment. *)
module Option_or_miss = struct
  type 'a t =
    | None
    | Some of
        { value : 'a Lazy.t
        ; may_contain : May_contain.Resolved.t
        }
    | Miss of
        { free : Free_variables.t
        ; gen : Env.t -> 'a
        ; may_contain : May_contain.Resolved.t
        }

  (* compresses a [Miss] when the set of free variables is empty. *)
  let squash = function
    | None -> None
    | Some a -> Some a
    | Miss { free; gen; may_contain } when Free_variables.is_empty free ->
      Some { value = Lazy.return (gen Env.empty); may_contain }
    | other -> other
  ;;

  let map a ~f =
    match a with
    | None -> None
    | Some { may_contain; value } -> Some { may_contain; value = Lazy.map value ~f }
    | Miss { free; gen; may_contain } ->
      Miss { free; gen = (fun m -> f (gen m)); may_contain }
  ;;

  let both a b =
    match a, b with
    | None, _ | _, None -> None
    | Some { may_contain = ma; value = a }, Some { may_contain = mb; value = b } ->
      Some
        { value = lazy (Lazy.force a, Lazy.force b)
        ; may_contain = May_contain.Resolved.merge ma mb
        }
    | Some { value = a; may_contain = ma }, Miss { free; gen; may_contain = mb } ->
      Miss
        { free
        ; gen = (fun m -> Lazy.force a, gen m)
        ; may_contain = May_contain.Resolved.merge ma mb
        }
    | Miss { free; gen; may_contain = ma }, Some { value = b; may_contain = mb } ->
      Miss
        { free
        ; gen = (fun m -> gen m, Lazy.force b)
        ; may_contain = May_contain.Resolved.merge ma mb
        }
    | ( Miss { free = free_a; gen = gen_a; may_contain = ma }
      , Miss { free = free_b; gen = gen_b; may_contain = mb } ) ->
      let free = Free_variables.union free_a free_b in
      Miss
        { free
        ; gen = (fun env -> gen_a env, gen_b env)
        ; may_contain = May_contain.Resolved.merge ma mb
        }
  ;;

  module Let_syntax = struct
    let map = map
    let both = both
  end
end

(* value_to_function takes a Value.t and attempts to translate it into a function which
   has access to a "key" and "data" (named so because this function is only used to
   simplify an [assoc], which provides both of those values. *)
let rec value_to_function
  : type key data result.
    result Value.t
    -> key Type_equal.Id.t
    -> data Type_equal.Id.t
    -> (key -> data -> result) Option_or_miss.t
  =
  fun value key_id data_id ->
  let open Option_or_miss in
  match value.value with
  | Constant r ->
    Some
      { value = lazy (fun _key _data -> Lazy.force r)
      ; may_contain = May_contain.Resolved.create ~path:No ~lifecycle:No ~input:No
      }
  | Exception _ -> None
  | Incr _ -> None
  | Named (_, id) ->
    let same_name = Type_equal.Id.same_witness in
    (match same_name id key_id, same_name id data_id with
     | Some T, _ ->
       Some
         { value = Lazy.return (fun key _data -> key)
         ; may_contain = May_contain.Resolved.create ~path:No ~lifecycle:No ~input:No
         }
     | _, Some T ->
       Some
         { value = Lazy.return (fun _key data -> data)
         ; may_contain = May_contain.Resolved.create ~path:No ~lifecycle:No ~input:No
         }
     | None, None ->
       Miss
         { free = Free_variables.(add empty id)
         ; gen = (fun env _ _ -> Env.find_exn env id)
         ; may_contain = May_contain.Resolved.create ~path:No ~lifecycle:No ~input:No
         })
  | Cutoff { t; added_by_let_syntax = true; equal = _ } ->
    value_to_function t key_id data_id
  | Cutoff { t = _; equal = _; added_by_let_syntax = false } -> None
  | Both (a, b) ->
    let%map a = value_to_function a key_id data_id
    and b = value_to_function b key_id data_id in
    fun key data -> a key data, b key data
  | Map { t; f } ->
    let%map a = value_to_function t key_id data_id in
    fun key data -> f (a key data)
  | Map2 { t1; t2; f } ->
    let%map t1 = value_to_function t1 key_id data_id
    and t2 = value_to_function t2 key_id data_id in
    fun key data -> f (t1 key data) (t2 key data)
  | Map3 { t1; t2; t3; f } ->
    let%map t1 = value_to_function t1 key_id data_id
    and t2 = value_to_function t2 key_id data_id
    and t3 = value_to_function t3 key_id data_id in
    fun key data -> f (t1 key data) (t2 key data) (t3 key data)
  | Map4 { t1; t2; t3; t4; f } ->
    let%map t1 = value_to_function t1 key_id data_id
    and t2 = value_to_function t2 key_id data_id
    and t3 = value_to_function t3 key_id data_id
    and t4 = value_to_function t4 key_id data_id in
    fun key data -> f (t1 key data) (t2 key data) (t3 key data) (t4 key data)
  | Map5 { t1; t2; t3; t4; t5; f } ->
    let%map t1 = value_to_function t1 key_id data_id
    and t2 = value_to_function t2 key_id data_id
    and t3 = value_to_function t3 key_id data_id
    and t4 = value_to_function t4 key_id data_id
    and t5 = value_to_function t5 key_id data_id in
    fun key data ->
      f (t1 key data) (t2 key data) (t3 key data) (t4 key data) (t5 key data)
  | Map6 { t1; t2; t3; t4; t5; t6; f } ->
    let%map t1 = value_to_function t1 key_id data_id
    and t2 = value_to_function t2 key_id data_id
    and t3 = value_to_function t3 key_id data_id
    and t4 = value_to_function t4 key_id data_id
    and t5 = value_to_function t5 key_id data_id
    and t6 = value_to_function t6 key_id data_id in
    fun key data ->
      f
        (t1 key data)
        (t2 key data)
        (t3 key data)
        (t4 key data)
        (t5 key data)
        (t6 key data)
  | Map7 { t1; t2; t3; t4; t5; t6; t7; f } ->
    let%map t1 = value_to_function t1 key_id data_id
    and t2 = value_to_function t2 key_id data_id
    and t3 = value_to_function t3 key_id data_id
    and t4 = value_to_function t4 key_id data_id
    and t5 = value_to_function t5 key_id data_id
    and t6 = value_to_function t6 key_id data_id
    and t7 = value_to_function t7 key_id data_id in
    fun key data ->
      f
        (t1 key data)
        (t2 key data)
        (t3 key data)
        (t4 key data)
        (t5 key data)
        (t6 key data)
        (t7 key data)
;;

let rec computation_to_function
  : type key data result.
    result Computation.t
    -> key_id:key Type_equal.Id.t
    -> data_id:data Type_equal.Id.t
    -> (Path.t -> key -> data -> result) Option_or_miss.t
  =
  fun computation ~key_id ~data_id ->
  let recurse computation = computation_to_function ~key_id ~data_id computation in
  let handle_subst (type r1 r2) ~(from : r1 Computation.t) ~via ~(into : r2 Computation.t)
    : (Path.t -> key -> data -> r2) Option_or_miss.t
    =
    match recurse from, recurse into with
    (* If any subcomputation is not simplifiable, then bail *)
    | None, _ | _, None -> None
    (* A rhs that isn't missing any variables can be used by ignoring the computed values
       on the lhs. *)
    | (Some _ | Miss _), Some r -> Some r
    | Some { value = (lazy from); may_contain = ma }, Miss { free; gen; may_contain = mb }
      ->
      let free = Free_variables.remove free via in
      let both_use_path = May_contain.Resolved.both_use_path ma mb in
      let gen env path key data =
        let from_path =
          if both_use_path then Path.(append path Elem.Subst_from) else path
        in
        let into_path =
          if both_use_path then Path.(append path Elem.Subst_into) else path
        in
        let env = Env.add_exn env ~key:via ~data:(from from_path key data) in
        gen env into_path key data
      in
      Option_or_miss.squash
        (Miss { free; gen; may_contain = May_contain.Resolved.merge ma mb })
    | ( Miss { free = free_a; gen = gen_a; may_contain = ma }
      , Miss { free = free_b; gen = gen_b; may_contain = mb } ) ->
      let free_b = Free_variables.remove free_b via in
      let free = Free_variables.union free_a free_b in
      let both_use_path = May_contain.Resolved.both_use_path ma mb in
      let gen env path key data =
        let from_path =
          if both_use_path then Path.(append path Elem.Subst_from) else path
        in
        let into_path =
          if both_use_path then Path.(append path Elem.Subst_into) else path
        in
        let env = Env.add_exn env ~key:via ~data:(gen_a env from_path key data) in
        gen_b env into_path key data
      in
      Option_or_miss.squash
        (Miss { free; gen; may_contain = May_contain.Resolved.merge ma mb })
  in
  match computation with
  | Return { value; here = _ } ->
    Option_or_miss.map (value_to_function value key_id data_id) ~f:(fun f _path -> f)
  | Sub { from; via; into; invert_lifecycles = _; here = _ } ->
    handle_subst ~from ~via ~into
  | Path { here = _ } ->
    Some
      { value = Lazy.return (fun path _ _ -> path)
      ; may_contain =
          May_contain.Resolved.create ~path:Yes_or_maybe ~lifecycle:No ~input:No
      }
  | _ -> None
;;

let computation_to_function t ~key_compare ~key_id ~data_id =
  let make_path_element = Path.Elem.keyed ~compare:key_compare key_id |> unstage in
  match computation_to_function t ~key_id ~data_id |> Option_or_miss.squash with
  | Some
      { value = f
      ; may_contain = { path = contains_path; input = _; lifecycle = _ } as may_contain
      } ->
    Option.some
      ( (fun path key data ->
          let path =
            match contains_path with
            | Yes_or_maybe -> Path.append path (Assoc (make_path_element key))
            | No -> path
          in
          (Lazy.force f) path key data)
      , may_contain )
  | None | Miss _ -> None
;;
