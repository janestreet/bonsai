open! Core_kernel
open! Import

let rec simplify_value
  : type key data result.
    result Value.t
    -> key Type_equal.Id.t
    -> data Type_equal.Id.t
    -> (key -> data -> result) option
  =
  fun value key_id data_id ->
  match value with
  | Constant (r, _) -> Some (fun _key _data -> r)
  | Incr _ -> None
  | Named name ->
    let same_name = Type_equal.Id.same_witness in
    (match same_name name key_id, same_name name data_id with
     | Some T, _ -> Some (fun key _data -> key)
     | _, Some T -> Some (fun _key data -> data)
     | None, None -> None)
  | Cutoff _ -> None
  | Both (a, b) ->
    let%map.Option a = simplify_value a key_id data_id
    and b = simplify_value b key_id data_id in
    fun key data -> a key data, b key data
  | Map { t; f } ->
    let%map.Option a = simplify_value t key_id data_id in
    fun key data -> f (a key data)
  | Map2 { t1; t2; f } ->
    let%map.Option t1 = simplify_value t1 key_id data_id
    and t2 = simplify_value t2 key_id data_id in
    fun key data -> f (t1 key data) (t2 key data)
  | Map3 { t1; t2; t3; f } ->
    let%map.Option t1 = simplify_value t1 key_id data_id
    and t2 = simplify_value t2 key_id data_id
    and t3 = simplify_value t3 key_id data_id in
    fun key data -> f (t1 key data) (t2 key data) (t3 key data)
  | Map4 { t1; t2; t3; t4; f } ->
    let%map.Option t1 = simplify_value t1 key_id data_id
    and t2 = simplify_value t2 key_id data_id
    and t3 = simplify_value t3 key_id data_id
    and t4 = simplify_value t4 key_id data_id in
    fun key data -> f (t1 key data) (t2 key data) (t3 key data) (t4 key data)
  | Map5 { t1; t2; t3; t4; t5; f } ->
    let%map.Option t1 = simplify_value t1 key_id data_id
    and t2 = simplify_value t2 key_id data_id
    and t3 = simplify_value t3 key_id data_id
    and t4 = simplify_value t4 key_id data_id
    and t5 = simplify_value t5 key_id data_id in
    fun key data ->
      f (t1 key data) (t2 key data) (t3 key data) (t4 key data) (t5 key data)
  | Map6 { t1; t2; t3; t4; t5; t6; f } ->
    let%map.Option t1 = simplify_value t1 key_id data_id
    and t2 = simplify_value t2 key_id data_id
    and t3 = simplify_value t3 key_id data_id
    and t4 = simplify_value t4 key_id data_id
    and t5 = simplify_value t5 key_id data_id
    and t6 = simplify_value t6 key_id data_id in
    fun key data ->
      f
        (t1 key data)
        (t2 key data)
        (t3 key data)
        (t4 key data)
        (t5 key data)
        (t6 key data)
  | Map7 { t1; t2; t3; t4; t5; t6; t7; f } ->
    let%map.Option t1 = simplify_value t1 key_id data_id
    and t2 = simplify_value t2 key_id data_id
    and t3 = simplify_value t3 key_id data_id
    and t4 = simplify_value t4 key_id data_id
    and t5 = simplify_value t5 key_id data_id
    and t6 = simplify_value t6 key_id data_id
    and t7 = simplify_value t7 key_id data_id in
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

let function_of_'return'_computation
  : type key data model action result.
    (model, action, result) Computation.t
    -> key_id:key Type_equal.Id.t
    -> data_id:data Type_equal.Id.t
    -> (key -> data -> result) option
  =
  fun computation ~key_id ~data_id ->
  match computation with
  | Return value -> simplify_value value key_id data_id
  | _ -> None
;;
