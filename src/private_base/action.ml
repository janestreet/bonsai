open Core

type 'a leaf = private Leaf
type ('from, 'into) sub = private Sub
type 'a model_resetter = private Model_resetter
type ('inner, 'outer) wrap = private Wrap
type 'inner wrap_static = private Wrap_static
type ('inner, 'outer) wrap_dynamic = private Wrap_dynamic
type switch = private Switch
type lazy_ = private Lazy
type ('key, 'inner) assoc = private Assoc
type ('io_key, 'model_key, 'inner) assoc_on = private Assoc_on

type 'a id =
  | Leaf_id : { action : 'action Type_equal.Id.t } -> 'action leaf id
  | Sub_id :
      { from : 'from id
      ; into : 'into id
      }
      -> ('from, 'into) sub id
  | Wrap_id :
      { inner : 'inner id
      ; outer : 'outer Type_equal.Id.t
      }
      -> ('inner, 'outer) wrap id
  | Model_reset_id : { inner : 'inner id } -> 'inner model_resetter id
  | Switch_id : switch id
  | Lazy_id : lazy_ id
  | Assoc_id :
      { key : 'key Type_equal.Id.t
      ; action : 'a id
      }
      -> ('key, 'a) assoc id
  | Assoc_on_id :
      { io_key : 'io_key Type_equal.Id.t
      ; model_key : 'model_key Type_equal.Id.t
      ; action : 'a id
      }
      -> ('io_key, 'model_key, 'a) assoc_on id

(* A [('dynamic, 'static, 'a) t] represents a Bonsai action. ['dynamic] describes the type
   of the dynamic actions that could be delivered to this computation. ['static] describes
   the type of the static actions that could be delivered to this computation. ['a]
   describes the overall structure of the actions. *)
and 'a t =
  | Leaf_static : 'static -> 'static leaf t
  | Leaf_dynamic : 'dynamic -> 'dynamic leaf t
  | Sub_from : 'a t -> ('a, _) sub t
  | Sub_into : 'a t -> (_, 'a) sub t
  | Wrap_inner : 'a t -> ('a, _) wrap t
  | Wrap_outer : 'outer -> (_, 'outer) wrap t
  | Model_reset_inner : 'a t -> 'a model_resetter t
  | Model_reset_outer : _ model_resetter t
  | Switch :
      { branch : int
      ; action : 'a t
      ; type_id : 'a id
      }
      -> switch t
  | Lazy :
      { action : 'a t
      ; type_id : 'a id
      }
      -> lazy_ t
  | Assoc :
      { key : 'key
      ; action : 'a t
      ; id : 'key Type_equal.Id.t
      ; compare : 'key -> 'key -> int
      }
      -> ('key, 'a) assoc t
  | Assoc_on :
      { io_key : 'io_key
      ; model_key : 'model_key
      ; action : 'a t
      ; io_id : 'io_key Type_equal.Id.t
      ; io_compare : 'io_key -> 'io_key -> int
      }
      -> ('io_key, 'model_key, 'a) assoc_on t

let rec same_witness : type a b. a id -> b id -> (a, b) Type_equal.t option =
  fun a b ->
  match a, b with
  | Leaf_id { action }, Leaf_id { action = action' } ->
    (match Type_equal.Id.same_witness action action' with
     | Some T -> Some T
     | None -> None)
  | Sub_id { from; into }, Sub_id { from = from'; into = into' } ->
    let same_from = same_witness from from' in
    let same_into = same_witness into into' in
    (match same_from, same_into with
     | Some T, Some T -> Some T
     | _ -> None)
  | Wrap_id { inner; outer }, Wrap_id { inner = inner'; outer = outer' } ->
    let same_inner = same_witness inner inner' in
    let same_outer = Type_equal.Id.same_witness outer outer' in
    (match same_inner, same_outer with
     | Some T, Some T -> Some T
     | _ -> None)
  | Model_reset_id { inner }, Model_reset_id { inner = inner' } ->
    (match same_witness inner inner' with
     | Some T -> Some T
     | None -> None)
  | Switch_id, Switch_id -> Some T
  | Lazy_id, Lazy_id -> Some T
  | Assoc_id { key; action }, Assoc_id { key = key'; action = action' } ->
    let same_key = Type_equal.Id.same_witness key key' in
    let same_action = same_witness action action' in
    (match same_key, same_action with
     | Some T, Some T -> Some T
     | _ -> None)
  | ( Assoc_on_id { io_key; model_key; action }
    , Assoc_on_id { io_key = io_key'; model_key = model_key'; action = action' } ) ->
    let same_io_key = Type_equal.Id.same_witness io_key io_key' in
    let same_model_key = Type_equal.Id.same_witness model_key model_key' in
    let same_action = same_witness action action' in
    (match same_io_key, same_model_key, same_action with
     | Some T, Some T, Some T -> Some T
     | _ -> None)
  | Leaf_id _, Sub_id _
  | Leaf_id _, Wrap_id _
  | Leaf_id _, Model_reset_id _
  | Leaf_id _, Switch_id
  | Leaf_id _, Lazy_id
  | Leaf_id _, Assoc_id _
  | Leaf_id _, Assoc_on_id _
  | Sub_id _, Leaf_id _
  | Sub_id _, Wrap_id _
  | Sub_id _, Model_reset_id _
  | Sub_id _, Switch_id
  | Sub_id _, Lazy_id
  | Sub_id _, Assoc_id _
  | Sub_id _, Assoc_on_id _
  | Wrap_id _, Leaf_id _
  | Wrap_id _, Sub_id _
  | Wrap_id _, Model_reset_id _
  | Wrap_id _, Switch_id
  | Wrap_id _, Lazy_id
  | Wrap_id _, Assoc_id _
  | Wrap_id _, Assoc_on_id _
  | Model_reset_id _, Leaf_id _
  | Model_reset_id _, Sub_id _
  | Model_reset_id _, Wrap_id _
  | Model_reset_id _, Switch_id
  | Model_reset_id _, Lazy_id
  | Model_reset_id _, Assoc_id _
  | Model_reset_id _, Assoc_on_id _
  | Switch_id, Leaf_id _
  | Switch_id, Sub_id _
  | Switch_id, Wrap_id _
  | Switch_id, Model_reset_id _
  | Switch_id, Lazy_id
  | Switch_id, Assoc_id _
  | Switch_id, Assoc_on_id _
  | Lazy_id, Leaf_id _
  | Lazy_id, Sub_id _
  | Lazy_id, Wrap_id _
  | Lazy_id, Model_reset_id _
  | Lazy_id, Switch_id
  | Lazy_id, Assoc_id _
  | Lazy_id, Assoc_on_id _
  | Assoc_id _, Leaf_id _
  | Assoc_id _, Sub_id _
  | Assoc_id _, Wrap_id _
  | Assoc_id _, Model_reset_id _
  | Assoc_id _, Switch_id
  | Assoc_id _, Lazy_id
  | Assoc_id _, Assoc_on_id _
  | Assoc_on_id _, Leaf_id _
  | Assoc_on_id _, Sub_id _
  | Assoc_on_id _, Wrap_id _
  | Assoc_on_id _, Model_reset_id _
  | Assoc_on_id _, Switch_id
  | Assoc_on_id _, Lazy_id
  | Assoc_on_id _, Assoc_id _ -> None
;;

let rec sexp_of_t : type a. a id -> Sexp.t = function
  | Leaf_id { action } -> [%sexp Leaf, (action : opaque Type_equal.Id.t)]
  | Sub_id { from; into } ->
    let from = sexp_of_t from in
    let into = sexp_of_t into in
    [%sexp Sub, (from : Sexp.t), (into : Sexp.t)]
  | Wrap_id { inner; outer } ->
    let inner = sexp_of_t inner in
    [%sexp Wrap, (inner : Sexp.t), (outer : opaque Type_equal.Id.t)]
  | Model_reset_id { inner } ->
    let inner = sexp_of_t inner in
    [%sexp Model_reset (inner : Sexp.t)]
  | Switch_id -> [%sexp Switch]
  | Lazy_id -> [%sexp Lazy]
  | Assoc_id { key; action } ->
    let action = sexp_of_t action in
    [%sexp Assoc, (key : opaque Type_equal.Id.t), (action : Sexp.t)]
  | Assoc_on_id { io_key; model_key; action } ->
    let action = sexp_of_t action in
    [%sexp
      Assoc
      , (io_key : opaque Type_equal.Id.t)
      , (model_key : opaque Type_equal.Id.t)
      , (action : Sexp.t)]
;;

let rec to_sexp : type a. a id -> a t -> Sexp.t = function
  | Leaf_id { action } ->
    let to_sexp = Type_equal.Id.to_sexp action in
    (function
      | Leaf_dynamic action -> [%sexp Leaf_dynamic, (to_sexp action : Sexp.t)]
      | Leaf_static action -> [%sexp Leaf_static, (to_sexp action : Sexp.t)])
  | Sub_id { from; into } ->
    (function
      | Sub_from action ->
        let to_sexp = to_sexp from in
        [%sexp Sub_from, (to_sexp action : Sexp.t)]
      | Sub_into action ->
        let to_sexp = to_sexp into in
        [%sexp Sub_into, (to_sexp action : Sexp.t)])
  | Wrap_id { inner; outer } ->
    (function
      | Wrap_inner action ->
        let to_sexp = to_sexp inner in
        [%sexp Wrap_inner, (to_sexp action : Sexp.t)]
      | Wrap_outer action ->
        let to_sexp = Type_equal.Id.to_sexp outer in
        [%sexp Wrap_outer, (to_sexp action : Sexp.t)])
  | Model_reset_id { inner } ->
    (function
      | Model_reset_outer -> [%sexp Model_reset_outer]
      | Model_reset_inner action ->
        let to_sexp = to_sexp inner in
        [%sexp Model_reset_inner, (to_sexp action : Sexp.t)])
  | Switch_id ->
    fun (Switch { branch; action; type_id }) ->
      let to_sexp = to_sexp type_id in
      [%sexp Switch, (branch : int), (to_sexp action : Sexp.t)]
  | Lazy_id ->
    fun (Lazy { action; type_id }) ->
      let to_sexp = to_sexp type_id in
      [%sexp Lazy, (to_sexp action : Sexp.t)]
  | Assoc_id { key = key_id; action = action_id } ->
    fun (Assoc { key; action; id = _; compare = _ }) ->
      let sexp_of_key = Type_equal.Id.to_sexp key_id in
      let sexp_of_action = to_sexp action_id in
      [%sexp Assoc, (key : key), (action : action)]
  | Assoc_on_id { io_key = io_key_id; model_key = model_key_id; action = action_id } ->
    fun (Assoc_on { io_key; model_key; action; io_id = _; io_compare = _ }) ->
      let sexp_of_io_key = Type_equal.Id.to_sexp io_key_id in
      let sexp_of_model_key = Type_equal.Id.to_sexp model_key_id in
      let sexp_of_action = to_sexp action_id in
      [%sexp Assoc_on, (io_key : io_key), (model_key : model_key), (action : action)]
;;

module Type_id = struct
  let nothing_type_id = Type_equal.Id.create ~name:"Nothing.t" [%sexp_of: Nothing.t]
  let nothing = Leaf_id { action = nothing_type_id }
  let leaf type_id = Leaf_id { action = type_id }
  let sub ~from ~into = Sub_id { from; into }
  let wrap ~inner ~outer = Wrap_id { inner; outer }
  let model_reset inner = Model_reset_id { inner }
  let lazy_ = Lazy_id
  let switch = Switch_id
  let assoc ~key ~action = Assoc_id { key; action }
  let assoc_on ~io_key ~model_key ~action = Assoc_on_id { io_key; model_key; action }
  let same_witness = same_witness
  let same_witness_exn a b = Option.value_exn (same_witness a b)
  let to_sexp = to_sexp
  let sexp_of_t = sexp_of_t
end

let static_leaf action = Leaf_static action
let dynamic_leaf action = Leaf_dynamic action
let sub_from action = Sub_from action
let sub_into action = Sub_into action
let wrap_inner action = Wrap_inner action
let wrap_outer action = Wrap_outer action
let model_reset_inner action = Model_reset_inner action
let model_reset_outer = Model_reset_outer
let switch ~branch ~type_id action = Switch { branch; action; type_id }
let lazy_ ~type_id action = Lazy { action; type_id }
let assoc ~key ~id ~compare action = Assoc { key; action; id; compare }

let assoc_on ~io_key ~io_id ~io_compare ~model_key action =
  Assoc_on { io_key; model_key; action; io_id; io_compare }
;;
