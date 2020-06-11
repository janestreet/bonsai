open! Core_kernel
open! Import
module Var = Var

let sub
      (type via)
      (Computation.T { t = from; action = from_action; model = from_model } :
         via Computation.packed)
      ~f
  =
  let via : via Type_equal.Id.t =
    Type_equal.Id.create
      ~name:(Source_code_position.to_string [%here])
      [%sexp_of: opaque]
  in
  let (Computation.T { t = into; action = into_action; model = into_model }) =
    f (Value.named via)
  in
  Computation.T
    { t = Subst { from; via; into }
    ; action = Meta.Action.both from_action into_action
    ; model = Meta.Model.both from_model into_model
    }
;;

let read x =
  Computation.T { t = Return x; model = Meta.Model.unit; action = Meta.Action.nothing }
;;

let pure f i = read (Value.map i ~f)
let const x = read (Value.return x)

let of_module1 (type i m a r) (component : (i, m, a, r) component_s) ~default_model input
  =
  let (module M) = component in
  Computation.T
    { t =
        Leaf { input; apply_action = M.apply_action; compute = M.compute; name = M.name }
    ; model = Meta.Model.of_module (module M.Model) ~name:M.name ~default:default_model
    ; action = Meta.Action.of_module (module M.Action) ~name:M.name
    }
;;

let of_module0 c ~default_model = of_module1 c ~default_model (Value.return ())
let of_module2 c ~default_model i1 i2 = of_module1 c ~default_model (Value.both i1 i2)

let assoc
      (type k v cmp)
      (comparator : (k, cmp) comparator)
      (map : (k, v, cmp) Map.t Value.t)
      ~f
  =
  let key_id : k Type_equal.Id.t =
    Type_equal.Id.create ~name:"key id" [%sexp_of: opaque]
  in
  let data_id : v Type_equal.Id.t =
    Type_equal.Id.create ~name:"data id" [%sexp_of: opaque]
  in
  let key_var = Value.named key_id in
  let data_var = Value.named data_id in
  let (Computation.T { t = by; action; model }) = f key_var data_var in
  Computation.T
    { t =
        Assoc
          { map
          ; key_id
          ; data_id
          ; by
          ; model_info = model
          ; input_by_k = T
          ; result_by_k = T
          ; model_by_k = T
          }
    ; action = Meta.Action.map comparator action
    ; model = Meta.Model.map comparator model
    }
;;

let enum (type k) (module E : Enum with type t = k) ~match_ ~with_ =
  let open Enum_types in
  let module E = struct
    include E
    include Comparator.Make (E)
  end
  in
  let create_case key =
    let component = with_ key in
    let (Computation.T { model; t = _; action = _ }) = component in
    let default_model = Case_model.create model model.default in
    component, default_model
  in
  let components, models =
    List.fold
      E.all
      ~init:(Map.empty (module E), Map.empty (module E))
      ~f:(fun (components, models) key ->
        let component, model = create_case key in
        let components = Map.add_exn components ~key ~data:component in
        let models = Map.add_exn models ~key ~data:model in
        components, models)
  in
  Computation.T
    { t =
        Enum
          { which = match_
          ; out_of = components
          ; sexp_of_key = [%sexp_of: E.t]
          ; key_equal = [%equal: E.t]
          ; key_and_cmp = T
          }
    ; model = Multi_model.model_info (module E) models
    ; action = Case_action.type_id [%sexp_of: E.t]
    }
;;

let if_ cond ~then_ ~else_ =
  enum
    (module Bool)
    ~match_:cond
    ~with_:(function
      | true -> then_
      | false -> else_)
;;

let state_machine1
      (type m a)
      here
      (module M : Model with type t = m)
      (module A : Action with type t = a)
      ~default_model
      ~apply_action
      input
  =
  let compute ~inject _input model = model, inject in
  let name = Source_code_position.to_string here in
  Computation.T
    { t = Leaf { input; apply_action; compute; name }
    ; model = Meta.Model.of_module (module M) ~name ~default:default_model
    ; action = Meta.Action.of_module (module A) ~name
    }
;;

let state_machine0 here model action ~default_model ~apply_action =
  let apply_action ~inject ~schedule_event () model action =
    apply_action ~inject ~schedule_event model action
  in
  state_machine1 here model action ~default_model ~apply_action (Value.return ())
;;

module Computation = struct
  type 'a t = 'a Computation.packed
end

module Value = Value

module Let_syntax = struct
  let return = read
  let ( <*> ) = Value.( <*> )
  let ( <$> ) f = Value.map ~f
  let ( >>| ) a f = Value.map a ~f

  module Let_syntax = struct
    let sub = sub
    let return = Value.return
    let map = Value.map
    let both = Value.both
  end
end

module Private = struct
  let conceal_value = Fn.id
  let reveal_value = Fn.id
  let conceal_computation = Fn.id
  let reveal_computation = Fn.id
end
