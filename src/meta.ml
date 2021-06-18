open! Core
open! Import

let unit_type_id = Type_equal.Id.create ~name:"()" [%sexp_of: unit]
let nothing_type_id = Type_equal.Id.create ~name:"Nothing.t" [%sexp_of: Nothing.t]

module Model = struct
  type 'a t =
    { default : 'a
    ; equal : 'a -> 'a -> bool
    ; type_id : 'a Type_equal.Id.t
    ; sexp_of : 'a -> Sexp.t
    ; of_sexp : Sexp.t -> 'a
    }

  let unit =
    { type_id = unit_type_id
    ; default = ()
    ; equal = equal_unit
    ; sexp_of = sexp_of_unit
    ; of_sexp = unit_of_sexp
    }
  ;;

  let of_module (type t) (module M : Model with type t = t) ~default ~name =
    let type_id = Type_equal.Id.create ~name:(sprintf "%s-model" name) M.sexp_of_t in
    { type_id; default; equal = M.equal; sexp_of = M.sexp_of_t; of_sexp = M.t_of_sexp }
  ;;

  let both model1 model2 =
    let sexp_of = Tuple2.sexp_of_t model1.sexp_of model2.sexp_of in
    let of_sexp = Tuple2.t_of_sexp model1.of_sexp model2.of_sexp in
    let type_id =
      Type_equal.Id.create
        sexp_of
        ~name:
          (sprintf
             "(%s * %s)"
             (Type_equal.Id.name model1.type_id)
             (Type_equal.Id.name model2.type_id))
    in
    let default = model1.default, model2.default in
    let equal = Tuple2.equal ~eq1:model1.equal ~eq2:model2.equal in
    { type_id; default; equal; sexp_of; of_sexp }
  ;;

  let map
        (type k cmp)
        (module M : Comparator with type t = k and type comparator_witness = cmp)
        model
    =
    let sexp_of_model = model.sexp_of in
    let model_of_sexp = model.of_sexp in
    let sexp_of_map_model = [%sexp_of: model Map.M(M).t] in
    let model_map_type_id =
      Type_equal.Id.create
        ~name:(sprintf "map to %s" (Type_equal.Id.name model.type_id))
        sexp_of_map_model
    in
    { type_id = model_map_type_id
    ; default = Map.empty (module M)
    ; equal = Map.equal model.equal
    ; sexp_of = sexp_of_map_model
    ; of_sexp = [%of_sexp: model Map.M(M).t]
    }
  ;;
end

module Action = struct
  type 'a t = 'a Type_equal.Id.t

  let nothing = nothing_type_id

  let both a b =
    Type_equal.Id.create
      ~name:(sprintf "(%s * %s)" (Type_equal.Id.name a) (Type_equal.Id.name b))
      (Either.sexp_of_t (Type_equal.Id.to_sexp a) (Type_equal.Id.to_sexp b))
  ;;

  let of_module (type t) (module M : Action with type t = t) ~name =
    Type_equal.Id.create ~name:(sprintf "%s-action" name) M.sexp_of_t
  ;;

  let map
        (type k cmp)
        (module M : Comparator with type t = k and type comparator_witness = cmp)
        action
    =
    let sexp_of_action = Type_equal.Id.to_sexp action in
    Type_equal.Id.create
      ~name:(sprintf "map to %s" (Type_equal.Id.name action))
      [%sexp_of: M.t * action]
  ;;
end
