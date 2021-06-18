open! Core
open! Import

module Action = struct
  type 'key t =
    | T :
        { action : 'a
        ; type_id : 'a Type_equal.Id.t
        ; key : 'key
        }
        -> 'key t

  let sexp_of_t sexp_of_key (T { action; type_id; key }) =
    let sexp_of_action = Type_equal.Id.to_sexp type_id in
    [%message "enum action with key" (action : action) (key : key)]
  ;;

  let type_id sexp_of_key =
    Type_equal.Id.create ~name:"enum action with key" [%sexp_of: key t]
  ;;
end

module Model = struct
  type t =
    | T :
        { model : 'm
        ; info : 'm Meta.Model.t
        ; t_of_sexp : Sexp.t -> t
        }
        -> t

  let sexp_of_t (T { model; info = { sexp_of; _ }; _ }) = sexp_of model

  let equal
        (T { model = m1; info = { type_id = t1; equal; _ }; _ })
        (T { model = m2; info = { type_id = t2; _ }; _ })
    =
    match Type_equal.Id.same_witness t1 t2 with
    | Some T -> equal m1 m2
    | None -> false
  ;;

  let create (info : _ Meta.Model.t) =
    let rec t_of_sexp sexp = wrap (info.of_sexp sexp)
    and wrap m = T { model = m; info; t_of_sexp } in
    wrap
  ;;
end

module Multi_model = struct
  type ('k, 'cmp) t = ('k, Model.t, 'cmp) Map.t

  let sexp_of_t (type k) (sexp_of_k : k -> Sexp.t) =
    let module K = struct
      type t = k [@@deriving sexp_of]
    end
    in
    [%sexp_of: Model.t Map.M(K).t]
  ;;

  let t_of_sexp
        (type k cmp)
        ((module K) : (k, cmp) comparator)
        (default_models : Model.t Map.M(K).t)
        sexp
    =
    let k_to_sexp_map = [%of_sexp: Sexp.t Map.M(K).t] sexp in
    Map.merge k_to_sexp_map default_models ~f:(fun ~key:_ -> function
      | `Both (sexp, Model.T { t_of_sexp; _ }) -> Some (t_of_sexp sexp)
      | `Left _sexp -> None
      | `Right default_model -> Some default_model)
  ;;

  let find_exn = Map.find_exn
  let set = Map.set

  let model_info (type k cmp) ((module K) : (k, cmp) comparator) default =
    let sexp_of = [%sexp_of: K.t t] in
    let of_sexp = t_of_sexp (module K) default in
    ({ default
     ; type_id = Type_equal.Id.create ~name:"poly-model" sexp_of
     ; equal = [%equal: Model.t Map.M(K).t]
     ; sexp_of
     ; of_sexp
     }
     : (k, cmp) t Meta.Model.t)
  ;;
end
