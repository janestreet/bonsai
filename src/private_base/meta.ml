open! Core
open! Import

let unit_type_id = Type_equal.Id.create ~name:"unit" [%sexp_of: unit]
let nothing_type_id = Type_equal.Id.create ~name:"Nothing.t" [%sexp_of: Nothing.t]

module type Type_id = sig
  type 'a t [@@deriving sexp_of]

  val same_witness : 'a t -> 'b t -> ('a, 'b) Type_equal.t option
  val same_witness_exn : 'a t -> 'b t -> ('a, 'b) Type_equal.t
  val to_type_id : 'a t -> 'a Type_equal.Id.t
  val to_sexp : 'a t -> 'a -> Sexp.t
  val nothing : Nothing.t t
  val unit : unit t
end

module Model = struct
  type 'a id =
    | Leaf : { type_id : 'a Type_equal.Id.t } -> 'a id
    | Tuple :
        { a : 'a id
        ; b : 'b id
        }
        -> ('a * 'b) id
    | Optional : 'a id -> 'a option id
    | Map :
        { k : 'k Type_equal.Id.t
        ; cmp : 'cmp Type_equal.Id.t
        ; by : 'result id
        }
        -> ('k, 'result, 'cmp) Map.t id
    | Map_on :
        { k_model : 'k_model Type_equal.Id.t
        ; k_io : 'k_io Type_equal.Id.t
        ; cmp : 'cmp_model Type_equal.Id.t
        ; by : 'result id
        }
        -> ('k_model, 'k_io * 'result, 'cmp_model) Map.t id
    | Multi_model : { multi_model : hidden Int.Map.t } -> hidden Int.Map.t id

  and 'a t =
    { default : 'a
    ; equal : 'a -> 'a -> bool
    ; type_id : 'a id
    ; sexp_of : 'a -> Sexp.t
    }

  and hidden =
    | T :
        { model : 'm
        ; info : 'm t
        }
        -> hidden

  module Type_id = struct
    type 'a t = 'a id

    let rec sexp_of_t : type a. (a -> Sexp.t) -> a t -> Sexp.t =
      fun sexp_of_a -> function
      | Leaf { type_id } -> [%sexp (type_id : a Type_equal.Id.t)]
      | Tuple { a; b } -> [%sexp (a : opaque t), (b : opaque t)]
      | Optional a -> [%sexp `optional (a : opaque t)]
      | Map { by; _ } -> [%sexp (by : opaque t)]
      | Map_on { by; _ } -> [%sexp (by : opaque t)]
      | Multi_model { multi_model } ->
        let sexp_of_hidden (T { info = { type_id; _ }; _ }) =
          [%sexp (type_id : opaque t)]
        in
        [%sexp (multi_model : hidden Int.Map.t)]
    ;;

    let rec to_sexp : type a. a t -> a -> Sexp.t = function
      | Leaf { type_id } -> Type_equal.Id.to_sexp type_id
      | Tuple { a = a_t; b = b_t } ->
        let sexp_of_a = to_sexp a_t in
        let sexp_of_b = to_sexp b_t in
        [%sexp_of: a * b]
      | Optional a_t ->
        let sexp_of_a = sexp_of_option (to_sexp a_t) in
        [%sexp_of: a]
      | Map { k; by; _ } ->
        let result : type k by. k Type_equal.Id.t -> by id -> (k, by, _) Map.t -> Sexp.t =
          fun k by ->
          let module Key = struct
            type t = k

            let sexp_of_t : t -> Sexp.t = Type_equal.Id.to_sexp k
          end
          in
          let sexp_of_by = to_sexp by in
          [%sexp_of: by Map.M(Key).t]
        in
        result k by
      | Map_on { k_model; k_io; by; _ } ->
        let result (type k_model) (k_model : k_model Type_equal.Id.t) k_io by =
          let module Key = struct
            type t = k_model

            let sexp_of_t : t -> Sexp.t = Type_equal.Id.to_sexp k_model
          end
          in
          let sexp_of_by = to_sexp by in
          let sexp_of_k_io = Type_equal.Id.to_sexp k_io in
          [%sexp_of: (k_io * by) Map.M(Key).t]
        in
        result k_model k_io by
      | Multi_model _ ->
        let sexp_of_hidden (T { info = { type_id; _ }; _ }) =
          sexp_of_t sexp_of_opaque type_id
        in
        [%sexp_of: hidden Int.Map.t]
    ;;

    exception Fail

    let type_equal_id_same_witness = Type_equal.Id.same_witness

    let rec same_witness : type a b. a t -> b t -> (a, b) Type_equal.t option =
      fun a b ->
      match a, b with
      | Leaf a, Leaf b -> type_equal_id_same_witness a.type_id b.type_id
      | Tuple a, Tuple b ->
        let%bind.Option T = same_witness a.a b.a in
        let%bind.Option T = same_witness a.b b.b in
        Some (Type_equal.T : (a, b) Type_equal.t)
      | Optional a, Optional b ->
        let%bind.Option T = same_witness a b in
        Some (Type_equal.T : (a, b) Type_equal.t)
      | Map a, Map b ->
        let%bind.Option T = type_equal_id_same_witness a.k b.k in
        let%bind.Option T = type_equal_id_same_witness a.cmp b.cmp in
        let%bind.Option T = same_witness a.by b.by in
        Some (Type_equal.T : (a, b) Type_equal.t)
      | Map_on a, Map_on b ->
        let%bind.Option T = type_equal_id_same_witness a.k_io b.k_io in
        let%bind.Option T = type_equal_id_same_witness a.k_model b.k_model in
        let%bind.Option T = type_equal_id_same_witness a.cmp b.cmp in
        let%bind.Option T = same_witness a.by b.by in
        Some (Type_equal.T : (a, b) Type_equal.t)
      | Multi_model a, Multi_model b ->
        if Map.equal
             (fun (T a) (T b) ->
               match same_witness a.info.type_id b.info.type_id with
               | None -> false
               | Some T -> true)
             a.multi_model
             b.multi_model
        then Some Type_equal.T
        else None
      | Leaf _, Tuple _
      | Leaf _, Optional _
      | Leaf _, Map _
      | Leaf _, Map_on _
      | Leaf _, Multi_model _
      | Tuple _, Leaf _
      | Tuple _, Optional _
      | Tuple _, Map _
      | Tuple _, Map_on _
      | Tuple _, Multi_model _
      | Map _, Leaf _
      | Map _, Tuple _
      | Map _, Optional _
      | Map _, Map_on _
      | Map _, Multi_model _
      | Map_on _, Leaf _
      | Map_on _, Tuple _
      | Map_on _, Optional _
      | Map_on _, Map _
      | Map_on _, Multi_model _
      | Multi_model _, Leaf _
      | Multi_model _, Tuple _
      | Multi_model _, Optional _
      | Multi_model _, Map _
      | Multi_model _, Map_on _
      | Optional _, Leaf _
      | Optional _, Tuple _
      | Optional _, Map _
      | Optional _, Map_on _
      | Optional _, Multi_model _ -> None
    ;;

    let same_witness_exn a b =
      match same_witness a b with
      | None -> raise_notrace Fail
      | Some proof -> proof
    ;;

    let to_type_id _ = Type_equal.Id.create ~name:"module tree type id" [%sexp_of: opaque]
    let unit = Leaf { type_id = unit_type_id }
    let nothing = Leaf { type_id = nothing_type_id }
  end

  let unit =
    { type_id = Type_id.unit; default = (); equal = equal_unit; sexp_of = sexp_of_unit }
  ;;

  let both model1 model2 =
    let sexp_of = Tuple2.sexp_of_t model1.sexp_of model2.sexp_of in
    let type_id = Tuple { a = model1.type_id; b = model2.type_id } in
    let default = model1.default, model2.default in
    let equal = Tuple2.equal ~eq1:model1.equal ~eq2:model2.equal in
    { type_id; default; equal; sexp_of }
  ;;

  let map
    (type k cmp)
    (module M : Comparator.S with type t = k and type comparator_witness = cmp)
    k
    cmp
    model
    =
    let sexp_of_model = model.sexp_of in
    let module M = struct
      include M

      let sexp_of_t = Comparator.sexp_of_t comparator
    end
    in
    let sexp_of_map_model = [%sexp_of: model Map.M(M).t] in
    let model_map_type_id = Map { k; cmp; by = model.type_id } in
    { type_id = model_map_type_id
    ; default = Map.empty (module M)
    ; equal = Map.equal model.equal
    ; sexp_of = sexp_of_map_model
    }
  ;;

  let map_on
    (type k cmp k_io cmp_io)
    (module M : Comparator.S with type t = k and type comparator_witness = cmp)
    (module M_io : Comparator.S with type t = k_io and type comparator_witness = cmp_io)
    k_model
    k_io
    cmp
    model
    =
    let module M = struct
      include M

      let sexp_of_t = Comparator.sexp_of_t comparator
    end
    in
    let module M_io = struct
      include M_io

      let sexp_of_t = Comparator.sexp_of_t comparator
    end
    in
    let sexp_of_model = model.sexp_of in
    let sexp_of_map_model = [%sexp_of: (M_io.t * model) Map.M(M).t] in
    let model_map_type_id = Map_on { k_model; k_io; cmp; by = model.type_id } in
    let io_equal a b = (Comparator.compare M_io.comparator) a b = 0 in
    { type_id = model_map_type_id
    ; default = Map.empty (module M)
    ; equal = Map.equal (Tuple2.equal ~eq1:io_equal ~eq2:model.equal)
    ; sexp_of = sexp_of_map_model
    }
  ;;

  let of_module ~sexp_of_model ~equal ~default ~name =
    let equal = Option.value ~default:[%eta2 phys_equal] equal in
    let type_id = Type_equal.Id.create ~name:(sprintf "%s-model" name) sexp_of_model in
    { type_id = Leaf { type_id }; default; equal; sexp_of = sexp_of_model }
  ;;

  module Hidden = struct
    type 'a model = 'a t

    type t = hidden =
      | T :
          { model : 'm
          ; info : 'm model
          }
          -> t

    let sexp_of_t (T { model; info = { sexp_of; _ }; _ }) = sexp_of model

    let equal
      (T { model = m1; info = { type_id = t1; equal; _ }; _ })
      (T { model = m2; info = { type_id = t2; _ }; _ })
      =
      match Type_id.same_witness t1 t2 with
      | Some T -> equal m1 m2
      | None -> false
    ;;

    let create (info : _ model) =
      let wrap m = T { model = m; info } in
      wrap
    ;;

    let lazy_ =
      { default = None
      ; equal = [%equal: t option]
      ; type_id =
          Leaf { type_id = Type_equal.Id.create ~name:"lazy-model" [%sexp_of: t option] }
      ; sexp_of = [%sexp_of: t option]
      }
    ;;
  end
end

module Multi_model = struct
  type t = Model.Hidden.t Int.Map.t

  let sexp_of_t (type k) (sexp_of_k : k -> Sexp.t) =
    let module K = struct
      type t = k [@@deriving sexp_of]
    end
    in
    [%sexp_of: Model.Hidden.t Map.M(K).t]
  ;;

  let find_exn = Map.find_exn
  let set = Map.set
  let to_models, of_models = Fn.id, Fn.id

  let model_info default =
    let sexp_of = [%sexp_of: int t] in
    let type_id = Model.Multi_model { multi_model = default } in
    ({ default; type_id; equal = [%equal: Model.Hidden.t Int.Map.t]; sexp_of }
     : t Model.t)
  ;;
end

module Input = struct
  module Type_id = Model.Type_id

  type 'a t = 'a Type_id.t [@@deriving sexp_of]

  let same_witness = Type_id.same_witness
  let same_witness_exn = Type_id.same_witness_exn
  let unit = Type_id.unit

  let create ?(name = "input") () =
    Model.Leaf { type_id = Type_equal.Id.create ~name sexp_of_opaque }
  ;;

  let both a b = Model.Tuple { a; b }
  let map k cmp by = Model.Optional (Model.Map { k; cmp; by })

  module Hidden = struct
    type 'a input = 'a t

    type 'key t =
      | T :
          { input : 'input
          ; type_id : 'input input
          ; key : 'key
          }
          -> 'key t

    let unit : unit t input =
      Leaf { type_id = Type_equal.Id.create ~name:"lazy input" sexp_of_opaque }
    ;;

    let int : int t option input =
      Optional (Leaf { type_id = Type_equal.Id.create ~name:"enum input" sexp_of_opaque })
    ;;
  end
end
