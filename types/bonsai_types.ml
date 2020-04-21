open! Core_kernel
module Snapshot = Snapshot
module Environment = Environment

type on_action_mismatch =
  [ `Ignore
  | `Raise
  | `Warn
  ]

module type Model = sig
  type t [@@deriving sexp, equal]
end

module type Action = sig
  type t [@@deriving sexp_of]
end

(** We need keys to have [t_of_sexp] and [sexp_of_t] *)
module type Comparator = sig
  type t [@@deriving sexp]

  include Comparator.S with type t := t
end

type ('k, 'cmp) comparator =
  (module Comparator with type t = 'k and type comparator_witness = 'cmp)

type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked = ..

type ('input, 'model, 'action, 'result, 'incr, 'event) eval_type =
  input:('input, 'incr) Incremental.t
  -> old_model:('model option, 'incr) Incremental.t
  -> model:('model, 'incr) Incremental.t
  -> inject:('action -> 'event)
  -> action_type_id:'action Type_equal.Id.t
  -> environment:'incr Environment.t
  -> incr_state:'incr Incremental.State.t
  -> ('input, 'model, 'action, 'result, 'incr, 'event) unpacked
  -> (('model, 'action, 'result, 'event) Snapshot.t, 'incr) Incremental.t

let unit_type_id =
  Type_equal.Id.create ~name:(Source_code_position.to_string [%here]) [%sexp_of: unit]
;;

(* Needs to be a module so we can [include] it later in [Expert].  Otherwise, the
   compiler complains that the types (impl vs intf) are of different kinds. *)
module Packed = struct
  type 'model model_info =
    { default : 'model
    ; equal : 'model -> 'model -> bool
    ; type_id : 'model Type_equal.Id.t
    ; sexp_of : 'model -> Sexp.t
    ; of_sexp : Sexp.t -> 'model
    }

  let unit_model_info =
    { type_id = unit_type_id
    ; default = ()
    ; equal = equal_unit
    ; sexp_of = sexp_of_unit
    ; of_sexp = unit_of_sexp
    }
  ;;

  let both_model_infos model1 model2 =
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

  type ('input, 'result, 'incr, 'event) t =
    | T :
        { unpacked : ('input, 'model, 'action, 'result, 'incr, 'event) unpacked
        ; action_type_id : 'action Type_equal.Id.t
        ; model : 'model model_info
        }
        -> ('input, 'result, 'incr, 'event) t
end

module Visitor = struct
  type t =
    { visit :
        'input 'model 'result 'incr 'event. ('input, 'result, 'incr, 'event) Packed.t
        -> ('input, 'result, 'incr, 'event) Packed.t
    }
end

let nothing_type_id =
  Type_equal.Id.create
    ~name:(Source_code_position.to_string [%here])
    [%sexp_of: Nothing.t]
;;

module type Definition = sig
  val extension_constructor : Obj.Extension_constructor.t
  val sexp_of_unpacked : _ unpacked -> Sexp.t
  val eval : ('input, 'model, 'action, 'result, 'incr, 'event) eval_type

  val visit
    :  ('input, 'result, 'incr, 'event) Packed.t
    -> Visitor.t
    -> ('input, 'result, 'incr, 'event) Packed.t
end

let handlers : (module Definition) Hashtbl.M(Int).t = Hashtbl.create (module Int)

let define (h : (module Definition)) =
  let (module H) = h in
  let key = H.extension_constructor |> Obj.Extension_constructor.id in
  Hashtbl.add_exn handlers ~key ~data:h
;;

let fetch (component : _ unpacked) =
  Hashtbl.find_exn
    handlers
    (component |> Obj.Extension_constructor.of_val |> Obj.Extension_constructor.id)
;;

let eval_ext
      ~input
      ~old_model
      ~model
      ~inject
      ~action_type_id
      ~environment
      ~incr_state
      component
  =
  let (module E) = fetch component in
  E.eval
    ~input
    ~old_model
    ~model
    ~inject
    ~action_type_id
    ~environment
    ~incr_state
    component
;;

let visit_ext component visitor =
  let (Packed.T { unpacked; action_type_id = _; model = _ }) = component in
  let (module E) = fetch unpacked in
  E.visit component visitor
;;

let sexp_of_unpacked component =
  let (module E) = fetch component in
  E.sexp_of_unpacked component
;;
