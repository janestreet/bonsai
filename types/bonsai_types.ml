open! Core_kernel
module Snapshot = Snapshot

type ('extra, 'new_model) on_action_mismatch =
  [ `Ignore
  | `Raise
  | `Warn
  | `Custom of 'extra -> 'new_model
  ]

type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked = ..

type ('input, 'model, 'action, 'result, 'incr, 'event) eval_type =
  input:('input, 'incr) Incremental.t
  -> old_model:('model option, 'incr) Incremental.t
  -> model:('model, 'incr) Incremental.t
  -> inject:('action -> 'event)
  -> action_type_id:'action Type_equal.Id.t
  -> incr_state:'incr Incremental.State.t
  -> ('input, 'model, 'action, 'result, 'incr, 'event) unpacked
  -> (('model, 'action, 'result, 'event) Snapshot.t, 'incr) Incremental.t

(* Needs to be a module so we can [include] it later in [Expert].  Otherwise, the
   compiler complains that the types (impl vs intf) are of different kinds. *)
module Packed = struct
  type ('input, 'model, 'result, 'incr, 'event) t =
    | T :
        ('input, 'model, 'action, 'result, 'incr, 'event) unpacked
        * 'action Type_equal.Id.t
        -> ('input, 'model, 'result, 'incr, 'event) t
end

module Visitor = struct
  type t =
    { visit :
        'input 'model 'result 'incr 'event. ( 'input
                                            , 'model
                                            , 'result
                                            , 'incr
                                            , 'event )
          Packed.t
        -> ('input, 'model, 'result, 'incr, 'event) Packed.t
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
    :  ('input, 'model, 'result, 'incr, 'event) Packed.t
    -> Visitor.t
    -> ('input, 'model, 'result, 'incr, 'event) Packed.t
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

let eval_ext ~input ~old_model ~model ~inject ~action_type_id ~incr_state component =
  let (module E) = fetch component in
  E.eval ~input ~old_model ~model ~inject ~action_type_id ~incr_state component
;;

let visit_ext component visitor =
  let (Packed.T (c, _)) = component in
  let (module E) = fetch c in
  E.visit component visitor
;;

let sexp_of_unpacked component =
  let (module E) = fetch component in
  E.sexp_of_unpacked component
;;
