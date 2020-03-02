open! Core_kernel
open! Import
include Bonsai_intf

module Generic = struct
  include Component
  include Packed

  let eval = eval_ext
  let sexp_of_t (T (t, _)) = sexp_of_unpacked t

  (* Constructor Functions *)

  let const r = T (Const.C r, nothing_type_id)
  let pure ~f = T (Pure.Pure_input f, nothing_type_id)

  let leaf ~apply_action ~compute ~name ~sexp_of_action =
    let action_type_id = Type_equal.Id.create ~name sexp_of_action in
    T (Leaf.C { apply_action; compute; name }, action_type_id)
  ;;

  module type Enum = sig
    type t [@@deriving sexp_of, compare, enumerate]
  end

  let enum
        (type k)
        ?(on_action_mismatch = `Ignore)
        (module E : Enum with type t = k)
        ~which
        ~(handle : k -> (_, _, _, _, _) t)
    =
    let module E_map = Map.Make_plain (E) in
    let f key =
      let (T (unpacked, action_type_id)) = handle key in
      Switch.Erase_action
        { t = unpacked
        ; action_type_id
        ; key
        ; sexp_of_key = E.sexp_of_t
        ; on_action_mismatch
        }
    in
    let components =
      List.fold E.all ~init:E_map.empty ~f:(fun map k ->
        Map.add_exn map ~key:k ~data:(f k))
    in
    T (Switch.Enum { components; which }, Switch.Case_action.type_id)
  ;;

  let if_ ?on_action_mismatch cond ~then_ ~else_ =
    enum
      ?on_action_mismatch
      (module Bool)
      ~which:cond
      ~handle:(function
        | true -> then_
        | false -> else_)
  ;;

  (* Modifier Functions *)

  let compose (T (t1, action_type_id1)) (T (t2, action_type_id2)) =
    T
      ( Compose.C { t1; action_type_id1; t2; action_type_id2 }
      , Type_equal.Id.create
          ~name:(Source_code_position.to_string [%here])
          (Either.sexp_of_t
             (Type_equal.Id.to_sexp action_type_id1)
             (Type_equal.Id.to_sexp action_type_id2)) )
  ;;

  let map (T (t, action_type_id)) ~f = T (Mapn.Map1 { t; f }, action_type_id)

  let map2 (T (t1, action_type_id1)) (T (t2, action_type_id2)) ~f =
    T
      ( Mapn.Map2 { t1; action_type_id1; t2; action_type_id2; f }
        ,
        Type_equal.Id.create
          ~name:(Source_code_position.to_string [%here])
          (Either.sexp_of_t
             (Type_equal.Id.to_sexp action_type_id1)
             (Type_equal.Id.to_sexp action_type_id2)) )
  ;;

  let map_input (T (t, action_type_id)) ~f = T (Map_input.C { t; f }, action_type_id)
  let return = const
  let both a b = map2 a b ~f:Tuple2.create
  let ( >>| ) t f = map t ~f

  module Infix = struct
    let ( >>> ) = compose
    let ( >>| ) = ( >>| )
    let ( @>> ) f t = map_input t ~f
  end

  open Infix

  module Model = struct
    let f (T (t, action_type_id)) ~get ~set =
      T (Projection.C { t; projection = { get; set } }, action_type_id)
    ;;

    let field field t = f t ~get:(Field.get field) ~set:(Field.fset field)
    let ignore = f ~get:(fun _ -> ()) ~set:(fun m () -> m)

    let state_machine here ~sexp_of_action ~apply_action =
      let name =
        sprintf "state-machine defined at %s" (Source_code_position.to_string here)
      in
      leaf ~apply_action ~compute:(fun ~inject _ _ -> inject) ~sexp_of_action ~name
    ;;

    let to_input_with_other (T (a, type_id)) = T (With_readonly_model.C a, type_id)
    let to_input t = to_input_with_other (snd @>> t)
  end

  let pure_from_model ~f = T (Pure.Pure_model f, nothing_type_id)
  let input = T (Pure.Return_input, nothing_type_id)
  let model = T (Pure.Return_model, nothing_type_id)

  module List_deprecated = struct end

  module With_incr = struct
    let pure ~f = T (Pure_incr.C f, nothing_type_id)
    let of_incr t = pure ~f:(Fn.const t)
    let pure_from_model ~f = compose (const ()) (pure ~f |> Model.to_input)

    let leaf ~apply_action ~compute ~name ~sexp_of_action =
      let action_type_id = Type_equal.Id.create ~name sexp_of_action in
      T (Leaf_incr.C { apply_action; compute; name }, action_type_id)
    ;;

    let model_cutoff (T (t, action_type_id)) ~cutoff =
      T (Cutoff.Model { t; cutoff }, action_type_id)
    ;;

    let value_cutoff ~cutoff = T (Cutoff.Value cutoff, nothing_type_id)
    let map (T (t, action_type_id)) ~f = T (Map_incr.C { t; f }, action_type_id)
    let map_input t ~f = compose (pure ~f) t
  end

  module Option = struct
    let default_on_action_for_none = `Ignore

    let wrap_model
          ?(on_action_for_none = default_on_action_for_none)
          (T (t, action_type_id))
      =
      T (Wrap_option.C { t; on_action_for_none }, action_type_id)
    ;;

    let wrap_model_with_default ?on_action_for_none t ~default =
      let open Incremental.Let_syntax in
      wrap_model ?on_action_for_none t
      |> With_incr.map ~f:(fun o ->
        match%map o with
        | Some v -> v
        | None -> default)
    ;;
  end

  module Either = struct
    let default_on_action_for_other_component = `Ignore

    let wrap_model
          ?(on_action_for_other_component = default_on_action_for_other_component)
          (T (t1, action_type_id1))
          (T (t2, action_type_id2))
      =
      T
        ( Wrap_either.C
            { t1; action_type_id1; t2; action_type_id2; on_action_for_other_component }
        , Type_equal.Id.create
            ~name:(Source_code_position.to_string [%here])
            (Either.sexp_of_t
               (Type_equal.Id.to_sexp action_type_id1)
               (Type_equal.Id.to_sexp action_type_id2)) )
    ;;

    let wrap_model_with_same_result ?on_action_for_other_component a b =
      wrap_model ?on_action_for_other_component a b
      |> With_incr.map ~f:(fun o ->
        match%map.Incremental o with
        | First v -> v
        | Second v -> v)
    ;;
  end

  module Map = struct
    let associ_model
          (type k cmp)
          ?(comparator : (k, cmp) Map.comparator option)
          (T (t, action_type_id))
      =
      let sexp_of_key =
        match comparator with
        | Some (module Comparator) -> Comparator.comparator.sexp_of_t
        | None -> sexp_of_opaque
      in
      T
        ( Assoc.By_model
            { t; action_type_id; sexp_of_key; assoc = { r_by_k = T; d_by_k = T } }
        , Type_equal.Id.create
            ~name:(Source_code_position.to_string [%here])
            (sexp_of_pair sexp_of_key (Type_equal.Id.to_sexp action_type_id)) )
    ;;

    let assoc_model ?comparator t = associ_model ?comparator (snd @>> t)

    let associ_input
          (type k cmp)
          ?(comparator : (k, cmp) Map.comparator option)
          (T (t, action_type_id))
      =
      let sexp_of_key =
        match comparator with
        | Some (module Comparator) -> Comparator.comparator.sexp_of_t
        | None -> sexp_of_opaque
      in
      T
        ( Assoc.By_input
            { t; action_type_id; sexp_of_key; assoc = { r_by_k = T; d_by_k = T } }
        , Type_equal.Id.create
            ~name:(Source_code_position.to_string [%here])
            (sexp_of_pair sexp_of_key (Type_equal.Id.to_sexp action_type_id)) )
    ;;

    let assoc_input ?comparator t = associ_input ?comparator (snd @>> t)

    let merge a b ~f =
      both a b
      |> With_incr.map ~f:(fun pair ->
        let open Incremental.Let_syntax in
        let return = pair |> Incremental.state |> Incremental.return in
        let%pattern_bind a, b = pair in
        Incr_map.merge a b ~f)
    ;;
  end

  module Open_on_rhs_intf = struct
    module type S = sig end
  end

  module Let_syntax = struct
    let return = return
    let map = map
    let both = both

    include Infix

    module Let_syntax = struct
      let return = return
      let both = both
      let map = map

      module Open_on_rhs = Infix
    end
  end

  module Arrow = struct
    let arr f = pure ~f
    let first t = both (fst @>> t) (arr snd)
    let second t = both (arr fst) (snd @>> t)
    let split t u = both (fst @>> t) (snd @>> u)
    let fanout = both
    let extend_first component = pure ~f:(fun x -> x, x) >>> first component
    let extend_second component = pure ~f:(fun x -> x, x) >>> second component
    let ( *** ) = split
    let ( &&& ) = fanout
    let ( ^>> ) = ( @>> )
    let ( >>^ ) = ( >>| )
  end

  module Expert = struct
    module Snapshot = Snapshot

    type nonrec ('input, 'model, 'action, 'result, 'incr, 'event) unpacked =
      ('input, 'model, 'action, 'result, 'incr, 'event) unpacked

    include Packed

    let reveal = Fn.id
    let conceal = Fn.id

    let of_full constructed_at ~f ~action_type_id =
      T (Full.C { f; constructed_at }, action_type_id)
    ;;

    let eval = eval
    let optimize = Optimize.optimize
  end
end

module type S = sig
  module Incr : Incremental.S
  module Event : Event.S
  include S_gen with module Incr := Incr with module Event := Event

  val to_generic : ('i, 'm, 'r) t -> ('i, 'm, 'r, Incr.state_witness, Event.t) Generic.t
  val of_generic : ('i, 'm, 'r, Incr.state_witness, Event.t) Generic.t -> ('i, 'm, 'r) t
end

module Make (Incr : Incremental.S) (Event : Event.S) :
  S with module Incr = Incr and module Event = Event = struct
  module Incr = Incr
  module Event = Event
  include Generic

  type ('i, 'm, 'r) t = ('i, 'm, 'r, Incr.state_witness, Event.t) Generic.Packed.t

  let to_generic = Fn.id
  let of_generic = Fn.id

  module type S = sig
    module Input : T
    module Model : T

    module Action : sig
      type t [@@deriving sexp_of]
    end

    module Result : T

    val apply_action
      :  inject:(Action.t -> Event.t)
      -> schedule_event:(Event.t -> unit)
      -> Input.t
      -> Model.t
      -> Action.t
      -> Model.t

    val compute : inject:(Action.t -> Event.t) -> Input.t -> Model.t -> Result.t
    val name : string
  end

  type ('input, 'model, 'action, 'result) component_s =
    (module S
      with type Input.t = 'input
       and type Model.t = 'model
       and type Action.t = 'action
       and type Result.t = 'result)

  module M (Component : S) = struct
    type nonrec t = (Component.Input.t, Component.Model.t, Component.Result.t) t
  end

  let of_module (type i m a r) m =
    let module M = (val m : S
                    with type Input.t = i
                     and type Action.t = a
                     and type Model.t = m
                     and type Result.t = r)
    in
    T
      ( Leaf.C { apply_action = M.apply_action; compute = M.compute; name = M.name }
      , Type_equal.Id.create ~name:M.name [%sexp_of: M.Action.t] )
  ;;

  include struct
    open Applicative.Make3_using_map2 (struct
        type nonrec ('r, 'i, 'm) t = ('i, 'm, 'r) t

        let map = `Custom map
        let map2 = map2
        let return = const
      end)

    module Applicative_infix = Applicative_infix

    let all = all
    let all_unit = all_unit
    let apply = apply
    let both = both
    let map3 = map3
    let return = return
    let ( <* ) = ( <* )
    let ( *> ) = ( *> )
    let ( <*> ) = ( <*> )
    let ( >>| ) = ( >>| )
  end

  module With_incr = struct
    include With_incr

    module type S = sig
      module Input : T
      module Model : T

      module Action : sig
        type t [@@deriving sexp_of]
      end

      module Result : T

      val apply_action
        :  Input.t Incr.t
        -> Model.t Incr.t
        -> inject:(Action.t -> Event.t)
        -> (schedule_event:(Event.t -> unit) -> Action.t -> Model.t) Incr.t

      val compute
        :  Input.t Incr.t
        -> Model.t Incr.t
        -> inject:(Action.t -> Event.t)
        -> Result.t Incr.t

      val name : string
    end

    type ('input, 'model, 'action, 'result) component_s =
      (module S
        with type Input.t = 'input
         and type Model.t = 'model
         and type Action.t = 'action
         and type Result.t = 'result)

    let of_module (type i m a r) m =
      let module M = (val m : S
                      with type Input.t = i
                       and type Action.t = a
                       and type Model.t = m
                       and type Result.t = r)
      in
      T
        ( Leaf_incr.C
            { apply_action = M.apply_action; compute = M.compute; name = M.name }
        , Type_equal.Id.create ~name:M.name [%sexp_of: M.Action.t] )
    ;;
  end
end

module Snapshot = Snapshot
