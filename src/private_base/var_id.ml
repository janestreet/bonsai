open! Core
include Var_id_intf

module Id = struct
  module T = Unique_id.Int63 ()

  type 'a t = T.t [@@deriving sexp_of]

  let create = T.create
  let same = T.equal

  (* Assumes that ids correspond to exactly one type. This should be true as long as we
     don't make more than 2^63 ids.

     Previously, we were relying on [Type_equal.Id] creating a new extensible variant
     constructor with a unique object ID. These IDs are backed by raw numbers in
     Javascript. Once they reach the max safe integer (about 2^53), incrementing these IDs
     will stop working. Having 2^63 identifiers should be strictly better than
     [Type_equal.Id]'s 2^53, and this [Obj.magic] should never cast incorrectly where
     [Type_equal.Id] was previously correct. *)
  let same_witness (a : 'a t) (b : 'b t) =
    if T.equal a b
    then Some ((Obj.magic : _ -> ('a, 'b) Type_equal.t) Type_equal.T)
    else None
  ;;

  let same_witness_exn (a : 'a t) (b : 'b t) =
    assert (T.equal a b);
    (Obj.magic : _ -> ('a, 'b) Type_equal.t) Type_equal.T
  ;;

  module Packed = struct
    type 'a id = 'a t

    module T = struct
      type t = T.t [@@deriving compare, equal, hash, sexp]
    end

    include T
    include Comparable.Make (T)
    include Hashable.Make (T)

    type t_gadt = T : 'a id -> t_gadt

    let reveal t = T t
  end

  let pack t = t

  module Set = struct
    include Set.Make (T)

    let is_empty = Set.is_empty
    let length = Set.length
    let iter = Set.iter
    let add = Set.add
    let remove = Set.remove
    let union = Set.union
    let fold = Set.fold
    let map_to_list t ~f = List.map (Set.to_list t) ~f
  end
end

include Id

module Map = struct
  module Data_holder = struct
    type t = Obj.t

    let hide : 'a -> t = Obj.magic
    let reveal : t -> 'a = Obj.magic
  end

  module Make (Data : T1) = struct
    module Id = Id
    module Data = Data

    type t = Data_holder.t Id.T.Map.t

    let empty = Id.T.Map.empty

    let singleton key data =
      let data = Data_holder.hide data in
      Id.T.Map.singleton key data
    ;;

    let add_exn t ~key ~data =
      let data = Data_holder.hide data in
      Map.add_exn t ~key ~data
    ;;

    let add_overwriting t ~key ~data =
      let data = Data_holder.hide data in
      Map.set t ~key ~data
    ;;

    let find t key =
      let%map.Option data = Map.find t key in
      Data_holder.reveal data
    ;;

    let find_exn t key =
      let data = Map.find_exn t key in
      Data_holder.reveal data
    ;;

    let remove = Map.remove

    type combine = { combine : 'a. key:'a Id.t -> 'a Data.t -> 'a Data.t -> 'a Data.t }
    type 'acc folder = { f : 'a. 'a Id.t -> 'a Data.t -> 'acc -> 'acc }
    type 'b mapper = { f : 'a. 'a Id.t -> 'a Data.t -> 'b }

    let merge t1 t2 { combine } =
      Map.merge_skewed t1 t2 ~combine:(fun ~key v1 v2 ->
        combine ~key (Data_holder.reveal v1) (Data_holder.reveal v2) |> Data_holder.hide)
    ;;

    let fold t ~init ({ f } : _ folder) =
      Map.fold t ~init ~f:(fun ~key ~data acc -> f key (Data_holder.reveal data) acc)
    ;;

    let map_to_list t ({ f } : _ mapper) =
      List.map (Map.to_alist t) ~f:(fun (key, data) -> f key (Data_holder.reveal data))
    ;;
  end
end
