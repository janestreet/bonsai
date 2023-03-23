open! Core
module Q = Base_quickcheck


let log = ref []
let clear_log () = log := []
let read_log () = Sexp.List !log
let log_s sexp = log := sexp :: !log
let weight_scalar = 10.
let size_scalar = 4

module Map_comparator = Comparator.Derived2_phantom (struct
    type ('a, 'b, 'c) t = ('a, 'b, 'c) Map.t

    let compare _ _ _ _ = assert false
    let sexp_of_t _ _ _ = assert false
  end)

module Effect_func_comparator = Comparator.Derived (struct
    type 'a t = 'a -> unit Bonsai.Effect.t

    let compare _ _ _ = assert false
    let sexp_of_t _ _ = assert false
  end)

module Witness = struct
  type ('a, 'cmp) t =
    | Unit : (unit, Unit.comparator_witness) t
    | Int : (int, Int.comparator_witness) t
    | Either :
        ('a, 'cmp_a) t * ('b, 'cmp_b) t
        -> (('a, 'b) Either.t, ('cmp_a, 'cmp_b) Either.comparator_witness) t
    | Tuple :
        ('a, 'cmp_a) t * ('b, 'cmp_b) t
        -> (('a, 'b) Tuple2.t, ('cmp_a, 'cmp_b) Tuple2.comparator_witness) t
    | Map :
        ('k, 'k_cmp) t * ('v, 'v_cmp) t
        -> (('k, 'v, 'k_cmp) Map.t, ('k_cmp, 'v_cmp) Map_comparator.comparator_witness) t
    | Effect_func :
        ('a, 'cmp) t
        -> ('a -> unit Bonsai.Effect.t, 'cmp Effect_func_comparator.comparator_witness) t

  let num_constructors = 6.

  let rec equal : type w cmp. (w, cmp) t -> w -> w -> bool = function
    | Unit -> Unit.equal
    | Int -> ( = )
    | Either (first_witness, second_witness) ->
      Either.equal (equal first_witness) (equal second_witness)
    | Tuple (first_witness, second_witness) ->
      Tuple2.equal ~eq1:(equal first_witness) ~eq2:(equal second_witness)
    | Map (_key_witness, value_witness) -> Map.equal (equal value_witness)
    | Effect_func _ -> fun _ _ -> true
  ;;

  let to_string (type w cmp) (witness : (w, cmp) t) =
    match witness with
    | Unit -> "Unit"
    | Int -> "Int"
    | Either (_, _) -> "Either"
    | Tuple (_, _) -> "Tuple"
    | Map (_, _) -> "Map"
    | Effect_func _ -> "Effect_func"
  ;;

  type ('a, 'b, 'cmp_a, 'cmp_b) same = T : ('a, 'a, 'cmp_a, 'cmp_a) same

  exception Not_same

  let rec same_witness_exn
    : type a b cmp_a cmp_b. (a, cmp_a) t -> (b, cmp_b) t -> (a, b, cmp_a, cmp_b) same
    =
    fun witness1 witness2 ->
    match witness1, witness2 with
    | Unit, Unit -> T
    | Int, Int -> T
    | Either (first_a, second_a), Either (first_b, second_b) ->
      let T = same_witness_exn first_a first_b in
      let T = same_witness_exn second_a second_b in
      T
    | Tuple (first_a, second_a), Tuple (first_b, second_b) ->
      let T = same_witness_exn first_a first_b in
      let T = same_witness_exn second_a second_b in
      T
    | Map (key_a, value_a), Map (key_b, value_b) ->
      let T = same_witness_exn key_a key_b in
      let T = same_witness_exn value_a value_b in
      T
    | Effect_func inner_a, Effect_func inner_b ->
      let T = same_witness_exn inner_a inner_b in
      T
    | _, _ -> raise_notrace Not_same
  ;;

  let is_same_witness
        (type a b cmp_a cmp_b)
        (witness1 : (a, cmp_a) t)
        (witness2 : (b, cmp_b) t)
    : bool
    =
    match same_witness_exn witness1 witness2 with
    | T -> true
    | exception Not_same -> false
  ;;

  let same_witness
    : type a b cmp_a cmp_b.
      (a, cmp_a) t -> (b, cmp_b) t -> (a, b, cmp_a, cmp_b) same option
    =
    fun witness1 witness2 ->
      match same_witness_exn witness1 witness2 with
      | T -> Some T
      | exception Not_same -> None
  ;;

  let same_witness_exn
        (type a b cmp_a cmp_b)
        (witness1 : (a, cmp_a) t)
        (witness2 : (b, cmp_b) t)
    : (a, b, cmp_a, cmp_b) same
    =
    match same_witness_exn witness1 witness2 with
    | T -> T
    | exception Not_same ->
      raise_s [%message "Witnesses passed to [same_witness_exn] are not the same!"]
  ;;

  let is_state_witness (type a cmp) (witness : (a, cmp) t) =
    match witness with
    | Tuple (first_witness, Effect_func inner_witness) ->
      is_same_witness first_witness inner_witness
    | _ -> false
  ;;

  type packed = T : ('a, 'cmp) t -> packed

  let to_goal_list witness =
    let rec iter : type a cmp. (a, cmp) t -> packed list -> packed list =
      fun witness goal_list ->
        match is_state_witness witness with
        | true -> T witness :: goal_list
        | false ->
          (match witness with
           | Tuple (first_witness, second_witness) ->
             let goal_list = iter first_witness goal_list in
             iter second_witness goal_list
           | Either (first_witness, second_witness) ->
             let goal_list = iter first_witness goal_list in
             iter second_witness goal_list
           | Map (key_witness, value_witness) ->
             let goal_list = iter key_witness goal_list in
             iter value_witness goal_list
           | Effect_func inner_witness -> iter inner_witness goal_list
           | Unit | Int -> T witness :: goal_list)
    in
    iter witness []
  ;;

  type modifier =
    | Enabled
    | Disabled

  (* [make_witness] generates a witness of random type and then passes it into a function
     to use to make a [Q.Generator.t] of any type *)
  type 'b witness_handler = { f : 'a 'cmp. ('a, 'cmp) t -> 'b Q.Generator.t }

  let rec make_witness
    : type b. no_comparator_types:modifier -> b witness_handler -> b Q.Generator.t
    =
    fun ~no_comparator_types handler ->
    let no_comp_weight =
      match no_comparator_types with
      | Enabled -> 1.
      | Disabled -> 0.
    in
    let open Q.Generator.Let_syntax in
    let base_cases = [ 1., handler.f Unit; 1., handler.f Int ] in
    match%bind Q.Generator.size with
    | 0 -> Q.Generator.weighted_union base_cases
    | original_size ->
      let continue witness =
        Q.Generator.with_size ~size:original_size (handler.f witness)
      in
      let either_gen =
        make_witness
          ~no_comparator_types
          { f =
              (fun first_witness ->
                 make_witness
                   ~no_comparator_types
                   { f =
                       (fun second_witness ->
                          continue (Either (first_witness, second_witness)))
                   })
          }
      in
      let tuple_gen =
        make_witness
          ~no_comparator_types
          { f =
              (fun first_witness ->
                 make_witness
                   ~no_comparator_types
                   { f =
                       (fun second_witness ->
                          continue (Tuple (first_witness, second_witness)))
                   })
          }
      in
      let map_gen =
        make_witness
          ~no_comparator_types:Disabled
          { f =
              (fun key_witness ->
                 make_witness
                   ~no_comparator_types:Enabled
                   { f = (fun value_witness -> continue (Map (key_witness, value_witness)))
                   })
          }
      in
      let effect_func_gen =
        make_witness
          ~no_comparator_types:Enabled
          { f =
              (fun inner_witness ->
                 continue (Tuple (inner_witness, Effect_func inner_witness)))
          }
      in
      Q.Generator.with_size
        ~size:(original_size - 1)
        (Q.Generator.weighted_union
           (base_cases
            @ [ 1., either_gen
              ; 1., tuple_gen
              ; no_comp_weight, map_gen
              ; no_comp_weight, effect_func_gen
              ]))
  ;;

  let make_witness handler = make_witness ~no_comparator_types:Enabled handler

  type 'c effect_witness_handler =
    { f : 'f 's 'cmp 'b 'cmp_b. ('f * 's, 'cmp) t -> ('b, 'cmp_b) t -> 'c Q.Generator.t }

  let rec make_effect_witness : type c. c effect_witness_handler -> c Q.Generator.t =
    fun handler ->
    let open Q.Generator.Let_syntax in
    let base_cases =
      let base_gen =
        make_witness
          { f =
              (fun inner_witness ->
                 let effect_wit = Effect_func inner_witness in
                 handler.f (Tuple (inner_witness, effect_wit)) effect_wit)
          }
      in
      [ 1., base_gen ]
    in
    match%bind Q.Generator.size with
    | 0 -> Q.Generator.weighted_union base_cases
    | original_size ->
      let recur_gen =
        make_witness
          { f =
              (fun inner_witness ->
                 make_effect_witness
                   { f =
                       (fun state_witness effect_witness ->
                          let new_effect_witness = Effect_func inner_witness in
                          Q.Generator.with_size
                            ~size:original_size
                            (handler.f
                               (Tuple
                                  (Tuple (inner_witness, new_effect_witness), state_witness))
                               (Tuple (new_effect_witness, effect_witness))))
                   })
          }
      in
      Q.Generator.with_size
        ~size:(original_size - 1)
        (Q.Generator.weighted_union (base_cases @ [ 1., recur_gen ]))
  ;;
end

module type Quickcheckable = sig
  type context
  type goal
  type 'a t

  type packed =
    | T :
        { unpacked : 'a t
        ; witness : ('a, 'cmp) Witness.t
        }
        -> packed

  val quickcheck_generator : context -> goal -> ('a, 'cmp) Witness.t -> 'a t Q.Generator.t
  val packed_quickcheck_shrinker : packed Q.Shrinker.t
  val quickcheck_observer : ('a, 'cmp) Witness.t -> 'a t Q.Observer.t
end

module type Packed_quickcheckable = sig
  type context
  type goal
  type packed

  val quickcheck_generator : context -> goal -> packed Q.Generator.t
  val quickcheck_observer : packed Q.Observer.t Lazy.t
end

module With_witness (M : Quickcheckable) :
  Packed_quickcheckable
  with type packed := M.packed
   and type context := M.context
   and type goal := M.goal = struct
  let quickcheck_generator context goal =
    let open Q.Generator.Let_syntax in
    Witness.make_witness
      { f =
          (fun witness ->
             let%map unpacked = M.quickcheck_generator context goal witness in
             M.T { unpacked; witness })
      }
  ;;

  let quickcheck_observer =
    lazy
      (Q.Observer.create (fun (M.T { unpacked; witness }) ~size ~hash ->
         Q.Observer.observe (M.quickcheck_observer witness) unpacked ~size ~hash))
  ;;
end

module type Comparator_and_model = sig
  type t

  include Comparator.S with type t := t
  include Bonsai.Model with type t := t
end

let rec make_comparator_and_model
  : type w cmp.
    (w, cmp) Witness.t
    -> (module Comparator_and_model with type t = w and type comparator_witness = cmp)
  =
  fun witness ->
  match witness with
  | Unit -> (module Unit)
  | Int -> (module Int)
  | Either (first_witness, second_witness) ->
    let module First = (val make_comparator_and_model first_witness) in
    let module Second = (val make_comparator_and_model second_witness) in
    (module struct
      type t = (First.t, Second.t) Either.t [@@deriving sexp]

      type comparator_witness =
        (First.comparator_witness, Second.comparator_witness) Either.comparator_witness

      let comparator : (t, comparator_witness) Comparator.t =
        Either.comparator First.comparator Second.comparator
      ;;

      let equal = Witness.equal witness
    end)
  | Tuple (first_witness, second_witness) ->
    let module First = (val make_comparator_and_model first_witness) in
    let module Second = (val make_comparator_and_model second_witness) in
    (module struct
      type t = (First.t, Second.t) Tuple2.t [@@deriving sexp]

      type comparator_witness =
        (First.comparator_witness, Second.comparator_witness) Tuple2.comparator_witness

      let comparator : (t, comparator_witness) Comparator.t =
        Tuple2.comparator First.comparator Second.comparator
      ;;

      let equal = Witness.equal witness
    end)
  | Map (key_witness, value_witness) ->
    let module K = (val make_comparator_and_model key_witness) in
    let module V = (val make_comparator_and_model value_witness) in
    (module struct
      type t = V.t Map.M(K).t [@@deriving sexp]

      let equal = Witness.equal witness

      type comparator_witness =
        (K.comparator_witness, V.comparator_witness) Map_comparator.comparator_witness

      let comparator = Map_comparator.comparator K.comparator V.comparator
    end)
  | Effect_func inner_witness ->
    let module M = (val make_comparator_and_model inner_witness) in
    (module struct
      type t = M.t -> unit Bonsai.Effect.t [@@deriving sexp]

      let equal = Witness.equal witness

      type comparator_witness =
        M.comparator_witness Effect_func_comparator.comparator_witness

      let comparator = Effect_func_comparator.comparator M.comparator
    end)
;;

let real_data_to_sexp (type a cmp) (witness : (a, cmp) Witness.t) (data : a) =
  let module M = (val make_comparator_and_model witness) in
  M.sexp_of_t data
;;

let real_data_to_string (type a cmp) (witness : (a, cmp) Witness.t) (data : a) : string =
  Sexp.to_string_hum (real_data_to_sexp witness data)
;;

let rec real_data_observer : type w cmp. (w, cmp) Witness.t -> w Q.Observer.t = function
  | Unit -> quickcheck_observer_unit
  | Int -> quickcheck_observer_int
  | Either (first_witness, second_witness) ->
    Either.quickcheck_observer
      (real_data_observer first_witness)
      (real_data_observer second_witness)
  | Tuple (first_witness, second_witness) ->
    Q.Observer.both (real_data_observer first_witness) (real_data_observer second_witness)
  | Map (key_witness, value_witness) ->
    Map.quickcheck_observer
      (real_data_observer key_witness)
      (real_data_observer value_witness)
  | Effect_func _inner_witness ->
    Q.Observer.create (fun _effect_func ~size:_ ~hash -> hash_fold_int hash 0)
;;

let rec real_data_generator : type w cmp. (w, cmp) Witness.t -> w Q.Generator.t = function
  | Unit -> quickcheck_generator_unit
  | Int -> quickcheck_generator_int
  | Either (first_witness, second_witness) ->
    Either.quickcheck_generator
      (real_data_generator first_witness)
      (real_data_generator second_witness)
  | Tuple (first_witness, second_witness) ->
    Q.Generator.both
      (real_data_generator first_witness)
      (real_data_generator second_witness)
  | Map (key_witness, value_witness) ->
    Map.quickcheck_generator
      (module (val make_comparator_and_model key_witness))
      (real_data_generator key_witness)
      (real_data_generator value_witness)
  | Effect_func inner_witness ->
    Q.Generator.return
      (Bonsai.Effect.of_sync_fun (fun input ->
         log_s (real_data_to_sexp inner_witness input)))
;;

let rec real_data_shrinker : type w cmp. (w, cmp) Witness.t -> w Q.Shrinker.t = function
  | Unit -> quickcheck_shrinker_unit
  | Int -> quickcheck_shrinker_int
  | Either (first_witness, second_witness) ->
    Either.quickcheck_shrinker
      (real_data_shrinker first_witness)
      (real_data_shrinker second_witness)
  | Tuple (first_witness, second_witness) ->
    Q.Shrinker.both (real_data_shrinker first_witness) (real_data_shrinker second_witness)
  | Map (key_witness, value_witness) ->
    Map.quickcheck_shrinker
      (real_data_shrinker key_witness)
      (real_data_shrinker value_witness)
  | Effect_func _inner_witness -> Q.Shrinker.atomic
;;

let weighted weight make_gen =
  if Float.equal weight 0. then [] else [ weight, make_gen () ]
;;

module Function = struct
  type ('input, 'output) t =
    | Identity : ('input, 'input) t
    | Const : 'output -> ('input, 'output) t
    | Add_const : int -> (int, int) t
    | Snd : ('a * 'b, 'b) t
    | Map_tuple : ('a, 'b) t * ('c, 'd) t -> ('a * 'c, 'b * 'd) t
    | Make_either : [ `First | `Second ] -> ('a * 'b, ('a, 'b) Either.t) t

  let rec quickcheck_generator
    : type input output cmp_in cmp_out.
      (input, cmp_in) Witness.t
      -> (output, cmp_out) Witness.t
      -> (input, output) t Q.Generator.t
    =
    fun in_witness out_witness ->
    let open Q.Generator.Let_syntax in
    let identity_weight =
      match Witness.is_same_witness in_witness out_witness with
      | true -> Witness.num_constructors
      | _ -> 0.
    in
    let int_weight =
      match in_witness, out_witness with
      | Int, Int -> Float.square Witness.num_constructors
      | _, _ -> 0.
    in
    let snd_weight =
      match in_witness with
      | Tuple (_, second_witness) ->
        (match Witness.is_same_witness second_witness out_witness with
         | true -> Float.square Witness.num_constructors
         | _ -> 0.)
      | _ -> 0.
    in
    let map_tuple_weight =
      match in_witness, out_witness with
      | Tuple _, Tuple _ -> Float.square Witness.num_constructors
      | _ -> 0.
    in
    let identity_weighted_gen =
      weighted identity_weight (fun () ->
        let handler
              (type input output cmp_in cmp_out)
              (in_witness : (input, cmp_in) Witness.t)
              (out_witness : (output, cmp_out) Witness.t)
          : (input, output) t Q.Generator.t
          =
          let T = Witness.same_witness_exn in_witness out_witness in
          Q.Generator.return Identity
        in
        handler in_witness out_witness)
    in
    let const_gen =
      let%map output = real_data_generator out_witness in
      Const output
    in
    let int_weighted_gen =
      weighted int_weight (fun () ->
        let T = Witness.same_witness_exn out_witness Int in
        let T = Witness.same_witness_exn in_witness Int in
        let%map add = real_data_generator in_witness in
        (Add_const add : (input, output) t))
    in
    let snd_weighted_gen =
      weighted snd_weight (fun () ->
        let handler
              (type input output cmp_in cmp_out)
              (in_witness : (input, cmp_in) Witness.t)
              (out_witness : (output, cmp_out) Witness.t)
          : (input, output) t Q.Generator.t
          =
          match in_witness with
          | Tuple (_, second_witness) ->
            let T = Witness.same_witness_exn out_witness second_witness in
            Q.Generator.return Snd
          | _ -> assert false
        in
        handler in_witness out_witness)
    in
    let base_cases =
      List.concat
        [ identity_weighted_gen; [ 1., const_gen ]; int_weighted_gen; snd_weighted_gen ]
    in
    match%bind Q.Generator.size with
    | 0 -> Q.Generator.weighted_union base_cases
    | size ->
      let map_tuple_weighted_gen =
        weighted map_tuple_weight (fun () ->
          let handler
                (type input output cmp_in cmp_out)
                (in_witness : (input, cmp_in) Witness.t)
                (out_witness : (output, cmp_out) Witness.t)
            : (input, output) t Q.Generator.t
            =
            match in_witness, out_witness with
            | Tuple (in_first, in_second), Tuple (out_first, out_second) ->
              let%bind func1 = quickcheck_generator in_first out_first in
              let%map func2 = quickcheck_generator in_second out_second in
              Map_tuple (func1, func2)
            | _ -> assert false
          in
          handler in_witness out_witness)
      in
      Q.Generator.with_size
        ~size:(size - 1)
        (Q.Generator.weighted_union (List.concat [ base_cases; map_tuple_weighted_gen ]))
  ;;

  let rec quickcheck_observer
    : type output input cmp_in cmp_out.
      (input, cmp_in) Witness.t
      -> (output, cmp_out) Witness.t
      -> (input, output) t Q.Observer.t
    =
    fun in_witness out_witness ->
      Q.Observer.create (fun t ~size ~hash ->
        match (t : (input, output) t) with
        | Identity -> hash_fold_int hash 0
        | Const output ->
          let hash = hash_fold_int hash 1 in
          Q.Observer.observe (real_data_observer out_witness) output ~size ~hash
        | Add_const add ->
          let hash = hash_fold_int hash 2 in
          Q.Observer.observe Int.quickcheck_observer add ~size ~hash
        | Snd -> hash_fold_int hash 3
        | Map_tuple (func1, func2) ->
          let hash = hash_fold_int hash 4 in
          (match in_witness, out_witness with
           | Tuple (in_first, in_second), Tuple (out_first, out_second) ->
             let hash =
               Q.Observer.observe (quickcheck_observer in_first out_first) func1 ~size ~hash
             in
             Q.Observer.observe (quickcheck_observer in_second out_second) func2 ~size ~hash
           | _ -> assert false)
        | Make_either which ->
          let hash = hash_fold_int hash 5 in
          (match in_witness with
           | Tuple _ ->
             (match which with
              | `First -> hash_fold_int hash 0
              | `Second -> hash_fold_int hash 1)
           | _ -> assert false))
  ;;
end

module Value = struct
  module rec T : sig
    type 'a t =
      | Return : 'a -> 'a t
      | Map : 'a t * ('a, 'cmp) Witness.t * ('a, 'b) Function.t -> 'b t
      | Real_value : 'a Bonsai.Value.t -> 'a t
      | Var : 'a -> 'a t
      | Both :
          { first : 'a t
          ; first_witness : ('a, _) Witness.t
          ; second : 'b t
          ; second_witness : ('b, _) Witness.t
          }
          -> ('a * 'b) t

    type packed =
      | T :
          { unpacked : 'a t
          ; witness : ('a, 'cmp) Witness.t
          }
          -> packed

    type context = packed list
    type goal = unit

    include
      Quickcheckable
      with type 'a t := 'a t
       and type packed := packed
       and type context := context
       and type goal := goal

    val to_string : ('a, _) Witness.t -> 'a t -> string
  end = struct
    type 'a t =
      | Return : 'a -> 'a t
      | Map : 'a t * ('a, 'cmp) Witness.t * ('a, 'b) Function.t -> 'b t
      | Real_value : 'a Bonsai.Value.t -> 'a t
      | Var : 'a -> 'a t
      | Both :
          { first : 'a t
          ; first_witness : ('a, _) Witness.t
          ; second : 'b t
          ; second_witness : ('b, _) Witness.t
          }
          -> ('a * 'b) t

    type packed =
      | T :
          { unpacked : 'a t
          ; witness : ('a, 'cmp) Witness.t
          }
          -> packed

    type context = packed list
    type goal = unit

    (* I think having a to_string function is helpful for debugging, and I don't want to
       rewrite it every time, so I'm leaving it here. *)
    let rec to_string : type a cmp. (a, cmp) Witness.t -> a t -> string =
      fun witness t ->
      match t with
      | Return data -> [%string "Return %{(real_data_to_string witness data)}"]
      | Map (inner, inner_witness, _f) ->
        [%string "Map with inner: %{(to_string inner_witness inner)}"]
      | Real_value _value -> "Real_value"
      | Var data -> [%string "Var %{(real_data_to_string witness data)}"]
      | Both { first; first_witness; second; second_witness } ->
        [%string
          "Both (%{to_string first_witness first}, %{to_string second_witness second})"]
    ;;

    let rec quickcheck_generator
      : type w cmp. context -> goal -> (w, cmp) Witness.t -> w t Q.Generator.t
      =
      fun context _goal witness ->
        let open Q.Generator.Let_syntax in
        let tuple_weight =
          match witness with
          | Tuple _ -> Witness.num_constructors
          | _ -> 0.
        in
        let context_weight =
          match context with
          | [] -> 0.
          (* Higher weight based on the assumption that values previously
             defined are likely to be used *)
          | _ :: _ -> Float.square weight_scalar
        in
        let return_gen =
          let%map data = real_data_generator witness in
          Return data
        in
        let var_gen =
          let%map data = real_data_generator witness in
          Var data
        in
        let context_weighted_gen =
          weighted context_weight (fun () ->
            let rec search_context : type a cmp. (a, cmp) Witness.t -> a t Q.Generator.t =
              fun witness ->
                let matching_witnesses =
                  List.filter_map context ~f:(fun (T inner) ->
                    match Witness.same_witness inner.witness witness with
                    | Some T -> Some (inner.unpacked : a t)
                    | None -> None)
                in
                match matching_witnesses with
                | _ :: _ -> Q.Generator.of_list matching_witnesses
                | [] ->
                  (match witness with
                   | Tuple (first_witness, second_witness) ->
                     let%bind first = search_context first_witness in
                     let%map second = search_context second_witness in
                     Both { first; first_witness; second; second_witness }
                   | Either (first_witness, second_witness) ->
                     let%bind first = search_context first_witness in
                     let%bind second = search_context second_witness in
                     let in_tuple = Both { first; second; first_witness; second_witness } in
                     let in_witness = Witness.Tuple (first_witness, second_witness) in
                     let%map f =
                       Q.Generator.of_list
                         [ Function.Make_either `First; Function.Make_either `Second ]
                     in
                     Map (in_tuple, in_witness, f)
                   | _ ->
                     let%bind (T inner) = Q.Generator.of_list context in
                     let%map f = Function.quickcheck_generator inner.witness witness in
                     Map (inner.unpacked, inner.witness, f))
            in
            search_context witness)
        in
        let base_cases =
          List.concat
            [ [ 1. /. weight_scalar, return_gen; 1., var_gen ]; context_weighted_gen ]
        in
        match%bind Q.Generator.size with
        | 0 -> Q.Generator.weighted_union base_cases
        | size ->
          let map_gen =
            (* Though this case doesn't look recursive, Packed.quickcheck_generator
               is mutually recursive with this function *)
            let%bind (T inner) = Packed.quickcheck_generator context () in
            let%map f = Function.quickcheck_generator inner.witness witness in
            Map (inner.unpacked, inner.witness, f)
          in
          let tuple_weighted_gen =
            weighted tuple_weight (fun () ->
              match witness with
              | Tuple (first_witness, second_witness) ->
                let%bind first = quickcheck_generator context () first_witness in
                let%map second = quickcheck_generator context () second_witness in
                (Both { first; first_witness; second; second_witness } : w t)
              | _ -> assert false)
          in
          Q.Generator.with_size
            ~size:(size - 1)
            (Q.Generator.weighted_union
               (List.concat [ base_cases; [ 1., map_gen ]; tuple_weighted_gen ]))
    ;;

    let rec quickcheck_observer : type w cmp. (w, cmp) Witness.t -> w t Q.Observer.t =
      fun witness ->
        Q.Observer.create (fun t ~size ~hash ->
          match (t : w t) with
          | Return data | Var data ->
            let hash = hash_fold_int hash 0 in
            Q.Observer.observe (real_data_observer witness) data ~size ~hash
          | Map (inner, inner_witness, f) ->
            let hash = hash_fold_int hash 1 in
            let hash =
              Q.Observer.observe (quickcheck_observer inner_witness) inner ~size ~hash
            in
            Q.Observer.observe
              (Function.quickcheck_observer inner_witness witness)
              f
              ~size
              ~hash
          | Real_value _ ->
            hash_fold_int hash 2
          | Both { first; first_witness; second; second_witness } ->
            let hash = hash_fold_int hash 3 in
            let hash =
              Q.Observer.observe (quickcheck_observer first_witness) first ~size ~hash
            in
            Q.Observer.observe (quickcheck_observer second_witness) second ~size ~hash)
    ;;

    let packed_quickcheck_shrinker =
      Q.Shrinker.fixed_point (fun packed_quickcheck_shrinker ->
        Q.Shrinker.create (fun (T { unpacked; witness }) ->
          match unpacked with
          | Return data ->
            Sequence.round_robin
              [ Sequence.map
                  (Q.Shrinker.shrink (real_data_shrinker witness) data)
                  ~f:(fun data -> T { unpacked = Return data; witness })
              ]
          | Var data ->
            Sequence.round_robin
              [ Sequence.map
                  (Q.Shrinker.shrink (real_data_shrinker witness) data)
                  ~f:(fun data -> T { unpacked = Var data; witness })
              ]
          | Map (inner, inner_witness, _f) ->
            Sequence.singleton (T { unpacked = inner; witness = inner_witness })
          | Real_value _ ->
            Sequence.singleton (T { unpacked = Return (); witness = Unit })
          | Both { first; first_witness; second; second_witness } ->
            let first_packed : packed = T { unpacked = first; witness = first_witness } in
            let second_packed : packed =
              T { unpacked = second; witness = second_witness }
            in
            Sequence.round_robin
              [ Q.Shrinker.shrink packed_quickcheck_shrinker first_packed
              ; Q.Shrinker.shrink packed_quickcheck_shrinker second_packed
              ]))
    ;;
  end

  and Packed :
    (Packed_quickcheckable
     with type packed := T.packed
      and type context := T.context
      and type goal := T.goal) =
    With_witness (T)

  include T
end

let remove_n_exn my_list n =
  (* n should be the zero-index of the element to remove *)
  let first_half, second_half = List.split_n my_list (n + 1) in
  let to_ret = List.last_exn first_half in
  let first_half = List.drop_last_exn first_half in
  to_ret, first_half @ second_half
;;

module Computation = struct
  type 'a t =
    | Return : 'a Value.t -> 'a t
    | Subst : 'a t * ('a, 'cmp) Witness.t * ('a Value.t -> 'b t) -> 'b t
    | Subst2 :
        { tuple_computation : ('f, 's) Tuple2.t t
        ; first_witness : ('f, _) Witness.t
        ; second_witness : ('s, _) Witness.t
        ; f : 'f Value.t -> 's Value.t -> 'r t
        }
        -> 'r t
    | Switch :
        { either_value : ('f, 's) Either.t Value.t
        ; first_witness : ('f, _) Witness.t
        ; second_witness : ('s, _) Witness.t
        ; f_first : 'f Value.t -> 'r t
        ; f_second : 's Value.t -> 'r t
        }
        -> 'r t
    | Assoc :
        { map_value : ('k, 'v, 'cmp) Map.t Value.t
        ; key_witness : ('k, 'cmp) Witness.t
        ; value_witness : ('v, _) Witness.t
        ; result_witness : ('r, _) Witness.t
        ; f : 'k Value.t -> 'v Value.t -> 'r t
        }
        -> ('k, 'r, 'cmp) Map.t t
    | State :
        { default_model : 'a
        ; default_witness : ('a, _) Witness.t
        }
        -> ('a * ('a -> unit Bonsai.Effect.t)) t

  type packed =
    | T :
        { unpacked : 'a t
        ; witness : ('a, 'cmp) Witness.t
        }
        -> packed

  let map t witness ~f = Subst (t, witness, fun value -> Return (Map (value, witness, f)))

  let one_argument_function_generator arg_witness continue context goal_list =
    Q.Generator.create (fun ~size ~random unpacked ->
      let context = Value.T { unpacked; witness = arg_witness } :: context in
      let generator = continue context goal_list in
      Q.Generator.generate generator ~size ~random)
  ;;

  let rec quickcheck_generator
    : type a cmp.
      Value.context -> Witness.packed list -> (a, cmp) Witness.t -> a t Q.Generator.t
    =
    fun context goal_list witness ->
      let open Q.Generator.Let_syntax in
      (* Function for choosing a witness from the goal list *)
      let pick_from_goal_list goal_list =
        let index =
          let f _index (Witness.T witness) = Witness.is_state_witness witness in
          Option.map (List.findi goal_list ~f) ~f:Tuple2.get1
        in
        let%bind random_index = Q.Generator.int_inclusive 0 (List.length goal_list - 1) in
        match index with
        | None -> Q.Generator.return random_index
        | Some index ->
          Q.Generator.weighted_union
            [ Float.square weight_scalar, Q.Generator.return index
            ; 1., Q.Generator.return random_index
            ]
      in
      (* Handlers for each type of computation *)
      let subst_handler inner_witness goal_list =
        let%bind inner_unpacked = quickcheck_generator context [] inner_witness in
        let%map f =
          one_argument_computation_generator inner_witness witness context goal_list
        in
        Subst (inner_unpacked, inner_witness, f)
      in
      let subst2_handler
            (type a b cmp_a cmp_b)
            (first_witness : (a, cmp_a) Witness.t)
            (second_witness : (b, cmp_b) Witness.t)
            goal_list
        =
        let inner_goal_list : Witness.packed list =
          match first_witness, second_witness with
          | _, Effect_func _ -> []
          | _ -> [ T first_witness; T second_witness ]
        in
        let%bind tuple_computation =
          quickcheck_generator
            context
            inner_goal_list
            (Tuple (first_witness, second_witness))
        in
        let%map f =
          two_argument_computation_generator
            first_witness
            second_witness
            witness
            context
            goal_list
        in
        Subst2 { tuple_computation; first_witness; second_witness; f }
      in
      let switch_handler first_witness second_witness goal_first goal_second =
        let inner_witness = Witness.Either (first_witness, second_witness) in
        let (inner_goal_list : Witness.packed list) =
          [ T first_witness; T second_witness ]
        in
        let%bind inner_unpacked =
          quickcheck_generator context inner_goal_list inner_witness
        in
        let%map f =
          one_argument_function_generator
            inner_witness
            (fun context _goal_list ->
               let%bind value = Value.quickcheck_generator context () inner_witness in
               let%bind f_first =
                 one_argument_computation_generator first_witness witness context goal_first
               in
               let%map f_second =
                 one_argument_computation_generator
                   second_witness
                   witness
                   context
                   goal_second
               in
               Switch
                 { either_value = value; first_witness; second_witness; f_first; f_second })
            context
            goal_list
        in
        Subst (inner_unpacked, inner_witness, f)
      in
      let assoc_handler key_witness value_witness result_witness goal_list =
        let%bind map =
          Value.quickcheck_generator context () (Witness.Map (key_witness, value_witness))
        in
        let%map f =
          two_argument_computation_generator
            key_witness
            value_witness
            result_witness
            context
            goal_list
        in
        Assoc { map_value = map; key_witness; value_witness; result_witness; f }
      in
      (* Compute weights *)
      let state_weight =
        match Witness.is_state_witness witness with
        | true -> weight_scalar *. Witness.num_constructors
        | _ -> 0.
      in
      let assoc_weight =
        match witness with
        | Map (_, _) -> weight_scalar *. Witness.num_constructors
        | _ -> 0.
      in
      let goal_weight, goal2_weight =
        match goal_list with
        | [] -> 0., 0.
        | [ _x ] -> weight_scalar *. Witness.num_constructors, 0.
        | _ :: _ :: _ ->
          ( weight_scalar *. Witness.num_constructors
          , weight_scalar *. Witness.num_constructors )
      in
      let value_gen =
        let%map value = Value.quickcheck_generator context () witness in
        Return value
      in
      let state_weighted_gen =
        (* Generate State Computation *)
        weighted state_weight (fun () ->
          let handler (type w cmp) (witness : (w, cmp) Witness.t) : w t Q.Generator.t =
            match witness with
            | Tuple (first_witness, second_witness) ->
              (match
                 Witness.same_witness_exn second_witness (Effect_func first_witness)
               with
               | T ->
                 let%map data = real_data_generator first_witness in
                 State { default_model = data; default_witness = first_witness })
            | _ -> assert false
          in
          handler witness)
      in
      (* Generate computation *)
      let base_cases =
        List.concat [ [ 1. /. weight_scalar, value_gen ]; state_weighted_gen ]
      in
      match%bind Q.Generator.size with
      | 0 -> Q.Generator.weighted_union base_cases
      | size ->
        let subst_gen =
          (* Generate Subst computations *)
          let witness_handler () : _ Witness.witness_handler =
            { f = (fun inner_witness -> subst_handler inner_witness goal_list) }
          in
          Witness.make_witness (witness_handler ())
        in
        let goal_gen =
          let%bind index = pick_from_goal_list goal_list in
          let T inner_witness, goal_list = remove_n_exn goal_list index in
          subst_handler inner_witness goal_list
        in
        let subst2_gen =
          (* Generate Subst2 computations *)
          let witness_handler () : _ Witness.witness_handler =
            { f =
                (fun first_witness ->
                   Witness.make_witness
                     { f =
                         (fun second_witness ->
                            subst2_handler first_witness second_witness goal_list)
                     })
            }
          in
          Witness.make_witness (witness_handler ())
        in
        let goal2_gen =
          let%bind index = pick_from_goal_list goal_list in
          let T goal_witness, goal_list = remove_n_exn goal_list index in
          let default () =
            let%bind index = pick_from_goal_list goal_list in
            let T second_witness, goal_list = remove_n_exn goal_list index in
            subst2_handler goal_witness second_witness goal_list
          in
          match Witness.is_state_witness goal_witness with
          | true ->
            (match goal_witness with
             | Tuple (first_witness, second_witness) ->
               subst2_handler first_witness second_witness goal_list
             | _ -> assert false)
          | false -> default ()
        in
        let goal2_switch_gen =
          (* Generate Switch computations *)
          let%bind index = pick_from_goal_list goal_list in
          (* Two separate goal_lists here because only one side of the Either gets used *)
          let T first_witness, goal_first = remove_n_exn goal_list index in
          let%bind index = pick_from_goal_list goal_list in
          let T second_witness, goal_second = remove_n_exn goal_list index in
          switch_handler first_witness second_witness goal_first goal_second
        in
        let switch_gen =
          let witness_handler () : _ Witness.witness_handler =
            { f =
                (fun first_witness ->
                   Witness.make_witness
                     { f =
                         (fun second_witness ->
                            switch_handler first_witness second_witness goal_list goal_list)
                     })
            }
          in
          Witness.make_witness (witness_handler ())
        in
        let assoc_weighted_gen =
          (* Generate Assoc computations *)
          weighted assoc_weight (fun () ->
            match witness with
            | Map (key_witness, result_witness) ->
              (Witness.make_witness
                 { f =
                     (fun value_witness ->
                        assoc_handler key_witness value_witness result_witness goal_list)
                 }
               : a t Q.Generator.t)
            | _ -> assert false)
        in
        let goal_assoc_weighted_gen =
          weighted (goal_weight *. assoc_weight) (fun () ->
            match witness with
            | Map (key_witness, result_witness) ->
              let%bind index = pick_from_goal_list goal_list in
              let T value_witness, goal_list = remove_n_exn goal_list index in
              (assoc_handler key_witness value_witness result_witness goal_list
               : a t Q.Generator.t)
            | _ -> assert false)
        in
        Q.Generator.with_size
          ~size:(size - 1)
          (Q.Generator.weighted_union
             (List.concat
                [ base_cases
                ; [ 1., subst_gen
                  ; goal_weight, goal_gen
                  ; 1., subst2_gen
                  ; goal2_weight, goal2_gen
                  ; goal2_weight, goal2_switch_gen
                  ; 1., switch_gen
                  ]
                ; assoc_weighted_gen
                ; goal_assoc_weighted_gen
                ]))

  and one_argument_computation_generator
    : type a a_cmp b b_cmp.
      (a, a_cmp) Witness.t
      -> (b, b_cmp) Witness.t
      -> Value.packed list
      -> Witness.packed list
      -> (a Value.t -> b t) Q.Generator.t
    =
    fun arg_witness result_witness ->
      one_argument_function_generator arg_witness (fun context goal_list ->
        quickcheck_generator context goal_list result_witness)

  and two_argument_computation_generator
    : type a a_cmp b b_cmp c c_cmp.
      (a, a_cmp) Witness.t
      -> (b, b_cmp) Witness.t
      -> (c, c_cmp) Witness.t
      -> Value.packed list
      -> Witness.packed list
      -> (a Value.t -> b Value.t -> c t) Q.Generator.t
    =
    fun arg1_witness arg2_witness result_witness ->
      one_argument_function_generator
        arg1_witness
        (one_argument_computation_generator arg2_witness result_witness)
  ;;

  let rec quickcheck_observer : type w cmp. (w, cmp) Witness.t -> w t Q.Observer.t =
    fun witness ->
      Q.Observer.create (fun t ~size ~hash ->
        match (t : w t) with
        | Return data ->
          let hash = hash_fold_int hash 0 in
          Q.Observer.observe (Value.quickcheck_observer witness) data ~size ~hash
        | Subst (inner, inner_witness, f) ->
          let hash = hash_fold_int hash 1 in
          let hash =
            Q.Observer.observe (quickcheck_observer inner_witness) inner ~size ~hash
          in
          let fn_observer =
            Q.Observer.fn
              (Value.quickcheck_generator [] () inner_witness)
              (quickcheck_observer witness)
          in
          Q.Observer.observe fn_observer f ~size ~hash
        | Subst2 { tuple_computation; first_witness; second_witness; f } ->
          let hash = hash_fold_int hash 2 in
          let tuple_witness = Witness.Tuple (first_witness, second_witness) in
          let hash =
            Q.Observer.observe
              (quickcheck_observer tuple_witness)
              tuple_computation
              ~size
              ~hash
          in
          let f_inner_observer =
            Q.Observer.fn
              (Value.quickcheck_generator [] () second_witness)
              (quickcheck_observer witness)
          in
          let f_observer =
            Q.Observer.fn (Value.quickcheck_generator [] () first_witness) f_inner_observer
          in
          Q.Observer.observe f_observer f ~size ~hash
        | Switch { either_value; first_witness; second_witness; f_first; f_second } ->
          let hash = hash_fold_int hash 3 in
          let inner_witness = Witness.Either (first_witness, second_witness) in
          let hash =
            Q.Observer.observe
              (Value.quickcheck_observer inner_witness)
              either_value
              ~size
              ~hash
          in
          let f_first_observer =
            Q.Observer.fn
              (Value.quickcheck_generator [] () first_witness)
              (quickcheck_observer witness)
          in
          let hash = Q.Observer.observe f_first_observer f_first ~size ~hash in
          let f_second_observer =
            Q.Observer.fn
              (Value.quickcheck_generator [] () second_witness)
              (quickcheck_observer witness)
          in
          Q.Observer.observe f_second_observer f_second ~size ~hash
        | Assoc { map_value; key_witness; value_witness; result_witness; f } ->
          let hash = hash_fold_int hash 4 in
          (* Observe value *)
          let map_witness = Witness.Map (key_witness, value_witness) in
          let hash =
            Q.Observer.observe (Value.quickcheck_observer map_witness) map_value ~size ~hash
          in
          (* Observe f *)
          let f_inner_observer =
            Q.Observer.fn
              (Value.quickcheck_generator [] () value_witness)
              (quickcheck_observer result_witness)
          in
          let f_observer =
            Q.Observer.fn (Value.quickcheck_generator [] () key_witness) f_inner_observer
          in
          Q.Observer.observe f_observer f ~size ~hash
        | State { default_model; default_witness } ->
          let hash = hash_fold_int hash 5 in
          (* Observe default_model *)
          Q.Observer.observe (real_data_observer default_witness) default_model ~size ~hash)
  ;;


  let packed_quickcheck_shrinker =
    Q.Shrinker.create (fun (T { unpacked; witness }) ->
      match unpacked with
      | Return data ->
        Sequence.round_robin
          [ Sequence.map
              (Q.Shrinker.shrink
                 Value.packed_quickcheck_shrinker
                 (Value.T { unpacked = data; witness }))
              ~f:(fun (T { unpacked; witness }) ->
                T { unpacked = Return unpacked; witness })
          ]
      | Subst (inner, inner_witness, _f) ->
        Sequence.singleton (T { unpacked = inner; witness = inner_witness })
      | Subst2 { tuple_computation; first_witness; second_witness; _ } ->
        let inner_witness = Witness.Tuple (first_witness, second_witness) in
        Sequence.singleton (T { unpacked = tuple_computation; witness = inner_witness })
      | Switch { either_value; first_witness; second_witness; _ } ->
        let inner_witness = Witness.Either (first_witness, second_witness) in
        Sequence.singleton (T { unpacked = Return either_value; witness = inner_witness })
      | Assoc { map_value; key_witness; value_witness; _ } ->
        let map_witness = Witness.Map (key_witness, value_witness) in
        Sequence.singleton (T { unpacked = Return map_value; witness = map_witness })
      | State { default_model; default_witness } ->
        Sequence.singleton
          (T { unpacked = Return (Return default_model); witness = default_witness }))
  ;;
end

let rec to_real_function
  : type input output. (input, output) Function.t -> (input -> output)
  = function
    | Identity -> Fn.id
    | Const output -> Fn.const output
    | Add_const add -> ( + ) add
    | Snd -> snd
    | Map_tuple (func1, func2) ->
      fun (first, second) -> (to_real_function func1) first, (to_real_function func2) second
    | Make_either which ->
      (match which with
       | `First -> fun (first, _) -> First first
       | `Second -> fun (_, second) -> Second second)
;;

let rec to_real_value : type w. w Value.t -> w Bonsai.Value.t =
  fun unpacked ->
  match unpacked with
  | Return data -> Bonsai.Value.return data
  | Var data -> Bonsai.Var.create data |> Bonsai.Var.value
  | Map (inner, _inner_witness, f) ->
    Bonsai.Value.map (to_real_value inner) ~f:(to_real_function f)
  | Real_value value -> value
  | Both { first; second; _ } ->
    Bonsai.Value.both (to_real_value first) (to_real_value second)
;;

let of_real_value (type w) (value : w Bonsai.Value.t) : w Value.t = Real_value value

let rec to_real_computation : type w. w Computation.t -> w Bonsai.Computation.t =
  fun unpacked ->
  let open Bonsai.Let_syntax in
  match unpacked with
  | Return unpacked -> Bonsai.read (to_real_value unpacked)
  | Subst (inner, _inner_witness, f) ->
    let%sub value = to_real_computation inner in
    to_real_computation (f (of_real_value value))
  | Subst2 { tuple_computation; f; _ } ->
    let%sub first, second = to_real_computation tuple_computation in
    to_real_computation (f (of_real_value first) (of_real_value second))
  | Switch { either_value; f_first; f_second; _ } ->
    let value = to_real_value either_value in
    (match%sub value with
     | First x -> to_real_computation (f_first (of_real_value x))
     | Second x -> to_real_computation (f_second (of_real_value x)))
  | Assoc { map_value; key_witness; f; _ } ->
    let map = to_real_value map_value in
    Bonsai.assoc
      (module (val make_comparator_and_model key_witness))
      map
      ~f:(fun key value ->
        to_real_computation (f (of_real_value key) (of_real_value value)))
  | State { default_model; default_witness } ->
    Bonsai.state ~default_model (module (val make_comparator_and_model default_witness))
;;

module Top_level_computation : sig
  val quickcheck_generator : Computation.packed Q.Generator.t
  val quickcheck_observer : Computation.packed Q.Observer.t
  val quickcheck_shrinker : Computation.packed Q.Shrinker.t
end = struct

  let rec extract_funcs
    : type gen gen_cmp res res_cmp.
      (gen, gen_cmp) Witness.t -> (res, res_cmp) Witness.t -> (gen, res) Function.t
    =
    fun gen_witness res_witness ->
      match gen_witness with
      | Tuple (gen_first, gen_second) ->
        (match gen_second with
         | Effect_func _ ->
           let T = Witness.same_witness_exn gen_second res_witness in
           Snd
         | Tuple _ ->
           (match res_witness with
            | Tuple (res_first, res_second) ->
              (match gen_first with
               | Tuple _ ->
                 let f = extract_funcs gen_first res_first in
                 let g = extract_funcs gen_second res_second in
                 Map_tuple (f, g)
               | _ -> assert false)
            | _ -> assert false)
         | _ -> assert false)
      | _ -> assert false
  ;;

  let top_level_extract_funcs
        (type f a b a_cmp b_cmp)
        (gen_witness : (f * a, a_cmp) Witness.t)
        (res_witness : (f * b, b_cmp) Witness.t)
    : (f * a, f * b) Function.t
    =
    match gen_witness, res_witness with
    | Tuple (_, gen_second), Tuple (_, res_second) ->
      Map_tuple (Identity, extract_funcs gen_second res_second)
    | _ -> raise_s [%message "gen is ill-formed!"]
  ;;

  let quickcheck_generator =
    let open Q.Generator.Let_syntax in
    let%bind original_size = Q.Generator.size in
    Q.Generator.with_size
      ~size:(original_size / size_scalar)
      (Witness.make_witness
         { f =
             (fun reg_witness ->
                Q.Generator.with_size
                  ~size:(original_size / size_scalar)
                  (Witness.make_effect_witness
                     { f =
                         (fun state_witness effect_witness ->
                            let res_witness = Witness.Tuple (reg_witness, effect_witness) in
                            let gen_witness = Witness.Tuple (reg_witness, state_witness) in
                            let goal_list = Witness.to_goal_list gen_witness in
                            let%map gen_computation =
                              Q.Generator.with_size
                                ~size:original_size
                                (Computation.quickcheck_generator [] goal_list gen_witness)
                            in
                            let res_computation =
                              Computation.map
                                gen_computation
                                gen_witness
                                ~f:(top_level_extract_funcs gen_witness res_witness)
                            in
                            Computation.T
                              { unpacked = res_computation; witness = res_witness })
                     }))
         })
  ;;

  let quickcheck_observer =
    Q.Observer.create (fun (Computation.T { witness; unpacked }) ~size ~hash ->
      Q.Observer.observe (Computation.quickcheck_observer witness) unpacked ~size ~hash)
  ;;

  let quickcheck_shrinker = Computation.packed_quickcheck_shrinker
end

module Packed_real_computation = struct
  type t =
    | T :
        { unpacked : 'a Bonsai.Computation.t
        ; witness : ('a, 'cmp) Witness.t
        }
        -> t
end

let to_packed_real_computation (T { unpacked; witness } : Computation.packed) =
  Packed_real_computation.T { unpacked = to_real_computation unpacked; witness }
;;

module type Result_spec = sig
  type t

  include Bonsai_test.Result_spec.S with type t := t
end

type 'w incoming =
  | First : 'a incoming -> ('a * 'b) incoming
  | Second : 'b incoming -> ('a * 'b) incoming
  | Effect_func : 'a -> ('a -> unit Bonsai.Effect.t) incoming

let combine_actions_exn
  : type a b.
    (a incoming -> unit Bonsai.Effect.t)
    -> (b incoming -> unit Bonsai.Effect.t)
    -> ((a * b) incoming -> unit Bonsai.Effect.t)
  =
  fun effect1 effect2 either ->
  match either with
  | First first -> effect1 first
  | Second second -> effect2 second
;;

let rec witness_to_result_spec
  : type w cmp.
    (w, cmp) Witness.t
    -> (module Result_spec with type t = w and type incoming = w incoming)
  =
  fun witness ->
  match witness with
  | Unit | Int | Either _ | Map _ ->
    (module struct
      type t = w
      type nonrec incoming = w incoming

      let view = real_data_to_string witness
      let incoming _ _ = raise_s [%message [%here] "BUG: This should be impossible"]
    end)
  | Tuple (first_witness, second_witness) ->
    let module First = (val witness_to_result_spec first_witness) in
    let module Second = (val witness_to_result_spec second_witness) in
    (module struct
      type t = (First.t, Second.t) Tuple2.t
      type nonrec incoming = (First.t * Second.t) incoming

      let view = real_data_to_string witness

      let incoming t =
        combine_actions_exn
          (First.incoming (Tuple2.get1 t))
          (Second.incoming (Tuple2.get2 t))
      ;;
    end)
  | Effect_func inner_witness ->
    let module Inner = (val witness_to_result_spec inner_witness) in
    (module struct
      type t = Inner.t -> unit Bonsai.Effect.t
      type nonrec incoming = w incoming

      let view = real_data_to_string witness

      let incoming (t : w) (incoming : incoming) =
        match incoming with
        | Effect_func inner -> t inner
      ;;
    end)
;;

let rec actions_generator
  : type a cmp. (a, cmp) Witness.t -> a incoming Q.Generator.t option
  =
  fun witness ->
  let open Q.Generator.Let_syntax in
  match witness with
  | Tuple (first_witness, second_witness) ->
    (match actions_generator first_witness, actions_generator second_witness with
     | Some first_generator, Some second_generator ->
       Some
         (match%bind Q.Generator.bool with
          | true ->
            let%map first = first_generator in
            First first
          | false ->
            let%map second = second_generator in
            Second second)
     | Some first_generator, None ->
       Some
         (let%map first = first_generator in
          First first)
     | None, Some second_generator ->
       Some
         (let%map second = second_generator in
          Second second)
     | None, None -> None)
  | Effect_func inner_witness ->
    Some
      (let%map data = real_data_generator inner_witness in
       Effect_func data)
  | Unit | Int | Either _ | Map _ -> None
;;
