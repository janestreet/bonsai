open! Core
open! Import

type ('input, 'action, 'model) dynamic_apply_action =
  inject:('action -> unit Effect.t)
  -> schedule_event:(unit Effect.t -> unit)
  -> 'input
  -> 'model
  -> 'action
  -> 'model

type ('action, 'model) static_apply_action =
  inject:('action -> unit Effect.t)
  -> schedule_event:(unit Effect.t -> unit)
  -> 'model
  -> 'action
  -> 'model

type ('model, 'dynamic_action, 'static_action, 'result) t =
  | Return : 'result Value.t -> (unit, Nothing.t, Nothing.t, 'result) t
  | Leaf1 :
      { input : 'input Value.t
      ; dynamic_apply_action : ('input, 'dynamic_action, 'model) dynamic_apply_action
      ; name : string
      ; kind : string
      }
      -> ( 'model
         , 'dynamic_action
         , Nothing.t
         , 'model * ('dynamic_action -> unit Effect.t) )
           t
  | Leaf0 :
      { (* [Leaf0] does not contain a [static_apply_action] for the same reason
           that we distinguish between dynamic and static actions at all. Because
           they do not depend on input, the static actions are not computed
           incrementally, and therefore can be collected into the top-level
           computation during the construction of the computation. *)
        compute : inject:('static_action -> unit Effect.t) -> 'model -> 'result
      ; name : string
      ; kind : string
      }
      -> ('model, Nothing.t, 'static_action, 'result) t
  | Leaf_incr :
      { input : 'input Value.t
      ; dynamic_apply_action :
          'input Incr.t
          -> inject:('dynamic_action -> unit Effect.t)
          -> (schedule_event:(unit Effect.t -> unit)
              -> 'model
              -> 'dynamic_action
              -> 'model)
               Incr.t
      ; compute :
          Incr.Clock.t
          -> 'input Incr.t
          -> 'model Incr.t
          -> inject:('dynamic_action -> unit Effect.t)
          -> 'result Incr.t
      ; name : string
      }
      -> ('model, 'dynamic_action, Nothing.t, 'result) t
  | Model_cutoff :
      { t : ('m, 'dynamic_action, 'static_action, 'r) t
      ; model : 'm Meta.Model.t
      }
      -> ('m, 'dynamic_action, 'static_action, 'r) t
  | Subst :
      { from : ('m1, 'dynamic_action1, 'static_action1, 'r1) t
      ; via : 'r1 Type_equal.Id.t
      ; into : ('m2, 'dynamic_action2, 'static_action2, 'r2) t
      ; here : Source_code_position.t option
      }
      -> ( 'm1 * 'm2
         , ('dynamic_action1, 'dynamic_action2) Either.t
         , ('static_action1, 'static_action2) Either.t
         , 'r2 )
           t
  | Subst_stateless_from :
      { from : (unit, Nothing.t, Nothing.t, 'r1) t
      ; via : 'r1 Type_equal.Id.t
      ; into : ('m, 'dynamic_action, 'static_action, 'r2) t
      ; here : Source_code_position.t option
      }
      -> ('m, 'dynamic_action, 'static_action, 'r2) t
  | Subst_stateless_into :
      { from : ('m, 'dynamic_action, 'static_action, 'r1) t
      ; via : 'r1 Type_equal.Id.t
      ; into : (unit, Nothing.t, Nothing.t, 'r2) t
      ; here : Source_code_position.t option
      }
      -> ('m, 'dynamic_action, 'static_action, 'r2) t
  | Store :
      { id : 'x Type_equal.Id.t
      ; value : 'x Value.t
      ; inner : ('m, 'dynamic_action, 'static_action, 'r) t
      }
      -> ('m, 'dynamic_action, 'static_action, 'r) t
  | Fetch :
      { id : 'a Type_equal.Id.t
      ; default : 'r
      ; for_some : 'a -> 'r
      }
      -> (unit, Nothing.t, Nothing.t, 'r) t
  | Assoc :
      { map : ('k, 'v, 'cmp) Map.t Value.t
      ; key_compare : 'k -> 'k -> int
      ; key_id : 'k Type_equal.Id.t
      ; data_id : 'v Type_equal.Id.t
      ; by : ('model, 'dynamic_action, 'static_action, 'result) t
      ; model_info : 'model Meta.Model.t
      ; action_info : 'dynamic_action Meta.Action.t
      ; result_by_k : ('result_by_k, ('k, 'result, 'cmp) Map.t) Type_equal.t
      ; model_by_k : ('model_by_k, ('k, 'model, 'cmp) Map.t) Type_equal.t
      }
      -> ('model_by_k, 'k * 'dynamic_action, 'k * 'static_action, 'result_by_k) t
  | Assoc_simpl :
      { map : ('k, 'v, 'cmp) Map.t Value.t
      ; key_id : 'k Type_equal.Id.t
      ; data_id : 'v Type_equal.Id.t
      ; by : Path.t -> 'k -> 'v -> 'result
      ; result_by_k : ('result_by_k, ('k, 'result, 'cmp) Map.t) Type_equal.t
      }
      -> (unit, Nothing.t, Nothing.t, 'result_by_k) t
  | Switch :
      { match_ : int Value.t
      ; arms : 'r packed Int.Map.t
      }
      -> ( (int, Int.comparator_witness) Hidden.Multi_model.t
         , int Hidden.Action.t
         , int Hidden.Action.t
         , 'r )
           t
  (* Lazy wraps the model in an option because otherwise you could make
     infinitely sized models (by eagerly expanding a recursive model) which
     would stack-overflow during eval.  [None] really means "unchanged from the
     default", and is used to halt the the eager expansion. *)
  | Lazy :
      'r packed Lazy.t
      -> (Hidden.Model.t option, unit Hidden.Action.t, unit Hidden.Action.t, 'r) t
  | Wrap :
      { model_id : 'outer_model Type_equal.Id.t
      ; inject_id : ('outer_dynamic_action -> unit Effect.t) Type_equal.Id.t
      ; inner : ('inner_model, 'inner_dynamic_action, 'inner_static_action, 'result) t
      ; dynamic_apply_action :
          ('result, 'outer_dynamic_action, 'outer_model) dynamic_apply_action
      }
      -> ( 'outer_model * 'inner_model
         , ('outer_dynamic_action, 'inner_dynamic_action) Either.t
         , 'inner_static_action
         , 'result )
           t
  | With_model_resetter :
      ('m, 'dynamic_action, 'static_action, 'r) t
      -> ('m, 'dynamic_action, (unit, 'static_action) Either.t, 'r * unit Effect.t) t
  | Path : (unit, Nothing.t, Nothing.t, Path.t) t
  | Lifecycle : Lifecycle.t option Value.t -> (unit, Nothing.t, Nothing.t, unit) t

and 'r packed =
  | T :
      { t : ('model, 'dynamic_action, 'static_action, 'r) t
      ; dynamic_action : 'dynamic_action Meta.Action.t
      ; static_action : 'static_action Meta.Action.t
      ; model : 'model Meta.Model.t
      ; apply_static : ('static_action, 'model) static_apply_action
      }
      -> 'r packed

let rec sexp_of_t
  : type model dynamic_action static_action result.
    (model, dynamic_action, static_action, result) t -> Sexp.t
  = function
    | Return value -> [%sexp Return (value : Value.t)]
    | Leaf1 { name; _ } -> [%sexp Leaf (name : string)]
    | Leaf0 { name; _ } -> [%sexp Leaf0 (name : string)]
    | Leaf_incr { name; _ } -> [%sexp Leaf_incr (name : string)]
    | Model_cutoff { t; _ } -> [%sexp Model_cutoff (t : t)]
    | Subst { from; via; into; here = _ } ->
      [%sexp Subst { from : t; via : _ Type_equal.Id.t; into : t; here = None }]
    | Subst_stateless_from { from; via; into; here = _ } ->
      [%sexp
        Subst_stateless_from { from : t; via : _ Type_equal.Id.t; into : t; here = None }]
    | Subst_stateless_into { from; via; into; here = _ } ->
      [%sexp
        Subst_stateless_into { from : t; via : _ Type_equal.Id.t; into : t; here = None }]
    | Store { id; value; inner } ->
      [%sexp Store { id : _ Type_equal.Id.t; value : Value.t; inner : t }]
    | Fetch { id; _ } -> [%sexp Fetch (id : _ Type_equal.Id.t)]
    | Assoc { map; by; _ } -> [%sexp Assoc { map : Value.t; by : t }]
    | Assoc_simpl { map; _ } -> [%sexp Assoc_simpl { map : Value.t }]
    | Switch { match_; arms; _ } ->
      let arms = arms |> Map.to_alist |> List.map ~f:[%sexp_of: int * packed] in
      [%sexp Switch { match_ : Value.t; arms : Sexp.t list }]
    | Lazy _ -> [%sexp Lazy]
    | With_model_resetter t -> [%sexp With_model_resetter (t : t)]
    | Wrap { inner; _ } -> [%sexp Wrap (inner : t)]
    | Path -> [%sexp Path]
    | Lifecycle _ -> [%sexp Lifecycle]

and sexp_of_packed : type r. r packed -> Sexp.t = fun (T { t; _ }) -> sexp_of_t t
